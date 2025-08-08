;;; macher.el --- LLM implementation toolset -*- lexical-binding: t -*-

;; Author: Kevin Montag
;; Version: 0.3.0
;; Package-Requires: ((emacs "30.1") (gptel "0.9.8.5"))
;; Keywords: convenience, gptel, llm
;; URL: https://github.com/kmontag/macher

;;; Commentary:
;; macher provides a toolset for project-aware LLM file editing based on gptel.
;;
;; Key features:
;; - Use gptel presets to add context and read/edit tools for the current project
;; - View proposed changes as diff-mode-friendly patches
;; - Send implementation requests using `macher-implement'
;; - Request revisions to patches with `macher-revise'
;;
;; Conceptually, when making a request, macher provides the LLM with a "workspace" containing files
;; from the current project (or just the current file if not in a project), and tools to read/edit
;; files in the workspace. Edits are not persisted to disk, but rather stored in the request's
;; associated `macher-context' object, where they can later be used to generate patches.
;;
;; The main interactive entrypoints are `macher-implement', `macher-revise', and (read-only)
;; `macher-discuss', all of which are wrappers around the more general `macher-action'. Actions run
;; in a dedicated per-workspace request buffer.
;;
;; Under the hood, actions use (ephemeral) gptel presets to set up the request context/tools, and to
;; display changes in a diff buffer when the request is complete. You can also install these presets
;; globally using `macher-install', and then use them in any gptel workflow - for example, you could
;; send a gptel request like:
;;
;;   @macher set up eslint with sensible defaults

;;; Code:

(require 'gptel)
(require 'gptel-context)
(require 'cl-lib)

;;; Customization

(defgroup macher nil
  "Project-aware LLM implementation toolset."
  :group 'convenience
  :prefix "macher-")

(defcustom macher-actions-alist
  '((implement :prompt "To implement: " :preset macher :transform macher--implement-prompt)
    (revise :prompt "Revision instructions: " :preset macher :transform macher--revise-prompt)
    (discuss :prompt "Discuss: " :preset macher-ro :transform identity))
  "Alist defining actions and their configuration.

Each entry is of the form (ACTION . PLIST) where ACTION is a symbol
representing the action name and PLIST contains the following keys:

- :transform - Function to transform the user input into the final
  prompt (required)

- :preset - gptel preset to use for the request (optional, defaults to
  'macher)

- :prompt - String prompt to show when asking for user input (optional)

The :transform function receives the user input string and should return
the final prompt string to send to the LLM.

The :preset can be a symbol (key from `macher--presets-alist') or a raw
gptel preset plist."
  :type
  '(alist
    :key-type symbol
    :value-type (plist :key-type keyword :value-type (choice string symbol function)))
  :group 'macher)

(defcustom macher-action-buffer-ui 'default
  "Specifies a base UI configuration to use for action buffers.

The action buffer for a workspace is the shared buffer where gptel
requests are made when using `macher-action' (or its wrappers like
`macher-implement').

This value controls the buffer-local setup that happens just after an
action buffer is created, and just before the
'macher-action-buffer-setup-hook' is fired. It effectively sets up a
predefined \"pre\" function for the 'macher-action-buffer-setup-hook'.

The choices are:

- 'basic - sets up buffer-local hooks in
  'macher-before-action-functions' and 'macher-after-action-functions'
  to display the action buffer and insert a nicely-formatted prompt.

- 'default - like 'basic, but also enables 'gptel-mode' and the global
  'gptel-default-mode' (e.g. markdown), and sets up auto-scrolling and
  line wrapping.

- 'org - like 'default, but instead of the 'gptel-default-mode', enables
  'org-mode' and requires 'gptel-org'. If you're already using
  'org-mode' (with 'gptel-org') as your 'gptel-default-mode', this will
  behave exactly the same as 'default.

- nil - don't perform any action buffer setup. The action buffer will
  just be a 'fundamental-mode' buffer which receives the responses from
  gptel requests made through `macher-action'. Use this if you want full
  control over the action buffer UI - you can customize the
  'macher-action-buffer-setup-hook' to perform your own initialization
  from scratch.

The exact behavior of the predefined UI configurations is subject to
change."
  :type
  '(choice
    (const :tag "Basic UI with hooks only" basic)
    (const :tag "Default UI with gptel-mode and formatting" default)
    (const :tag "Org-mode UI with gptel-org" org)
    (const :tag "No setup - full manual control" nil))
  :group 'macher)

(defcustom macher-action-buffer-setup-hook nil
  "Hook run when creating new macher action buffers.

Functions in this hook are called with no arguments in the newly created
action buffer. The buffer will already have its workspace set up (i.e.
`macher-workspace' can be used to get the associated workspace) and
'default-directory' set to the workspace root.

The base UI configuration is controlled by `macher-action-buffer-ui'
and is applied before this hook runs. This hook can be used for
additional customization on top of the base configuration.

If you want full control over the action buffer UI, set
`macher-action-buffer-ui' to nil and use this hook to perform all
setup from scratch. In that case, you'll probably also need to
customize (globally or buffer-locally) the
'macher-before-action-functions' to make sure the buffer actually gets
displayed."
  :type 'hook
  :group 'macher)

(defcustom macher-action-dispatch-hook nil
  "Hook run when invoking a macher action.

This hook is run within the source buffer where the action is being
invoked, e.g. a file or directory buffer. It runs before the prompt text
is captured/transformed - so you could use this, for example, to adjust
the selected region, or add the current file to the gptel context.

This runs prior to the 'macher-before-action-functions', and potentially
prior to the creation of the action buffer."
  :type 'hook
  :group 'macher)

(defcustom macher-before-action-functions nil
  "Abnormal hook run before sending the request for a macher action.

Each function is called in the workspace's action buffer, with a
`macher-action-execution' object containing details about the request
that's about to be sent.

Note this hook runs _after_ the 'macher-action-dispatch-hook', after the
prompt has been captured and transformed but before the actual request
is sent.

Functions can modify the PROMPT or CONTEXT fields of the execution
object to change the prompt that will be sent to the LLM and/or add
context to the request's 'gptel-fsm'.

Note that the default UI configurations (controlled by
`macher-action-buffer-ui') will modify this hook buffer-locally in the
action buffer. This is what handles inserting the prompt and displaying
the action buffer when you make a request. If you set
`macher-action-buffer-ui' to nil, you'll probably also want to add
something to this hook which at least displays the action buffer."
  :type 'hook
  :group 'macher)

(defcustom macher-after-action-functions nil
  "Abnormal hook run after a macher action's request completes.

Each function is called with three arguments: ERROR (nil on success, or an
error description on failure), EXECUTION (the `macher-action-execution'
object for the completed action), and FSM (the `gptel-fsm' object for the
request).

The hook will be run in the action buffer where the request is being
executed, so it can be customized buffer-locally. In the edge case where
the action buffer is killed during request execution, the hook will not
be run - though gptel will probably throw other errors first in this
case.

This hook runs after the callback provided to `macher-action' (if any)."
  :type 'hook
  :group 'macher)

(defcustom macher-context-string-function #'macher--context-string
  "Function for generating workspace information strings during macher requests.

This function will be called from the buffer where the request is being
sent, and receives the same arguments as the standard
'gptel-context-string-function'. It can be synchronous (one argument,
just the context list) or asynchronous (two args, a callback and the
context list) - see that function's docstring for details.

The function should return (or invoke the async callback with) a
workspace information string that will be added to the request content,
in the same place the standard gptel context as indicated by
'gptel-use-context'. It can also return nil, in which case the request
will not be modified.

The default function adds information about the current workspace (e.g.
the project being edited) including file listings and context
indicators."
  :type '(function :tag "Workspace string function")
  :group 'macher)

(defcustom macher-context-string-max-files 50
  "Maximum number of files to include in the workspace context string.

When generating the workspace context information, only up to this many
files will be listed.

Files from the gptel context are always included in the workspace
context (even if their count exceeds this limit). Additional workspace
files will be included up to the remaining limit.

Files are listed in the same order as returned by the workspace's files
function - for built-in workspace types, this means files are listed in
alphabetical order.

Set to nil to disable the limit entirely.

Note: This value is used within the default `macher--context-string'
function. If you customize 'macher-context-string-function' to use a
different function, this value will have no effect."
  :type '(choice (natnum :tag "Maximum number of files") (const :tag "No limit" nil))
  :group 'macher)

(defcustom macher-process-request-function #'macher--process-request
  "Function to handle changes during the macher request lifecycle.

This function is called with two arguments:

- CONTEXT: the 'macher-context' for the current request.

- FSM: the 'gptel-fsm' (state machine) for the current request.

This function is only called for requests that actually create a
'macher-context', i.e. requests with macher tools of some sort. The
'macher-notools preset will not cause the hook to be called.

The value of this variable will be stored at request-time and used
throughout a particular request. This enables custom handling on a
per-request basis.

By default, this function prepares and processes the patch buffer if any
edits were made during the request.")

(defcustom macher-patch-prepare-functions
  '(macher--patch-prepare-diff macher--patch-prepare-metadata)
  "Abnormal hook called to prepare the patch content when processing a request.

Functions in this hook will be called inside an initially-empty
temporary buffer (not the actual patch buffer), and should modify the
buffer to contain the patch content. They will be called with three
arguments:

- CONTEXT: the 'macher-context' associated with the request being
  processed.

- FSM: the 'gptel-fsm' struct for the request.

- CALLBACK: a callback that must be called after the preparation step is
  finished. This function can be called synchronously or asynchronously,
  but it must be called in all cases.

By default, the functions in this hook will populate the buffer with a
git-patch-style diff, and add metadata including the request prompt.

If you want to add to this hook but also preserve the default behavior,
make sure you call `add-hook' (or similar) _after_ requiring 'macher'.
If you call it beforehand, the default hook value will simply be
ignored. If you're using 'use-package', this means you should avoid the
:hook keyword, and instead use `add-hook' in the :config section.

This hook is run within the default 'macher-process-request-function'.
If you modify that function for a particular request, this hook won't be
run.")

(defcustom macher-patch-buffer-ui 'diff
  "Specifies a base UI configuration to use for patch buffers.

The patch buffer for a workspace is the shared buffer where diff content
is displayed when using macher actions that generate patches.

This value controls the buffer-local setup that happens just after a
patch buffer is created, and just before the
'macher-patch-buffer-setup-hook' is fired. It effectively sets up a
predefined \"pre\" function for the 'macher-patch-buffer-setup-hook'.

The choices are:

- 'diff - sets up 'diff-mode', makes the buffer read-only, and adds
  buffer-local hooks to display the patch buffer when it's ready.

- nil - don't perform any patch buffer setup. The patch buffer will
  just be a 'fundamental-mode' buffer which receives the patch content.
  Use this if you want full control over the patch buffer UI - you can
  customize the 'macher-patch-buffer-setup-hook' and
  'macher-patch-ready-hook' to perform your own initialization from
  scratch."
  :type
  '(choice
    (const :tag "Default diff UI with diff-mode" diff)
    (const :tag "No setup - full manual control" nil))
  :group 'macher)

(defcustom macher-patch-buffer-setup-hook nil
  "Hook run when creating new macher patch buffers.

Functions in this hook are called with no arguments in the newly created
patch buffer. The buffer will already have its workspace set up (i.e.
`macher-workspace' can be used to get the associated workspace) and
'default-directory' set to the workspace root.

The base UI configuration is controlled by `macher-patch-buffer-ui'
and is applied before this hook runs. This hook can be used for
additional customization on top of the base configuration.

If you want full control over the patch buffer UI, set
`macher-patch-buffer-ui' to nil and use this hook to perform all
setup from scratch."
  :type 'hook
  :group 'macher)

(defcustom macher-patch-ready-hook nil
  "Hook called after the patch buffer has been prepared.

Functions in this hook will be called inside the patch buffer, with its
contents initialized to the result of the
'macher-patch-prepare-functions' (i.e. the contents of the temporary
preparation buffer after running those functions). They will be called
with no arguments.

The patch buffer's 'default-directory' will be set to the workspace
root, and `macher-workspace' can be called to retrieve the associated
workspace.

The base UI configuration is controlled by `macher-patch-buffer-ui'
and will modify this hook buffer-locally in the patch buffer. This is
what handles setting up 'diff-mode' and displaying the patch buffer when
it's ready. If you set `macher-patch-buffer-ui' to nil, you'll probably
also want to add something to this hook which at least displays the
patch buffer.

This hook is run within the default 'macher-process-request-function'.
If you modify that function for a particular request, this hook won't be
run."
  :type 'hook
  :group 'macher)

(defcustom macher-read-tools-function #'macher--read-tools
  "Function for generating read-only tools for macher requests.

This function is called with two arguments:

- CONTEXT: the macher-context object for the current request.

- MAKE-TOOL-FUNCTION: function that takes the same arguments as
  `gptel-make-tool', but must be used instead of `gptel-make-tool' to
  create tools.

The function should return a list of tools that provide read-only access
to the workspace. Only tools created via the MAKE-TOOL-FUNCTION may be
returned - these will be scoped to the current request, and will not be
added to the global gptel registry."
  :type 'function
  :group 'macher)

(defcustom macher-edit-tools-function #'macher--edit-tools
  "Function for generating file editing tools for macher requests.

This function is called with two arguments:

- CONTEXT: the 'macher-context' object for the current request.

- MAKE-TOOL-FUNCTION: function that takes the same arguments as
  `gptel-make-tool', but must be used instead of `gptel-make-tool' to
  create tools.

The function should return a list of tools that provide read/write access
to the workspace. Only tools created via the MAKE-TOOL-FUNCTION may be
returned - these will be scoped to the current request, and will not be
added to the global gptel registry."
  :type 'function
  :group 'macher)

(defcustom macher-match-max-columns 300
  "Maximum length in characters for individual lines in search matches.

Any matches or context lines from the search tool which exceed this
length will be replaced with an omission message, similar to ripgrep's
'--max-columns' option."
  :type 'natnum
  :group 'macher)

(defcustom macher-workspace-functions '(macher--project-workspace macher--file-workspace)
  "Functions to determine the workspace for the current buffer.

Each function in this list is called with no arguments in the current
buffer until one returns a non-nil workspace cons cell of the form
\(TYPE . ID).

Functions should return nil if they cannot determine a workspace for
the current buffer, allowing other functions in the list to try.

Built-in workspace functions:
- `macher--project-workspace': Uses project.el to find project workspaces
- `macher--file-workspace': Falls back to single-file workspaces

The output and patch buffers will be shared among files in the same
workspace (i.e. same type and ID).

To add custom workspace detection, add functions to this list.

Custom functions should return a cons cell (TYPE . ID) where:
- TYPE is a symbol from the 'macher-workspace-types-alist' (e.g.
  'project, 'file, or a custom workspace type)
- ID is the workspace identifier (typically a root directory or file path,
  but for custom workspace types, it can be any string as long as you
  can use it to resolve a real root path)"
  :type 'hook
  :group 'macher)

(defcustom macher-workspace-types-alist
  '((project
     .
     (:get-root
      macher--project-root
      :get-name macher--project-name
      :get-files macher--project-files))
    (file . (:get-root file-name-directory :get-name file-name-nondirectory :get-files list)))
  "Alist mapping workspace types to their defining functions.

Each entry is of the form (TYPE . PLIST) where TYPE is a symbol
representing the workspace type and PLIST contains the following keys:

- :get-root - Function to get the workspace root, i.e. the base
  directory for unified diffs and the \"root\" for relative filenames
  provided to tools.

- :get-name - Function to get a descriptive name for the workspace.

- :get-files - Function to get files in the workspace. Returned paths
  can be absolute, or relative to the workspace root.

All functions receive the workspace ID as a string argument, i.e. the
cdr of a workspace cons cell whose type matches the associated alist key.

To add a new workspace type, add an entry to this alist and update
'macher-workspace-functions' to detect it."
  :type '(alist :key-type symbol :value-type (plist :key-type keyword :value-type function))
  :group 'macher)

;;; Constants

(defconst macher--max-read-length (* 1024 1024)
  "Max number of bytes to return from the read tool.")

;;; Variables

(defvar-local macher--workspace nil
  "Workspace information for the current macher session.
When nil (the default), uses `macher-workspace-functions' to
determine the workspace scope.

When set to a cons cell of the form (TYPE . ID) where:
- TYPE is a symbol identifying the workspace type
- ID is the workspace identifier (typically a root directory or file path)

The workspace represents the conceptual space where the LLM edits files.
From the LLM's perspective, when a request is made, the entire
workspace is loaded into in-memory editing buffers.")

;; Prevent the stored workspace from being cleared when changing major modes. See
;; `kill-all-local-variables'.
(put 'macher--workspace 'permanent-local t)

;;; Context Structure

(cl-defstruct (macher-context (:constructor macher--make-context))
  "Structure holding context information for an implementation request.

- CONTENTS is an alist of (filename . (orig-content . new-content))
  pairs, where orig-content and new-content are strings.

- WORKSPACE is the workspace information (same format as 'macher--workspace').

- PROMPT is the raw prompt text sent to the LLM (not including any
  conversation history/prior messages that might also have been
  included).

  Note the prompt is captured via a prompt transform that gets appended
  to 'gptel-prompt-transform-functions' when applying macher presets. If
  the prompt is modified by other transforms that get added later in the
  chain (e.g. via other presets), these modifications won't be picked
  up.

- PROCESS-REQUEST-FUNCTION is the function to process the request
  throughout its lifecycle. This will be set to the current value of
  'macher-process-request-function' at the time a request is made.

- DATA is an arbitrary user-defined data object that you can manipulate
  in custom tools.

- DIRTY-P is a boolean flag indicating whether any changes have been
  made to the workspace during the request. This defaults to nil and is
  set to t by the edit tools when they make changes. Custom tools that
  make changes should set this flag explicitly."
  (contents nil)
  (workspace)
  (prompt)
  (process-request-function)
  (data nil)
  (dirty-p nil))

(cl-defstruct (macher-action-execution (:constructor macher--make-action-execution))
  "Structure holding execution information for a macher action.

- ACTION is the action symbol (e.g. 'implement, 'revise, 'discuss).

- INPUT is the raw user input string.

- PROMPT is the transformed prompt string that will be sent to the LLM.
  Functions in 'macher-before-action-functions' can modify this field to
  change the prompt before sending.

- BUFFER is the action buffer where the request will be sent.

- SOURCE is the source buffer where the action was initiated.

- CONTEXT will be passed as the :context key when calling
  `gptel-request'. This is a user-defined object that can be read from
  the 'gptel-fsm' (state machine) associated with the request. Functions
  in 'macher-before-action-functions' can modify this field."
  (action)
  (input)
  (prompt)
  (buffer)
  (source)
  (context nil))

;;; Internal Functions

;; Built-in workspace detection functions
(defun macher--project-workspace ()
  "Detect project workspace for the current buffer.
Returns (project . ROOT) if the buffer is in a project, nil otherwise."
  (require 'project)
  (when-let ((project (project-current nil default-directory)))
    (cons 'project (project-root project))))

(defun macher--file-workspace ()
  "Detect file workspace for the current buffer.
Returns (file . FILENAME) if the buffer is visiting a file, nil otherwise."
  (when-let ((filename (buffer-file-name)))
    (cons 'file filename)))

;; Built-in workspace type functions
(defun macher--project-root (project-id)
  "Get the project root for PROJECT-ID, validating it's a real project root."
  (require 'project)
  ;; Verify that project-id is actually a valid project root directory.
  (unless (and (stringp project-id)
               (file-name-absolute-p project-id)
               (file-directory-p project-id)
               (project-current nil project-id))
    (error "Project ID '%s' is not a valid project root directory" project-id))
  project-id)

(defun macher--project-name (project-id)
  "Get the project name for PROJECT-ID using project.el."
  (require 'project)
  (if-let ((project (project-current nil project-id)))
    (project-name project)
    ;; Get the last directory name from the root (trailing slash removed).
    (file-name-nondirectory (directory-file-name project-id))))

(defun macher--project-files (project-id)
  "Get list of files in PROJECT-ID using project.el.
Returns a list of relative file paths."
  ;; project.el allows a nil ID (in which case it just looks at the current buffer), but this
  ;; would give inconsistent results and introduce a security risk that files from unexpected
  ;; projects could be exposed to the workspace.
  (unless (stringp project-id)
    (error "Project ID must be a string, got %s (%s)" (type-of project-id) project-id))
  (require 'project)
  (when-let* ((proj (project-current nil project-id))
              (files (project-files proj)))
    ;; Return files relative to project root.
    (mapcar (lambda (f) (file-relative-name f project-id)) files)))

(defun macher-workspace (&optional buffer)
  "Get the workspace information for BUFFER.
Returns a cons cell representing the workspace scope.

The return value is always a cons cell of the form (TYPE . ID) where:
- TYPE is a symbol identifying the workspace type
- ID is the workspace identifier (typically a root directory or file path)

If the buffer-local variable `macher--workspace` is nil, this function
runs the functions in `macher-workspace-functions' until one returns a
non-nil workspace cons cell. If no function returns a workspace,
it returns nil."
  (with-current-buffer (or buffer (current-buffer))
    (or macher--workspace (cl-some #'funcall macher-workspace-functions))))

(defun macher--workspace-root (workspace)
  "Get the workspace root for WORKSPACE.

WORKSPACE is a cons cell (TYPE . ID) where TYPE is a workspace type.
Returns the root directory path for the workspace.

Uses the appropriate root function as configured in the
'macher-workspace-types-alist', and validates that the result is an
absolute path to a real directory."
  (let* ((workspace-type (car workspace))
         (workspace-id (cdr workspace))
         (type-config (alist-get workspace-type macher-workspace-types-alist))
         (root-fn (plist-get type-config :get-root))
         (root
          (if root-fn
              (or (funcall root-fn workspace-id)
                  (error
                   "Root function for workspace type %s failed to return a root" workspace-type))
            (error "No root function configured for workspace type %s" workspace-type))))
    ;; Verify that the root is a real directory.
    (unless (and root (file-name-absolute-p root))
      (error "Workspace root '%s' is not an absolute path" root))
    (unless (and root (file-directory-p root))
      (error "Workspace root '%s' is not a valid directory" root))
    root))

(defun macher--workspace-name (workspace)
  "Get a descriptive name for WORKSPACE.
WORKSPACE is a cons cell (TYPE . ID) where TYPE is a workspace type.
Returns a string containing the workspace name."
  (let* ((workspace-type (car workspace))
         (workspace-id (cdr workspace))
         (type-config (alist-get workspace-type macher-workspace-types-alist))
         (name-fn (plist-get type-config :get-name)))
    (if name-fn
        (funcall name-fn workspace-id)
      (error "No name function configured for workspace type %s" workspace-type))))

(defun macher--workspace-files (workspace)
  "Get list of files in WORKSPACE.
WORKSPACE is a cons cell (TYPE . ID) where TYPE is a workspace type.
Returns a list of absolute file paths."
  (let* ((workspace-type (car workspace))
         (workspace-id (cdr workspace))
         (type-config (alist-get workspace-type macher-workspace-types-alist))
         (files-fn (plist-get type-config :get-files))
         (root-path (macher--workspace-root workspace)))
    (when files-fn
      (let ((files (funcall files-fn workspace-id)))
        ;; Ensure all paths are absolute.
        (when files
          (mapcar
           (lambda (f)
             (if (file-name-absolute-p f)
                 f
               (expand-file-name f root-path)))
           files))))))

(defun macher--context-string (contexts)
  "Generate workspace information string for the current workspace.

CONTEXTS is the list of contexts as passed to the standard gptel context
string function, potentially nil.

Returns a workspace information string to be added to the request."
  (let* ((workspace (macher-workspace))
         (workspace-type (car workspace))
         (workspace-id (cdr workspace))
         (workspace-name (macher--workspace-name workspace))
         (workspace-files (macher--workspace-files workspace))
         ;; Extract normalized file paths from contexts for comparison.
         (context-file-paths
          (let (file-paths)
            (dolist (context contexts)
              (let ((source (car context)))
                (when (stringp source)
                  (push (file-truename source) file-paths))))
            file-paths)))

    ;; Generate workspace description with context indicators.
    (when workspace-files
      ;; Separate files into two categories based on whether they're in the context.
      ;; Files from gptel context are always listed first and all included.
      (let ((files-in-context '())
            (files-available-for-editing '()))

        ;; Single pass: collect files in original order, separating context vs non-context files.
        (dolist (file-path workspace-files)
          (let* ((rel-path (file-relative-name file-path (macher--workspace-root workspace)))
                 (normalized-file-path
                  (condition-case nil
                      (file-truename file-path)
                    (error
                     file-path)))
                 (in-context-p (member normalized-file-path context-file-paths)))
            (if in-context-p
                (push rel-path files-in-context)
              (push rel-path files-available-for-editing))))

        ;; Reverse to maintain original order.
        (setq files-in-context (reverse files-in-context))
        (setq files-available-for-editing (reverse files-available-for-editing))

        ;; Trim the available files list if there's a limit.
        (when macher-context-string-max-files
          (let ((remaining-limit
                 (max 0 (- macher-context-string-max-files (length files-in-context)))))
            (when (> (length files-available-for-editing) remaining-limit)
              (setq files-available-for-editing
                    (seq-take files-available-for-editing remaining-limit)))))

        (with-temp-buffer
          (insert "\n")
          (insert "=======================================================\n")
          (insert "WORKSPACE CONTEXT:\n")
          (insert "=======================================================\n")
          (insert "\n")
          (insert
           "!!! IMPORTANT INFORMATION, READ AND UNDERSTAND THIS SECTION BEFORE USING TOOLS !!!\n")
          (insert "\n")
          (insert "The workspace is an in-memory editing environment containing files from ")
          (insert (format "the user's current project, which is named `%s`.\n" workspace-name))
          (insert "\n")
          (insert "ONLY YOU, the LLM, have access to the files in the workspace. You DO NOT ")
          (insert "need to check for external changes.\n")
          (insert "\n")
          (insert "Edit freely using workspace tools. ")
          (insert "No permissions required - this is a safe virtual editing space.\n")

          (when files-in-context
            (insert "\n")
            (insert "!!! CRITICAL RULE: DO NOT RE-READ OR SEARCH THESE FILES !!!\n")
            (insert
             "Some files' contents have already been provided to you above in the REQUEST CONTEXT.\n")
            (insert "You MUST NOT re-read or search the contents of these files.\n")
            (insert "This is wasteful and unnecessary!\n")

            (insert "\n")
            (insert
             "== Files already provided above (DO NOT use read or search tools on these): ==\n")
            (dolist (rel-path files-in-context)
              (insert (format "    %s\n" rel-path))))

          (when files-available-for-editing
            (insert "\n")
            (insert
             (format "%s available for editing:\n"
                     (if files-in-context
                         "Other files"
                       "Files")))
            (dolist (rel-path files-available-for-editing)
              (insert (format "    %s\n" rel-path)))
            ;; Add a note if files were truncated due to the limit.
            (when macher-context-string-max-files
              (let* ((total-files (length workspace-files))
                     (listed-files
                      (+ (length files-in-context) (length files-available-for-editing)))
                     (truncated-files (- total-files listed-files)))
                (when (> truncated-files 0)
                  (insert
                   (format "\n    ... and %d more files\n"
                           truncated-files
                           macher-context-string-max-files)))))
            (insert "\n"))

          (insert "\n")
          (insert "=======================================================\n")
          (insert "END WORKSPACE CONTEXT:\n")
          (insert "=======================================================\n")
          (insert "\n")

          (buffer-string))))))

(defun macher--workspace-hash (workspace &optional length)
  "Generate a unique hash for WORKSPACE.
LENGTH specifies the number of characters in the hash (default 16).
This can be used, for example, to ensure unique buffer names per
workspace, without needing to put the full path in the buffer name."
  (let* ((workspace-type (car workspace))
         (workspace-id (cdr workspace))
         (hash-input (secure-hash 'sha256 (concat (format "%s" workspace-type) workspace-id)))
         (chars "abcdefghijklmnopqrstuvwxyz0123456789")
         (hash-length (or length 16))
         (result ""))
    ;; Take the starting characters and map them to our character set.
    (dotimes (i hash-length result)
      (let* ((hex-char (aref hash-input i))
             (idx
              (mod
               (if (>= hex-char ?a)
                   (- hex-char ?a -10)
                 (- hex-char ?0))
               (length chars))))
        (setq result (concat result (substring chars idx (1+ idx))))))))

(defun macher--action-buffer-setup-basic ()
  "Set up basic common behavior for action buffers.

This performs setup related to the actual text that gets put into the
buffer when making requests - tool calls are included, and buffer-local
hooks are added to format/insert prompts sent by the user.

This also handles displaying the action buffer when actions are
performed."
  ;; Include tool results.
  (setq-local gptel-include-tool-results t)
  ;; Set up the buffer-local hooks to insert prompts and headings.
  (add-hook 'macher-before-action-functions #'macher--before-action nil t)
  (add-hook 'macher-after-action-functions #'macher--after-action nil t))

(defun macher--action-buffer-setup-ui ()
  "Set up a slightly more opinionated action buffer UI.

This setup is shared among the 'default and 'org UI configurations. The
function enables 'gptel-mode' and sets up auto-scrolling and line
wrapping."
  ;; Enable gptel-mode for a nice header and LLM interaction feedback.
  (gptel-mode 1)
  ;; Wrap lines.
  (visual-line-mode 1)
  ;; Auto-scroll when at end of buffer.
  (setq-local window-point-insertion-type t))


(defun macher--action-buffer-setup ()
  "Apply the base UI configuration based on `macher-action-buffer-ui'.

This function is called automatically when creating action buffers,
before running the `macher-action-buffer-setup-hook'."
  (pcase macher-action-buffer-ui
    ('basic
     ;; Basic UI: just hooks.
     (macher--action-buffer-setup-basic))
    ('default
     ;; Use the global gptel default mode (e.g., markdown-mode)
     (funcall (or gptel-default-mode #'text-mode))
     (macher--action-buffer-setup-ui)
     (macher--action-buffer-setup-basic))
    ('org
     ;; Use org as the major mode. Tool output blocks will be auto-folded.
     (require 'gptel-org)
     (org-mode)
     (macher--action-buffer-setup-ui)
     (macher--action-buffer-setup-basic))
    ;; nil: no automatic setup
    ((pred null))
    (_
     (user-error
      (format "Unrecognized action buffer UI configuration: %s" macher-action-buffer-ui)))))

(defun macher--before-action (execution)
  "Default function for before-action processing.

This is added buffer-locally (in the action buffer) to the
'macher-before-action-functions' when using any of the predefined
'macher-action-buffer-ui' configurations. If you set
`macher-action-buffer-ui' to nil, this function will never be called.

This function takes an EXECUTION object (a 'macher-action-execution'
struct) describing the action and the request.

The function inserts the prompt at the end of the current buffer (action
buffer), including some headings to better distinguish between user and
LLM content.

It adapts the prompt formatting based on the current major mode."
  (let* ((prompt (macher-action-execution-prompt execution))
         (input (macher-action-execution-input execution))
         (action (macher-action-execution-action execution))
         (action-str (symbol-name action))
         ;; Use some custom formatting if we're in org mode. Otherwise, format for markdown.
         (is-org-mode (derived-mode-p 'org-mode))
         (header-prefix
          (if is-org-mode
              ""
            (format "`%s` " action-str)))
         (header-postfix
          (if is-org-mode
              ;; Add the action as a tag at the end of the headline.
              (format " :%s:" action-str)
            ""))
         ;; Extract the first non-whitespace line from the prompt and truncate to fill-column.
         (truncated-input
          (let* ((lines (split-string input "\n" t "[[:space:]]*"))
                 (first-line (or (car lines) ""))
                 ;; Calculate available space: total fill-column minus prefix, action, and spacing.
                 (prefix (or (alist-get major-mode gptel-prompt-prefix-alist) ""))
                 (used-length (+ (length prefix) (length header-prefix) (length header-postfix)))
                 (available-length (max 10 (- (or fill-column 70) used-length))))
            (truncate-string-to-width first-line available-length nil nil "...")))
         ;; Make the separation between prompt/response clearer using a foldable block in org-mode,
         ;; otherwise a markdown-style code block.
         (full-prompt-str
          (if is-org-mode
              (concat
               (format ":PROMPT:\n" truncated-input) (org-escape-code-in-string prompt) "\n:END:\n")
            (concat "```\n" prompt "\n```\n"))))

    (goto-char (point-max))

    ;; If the buffer is empty, insert the prefix first.
    (when (and (= (point-min) (point-max)) (alist-get major-mode gptel-prompt-prefix-alist))
      (insert (alist-get major-mode gptel-prompt-prefix-alist)))

    ;; Header string.
    (insert (format "%s%s%s\n" header-prefix truncated-input header-postfix))

    ;; Add the demarcated prompt text.
    (insert full-prompt-str)

    ;; In org mode, fold the prompt immediately, like with tool-use output.
    (when is-org-mode
      (ignore-errors
        (save-excursion
          (search-backward ":PROMPT:")
          (when (looking-at "^:PROMPT:")
            (org-cycle)))))

    ;; Enter a selected-buffer context, so we preserve the currently-selected buffer after exiting.
    ;; (since `display-buffer' may change the selected buffer).
    (with-current-buffer (current-buffer)
      (display-buffer (current-buffer)))))

(defun macher--after-action (_err _execution fsm)
  "Default function for after-action handling.

This is added buffer-locally (in the action buffer) to the
'macher-after-action-functions' when using the 'basic, 'default, or
'org UI configurations. If you set `macher-action-buffer-ui' to nil,
this function will never be called.

The function just inserts the prefix for the next prompt if it wasn't
already inserted by gptel - that is, if the request was aborted or ended
in an error. The marker info from the gptel FSM is used for placement."
  (let* ((info (gptel-fsm-info fsm))
         ;; This should always be present.
         (start-marker (plist-get info :position))
         ;; This might be nil if the request ended with an error/abort before any response was
         ;; received.
         (tracking-marker (plist-get info :tracking-marker))
         (current-marker (or tracking-marker start-marker)))
    ;; The current marker should always be non-nil, but don't error out if it's missing/both markers
    ;; were nil for some reason.
    (when current-marker
      (save-excursion
        (goto-char current-marker)

        (when-let ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
          ;; Check if we're at the end of the gptel prompt prefix, i.e. the one that was just
          ;; inserted due to the completion of the request. If not (this is the case if the request
          ;; ended with an error or abort), insert it so we're prepared for the next prompt.
          (unless (and (>= (point) (length prefix))
                       (string=
                        prefix
                        (buffer-substring-no-properties (- (point) (length prefix)) (point))))
            (insert "\n" prefix)))))))

(defun macher--patch-buffer-setup-diff ()
  "Set up 'diff-mode' and related settings for patch buffers.

This performs setup related to displaying patch content as diffs,
including enabling 'diff-mode' and setting up buffer-local hooks for
displaying the patch buffer when it's ready."
  (diff-mode)
  ;; Diffs are generally easier to interact with (e.g. press RET to jump to a change) in read-only
  ;; mode, so this seems like a reasonable default.
  (read-only-mode 1)
  ;; Diffs are visually confusing if continuation lines are displayed.
  (setq-local truncate-lines t)
  ;; Set up the buffer-local hook to display the patch buffer when ready.
  (add-hook 'macher-patch-ready-hook #'macher--patch-ready nil t))

(defun macher--patch-buffer-setup ()
  "Apply the base UI configuration based on `macher-patch-buffer-ui'.

This function is called automatically when creating patch buffers,
before running the `macher-patch-buffer-setup-hook'."
  (pcase macher-patch-buffer-ui
    ('diff
     ;; Diff UI: diff-mode + display behavior.
     (macher--patch-buffer-setup-diff))
    ;; nil: no automatic setup.
    ((pred null))
    (_
     (user-error
      (format "Unrecognized patch buffer UI configuration: %s" macher-patch-buffer-ui)))))

(defun macher--patch-ready ()
  "Set up the patch buffer with appropriate modes and settings.

This is added buffer-locally (in the patch buffer) to the
'macher-patch-ready-hook' when using the 'diff UI configuration.
If you set `macher-patch-buffer-ui' to nil, this function will
never be called.

The function syncs 'diff-mode' settings based on the current content and
displays the patch buffer."
  ;; Re-detect patch type (i.e. 'git) now that the buffer has been populated.
  (when (derived-mode-p 'diff-mode)
    (diff-setup-buffer-type))

  ;; Display the buffer. Do this in a no-op buffer context to avoid changing the actual current
  ;; buffer.
  (with-current-buffer (current-buffer)
    (display-buffer (current-buffer))))

(defun macher--process-request (context fsm)
  "Process the macher CONTEXT throughout the request lifecycle.

CONTEXT is the 'macher-context' for the current request.

FSM is the 'gptel-fsm' (state machine) for the current request.

This is the default implementation of 'macher-process-request-function'."
  (when context
    ;; Check if any changes were made during the request using the dirty-p flag.
    (when (macher-context-dirty-p context)
      (macher--build-patch context fsm))))

(defun macher--build-patch (context fsm)
  "Run the 'macher-patch-prepare-functions' and the 'macher-patch-ready-hook'.

CONTEXT and FSM are the same as passed to the
'macher-process-request-function'."
  ;; Prepare patch content in a temporary buffer.
  (with-temp-buffer
    (let* ((temp-buffer (current-buffer))
           (remaining-functions (copy-sequence macher-patch-prepare-functions))
           ;; Forward declaration so that the function can be called recursively.
           (recursive-process-next-function)
           (process-next-function
            (lambda ()
              "Process the next function in the preparation chain."
              (if remaining-functions
                  (let ((func (pop remaining-functions)))
                    (funcall func context fsm recursive-process-next-function))
                ;; All functions processed, finalize the patch.
                (let ((patch-content
                       (with-current-buffer temp-buffer
                         (buffer-substring-no-properties (point-min) (point-max)))))
                  ;; Populate the actual patch buffer.
                  (with-current-buffer (macher-patch-buffer (macher-context-workspace context) t)
                    ;; Save read-only status to restore later.
                    (let ((was-read-only buffer-read-only))
                      ;; Temporarily disable read-only mode to update the buffer.
                      (when was-read-only
                        (read-only-mode -1))
                      (erase-buffer)
                      (insert patch-content)
                      (when was-read-only
                        (read-only-mode 1)))

                    ;; Move to the beginning of the buffer.
                    (goto-char (point-min))

                    ;; Run the patch-ready hooks.
                    (run-hooks 'macher-patch-ready-hook)))))))
      (setq recursive-process-next-function process-next-function)
      ;; Start the preparation chain.
      (funcall process-next-function))))

(defun macher--get-buffer (buffer-type &optional workspace create)
  "Get a macher buffer with BUFFER-TYPE associated with WORKSPACE.

BUFFER-TYPE can be any string or nil. The return value for a given
BUFFER-TYPE is shared across all files in the same workspace.

If CREATE is non-nil, create the buffer if it doesn't already exist.
Otherwise, return nil if the buffer doesn't already exist.

When WORKSPACE is nil, use the current buffer's workspace as determined
by `macher-workspace'.

Returns a cons cell (BUFFER . CREATED-P) where BUFFER is the target
buffer and CREATED-P is t if the buffer was newly created, nil
otherwise."
  (let* ((workspace (or workspace (macher-workspace)))
         (workspace-type (car workspace))
         (workspace-id (cdr workspace))
         (workspace-name (macher--workspace-name workspace))
         (buffer-type-segment
          (if buffer-type
              (format "-%s" buffer-type)
            ""))
         (buffer-name
          (format "*macher%s:%s@%s<%s>*"
                  buffer-type-segment workspace-type workspace-name
                  ;; Use a shorter hash in buffer names.
                  (macher--workspace-hash workspace 4)))
         target-buffer
         created-p)

    (unless (consp workspace)
      (error "Workspace must be a cons cell"))

    (setq target-buffer (and buffer-name (get-buffer buffer-name)))
    (when (and (not target-buffer) create buffer-name)
      (setq target-buffer (get-buffer-create buffer-name))
      (setq created-p t)
      (with-current-buffer target-buffer
        ;; Track workspace for this buffer.
        (setq-local macher--workspace workspace)
        ;; Set the context directory for operations like diff application or 'project.el'
        ;; lookups.
        (let ((base-dir (macher--workspace-root workspace)))
          (setq-local default-directory base-dir))))
    (when target-buffer
      (cons target-buffer created-p))))

(defun macher-action-buffer (&optional workspace create)
  "Get the macher action buffer associated with WORKSPACE.

The action buffer is used for requests sent via `macher-action', and is
shared across all files in the same workspace.

If CREATE is non-nil, create the buffer if it doesn't already exist.
Otherwise, return nil if the buffer doesn't already exist.

When WORKSPACE is nil, use the current buffer's workspace as determined
by `macher-workspace'."
  (let ((result (macher--get-buffer nil workspace create)))
    (when result
      (let ((target-buffer (car result))
            (created-p (cdr result)))
        (when created-p
          (with-current-buffer target-buffer
            ;; Apply base UI configuration first.
            (macher--action-buffer-setup)
            ;; Then run user hooks.
            (run-hooks 'macher-action-buffer-setup-hook)))
        target-buffer))))

(defun macher-patch-buffer (&optional workspace create)
  "Get the macher patch buffer associated with WORKSPACE.

The patch buffer is used for displaying proposed changes as diffs and is
shared across all files in the same workspace.

If CREATE is non-nil, create the buffer if it doesn't already exist.
Otherwise, return nil if the buffer doesn't already exist.

When WORKSPACE is nil, use the current buffer's workspace as determined
by `macher-workspace'."
  (let ((result (macher--get-buffer "patch" workspace create)))
    (when result
      (let ((target-buffer (car result))
            (created-p (cdr result)))
        (when created-p
          (with-current-buffer target-buffer
            ;; Apply base UI configuration first.
            (macher--patch-buffer-setup)
            ;; Then run user hooks.
            (run-hooks 'macher-patch-buffer-setup-hook)))
        target-buffer))))

(defun macher--normalize-path (path)
  "Normalize PATH to an absolute path suitable for context lookups.
Ensures paths are consistently handled throughout the codebase."
  (if (file-name-absolute-p path)
      (expand-file-name path)
    (error "PATH must be an absolute file path")))

(defun macher--load-gptel-context-files (contexts macher-context)
  "Prepare editing buffers for any workspace files that are in the gptel context.

CONTEXTS is the list of gptel contexts as passed to the context string
function. MACHER-CONTEXT is a `macher-context' struct. This function
creates implementation buffers for any files that are within both the
gptel context and the current workspace. This ensures that those
workspace file contents are frozen at request time, even if the files
are modified on disk before editing operations occur - that is, it
ensures that the LLM can trust that file contents provided to it in the
context won't change before it can edit them."
  (when-let* ((workspace (macher-context-workspace macher-context))
              (workspace-type (car workspace))
              (workspace-id (cdr workspace)))
    (dolist (context-entry contexts)
      (let ((source (car context-entry)))
        ;; Only process file paths (strings), not buffers.
        (when (stringp source)
          (let* ((full-path (expand-file-name source))
                 (workspace-root (macher--workspace-root workspace))
                 (in-workspace-p
                  (if (eq workspace-type 'file)
                      ;; Single-file workspace: only include if it's the exact file.
                      (string= full-path workspace-id)
                    ;; General workspace: include if file is within the workspace root.
                    (and workspace-root (string-prefix-p workspace-root full-path)))))
            ;; If this context file is in the workspace, load implementation contents for it.
            (when in-workspace-p
              (macher-context--contents-for-file full-path macher-context))))))))

(defun macher--resolve-workspace-path (workspace rel-path)
  "Get the full path for REL-PATH within the WORKSPACE.

The path will be resolved relative to the workspace root. '.' and '..'
are handled as standard relative path segments.

Raises an error if the LLM doesn't have permission to interact with the
path. Specifically, an error is thrown if:

- The path resolves somewhere outside the workspace root, and isn't
  present in the workspace's files list.

- The path resolves to a file which exists on the filesystem, but isn't
  present in the workspace's files list (directories beneath the
  workspace root are allowed).

- The path contains an existing symbolic link as a non-final component
  below the workspace root - that is, we don't follow allow following
  directory symlinks within the workspace.

- The path contains an existing file as a non-final component below the
  workspace root - that is, we don't allow resolving paths that would
  require treating an existing file as a directory.

Absolute paths are allowed, though the rules above apply to the
filename, and in practice it would be unexpected for the LLM to pass
them in.

Note also that paths outside the workspace root are allowed _if_ they
appear in the workspace's files list. This won't be the case for the
built-in workspace types, but might be relevant for custom workspace
types."
  (let* (
         ;; We don't really want to deal with the `file-truename', as this would resolve symlinks
         ;; and might mess up the path structure when dealing with relative paths like
         ;; "../sibling-dir-in-custom-workspace/". However, we will be using `expand-file-name' to
         ;; resolve the path, which expands some path components like "~" into real directories -
         ;; so, in order to more easily check whether the resolved path is inside the workspace
         ;; root, we expand these components immediately.
         (workspace-root (expand-file-name (macher--workspace-root workspace)))
         ;; Resolve the path relative to workspace root, handling . and ..
         (full-path (expand-file-name rel-path workspace-root)))

    ;; Check for files or symlinks in non-final path components below the workspace root
    ;; Only perform this check for paths that are within the workspace directory - other files
    (when (string-prefix-p workspace-root full-path)
      (let* ((relative-path (file-relative-name full-path workspace-root))
             ;; Use file-name-split for OS-independent path component splitting.
             (path-components (file-name-split relative-path))
             (current-path workspace-root))
        ;; Check each component except the last one for files or symlinks (only below workspace
        ;; root).
        (when (> (length path-components) 1)
          (dolist (component (butlast path-components))
            (unless (string-empty-p component) ; Skip empty components
              (setq current-path (expand-file-name component current-path))
              (when (file-exists-p current-path)
                (cond
                 ((file-symlink-p current-path)
                  (error "Path '%s' contains a symbolic link in a non-final component" rel-path))
                 ((not (file-directory-p current-path))
                  (error "Path '%s' contains a file in a non-final component" rel-path)))))))))

    ;; Validate access permissions.
    (let* ((raw-workspace-files (macher--workspace-files workspace))
           ;; Process workspace files by expanding them relative to workspace root
           (workspace-files
            (when raw-workspace-files
              (mapcar
               (lambda (file-path)
                 (if (file-name-absolute-p file-path)
                     file-path
                   (expand-file-name file-path workspace-root)))
               raw-workspace-files)))
           (is-outside-workspace
            (not
             (or
              ;; Check whether the path is equal to the workspace root, i.e. '.'.
              (string= (directory-file-name workspace-root) full-path)
              ;; Check whether the path begins with the workspace root + the path separator.
              (string-prefix-p (file-name-as-directory workspace-root) full-path))))
           (file-exists (file-exists-p full-path)))

      (when (and is-outside-workspace (not (member full-path workspace-files)))
        (error "Path '%s' resolves outside the workspace" rel-path))

      (when (and file-exists
                 ;; Allow directories, but only within the workspace.
                 (or is-outside-workspace (not (file-directory-p full-path)))
                 (not (member full-path workspace-files)))
        (error "File '%s' is not in the workspace files list" rel-path)))

    ;; Return the resolved path
    full-path))

(defun macher--format-size (bytes)
  "Format BYTES as a human-readable size string."
  (let ((units '("B" "KB" "MB" "GB" "TB"))
        (size (float bytes))
        (unit-index 0))
    (while (and (>= size 1024) (< unit-index (1- (length units))))
      (setq size (/ size 1024.0))
      (setq unit-index (1+ unit-index)))
    (if (= unit-index 0)
        (format "%d %s" (truncate size) (nth unit-index units))
      (format "%.1f %s" size (nth unit-index units)))))

(defun macher--read-string (content &optional offset limit show-line-numbers)
  "Read string CONTENT with optional line-number OFFSET and LIMIT.

- CONTENT is the full string content to read from.

- OFFSET, if provided, specifies the line number to start reading
  from (1-based). For negative values, starts at that many lines before
  the end of the file.

- LIMIT, if provided, specifies the number of lines to read from the
  start position. For negative values, the actual limit is computed as
  (total_lines + limit). For example, with a 100-line file: limit=10
  reads 10 lines, limit=-10 reads 90 lines (100 + (-10)).

- SHOW-LINE-NUMBERS, if non-nil, formats output in cat -n style with
  line numbers. Lines are formatted as \"[spaces for alignment][line
  number][tab][line content]\".

If neither OFFSET nor LIMIT is provided, returns the full content.

If only OFFSET is provided, returns content from that line to the end.

If only LIMIT is provided, returns the first LIMIT lines.

If both are provided, returns LIMIT lines starting from OFFSET.

Returns the processed content as a string."
  (if (and (not offset) (not limit) (not show-line-numbers))
      ;; Return full content if no parameters provided.
      content
    (let* ((lines (split-string content "\n"))
           (num-lines (length lines))
           (start-idx
            (if offset
                (cond
                 ;; Negative offset: start at that many lines before the end.
                 ((< offset 0)
                  (max 0 (+ num-lines offset)))
                 ;; Zero or positive offset: 1-based indexing (treat 0 as 1).
                 (t
                  (max 0 (min (1- (max 1 offset)) num-lines))))
              0))
           (actual-limit
            (when limit
              (cond
               ;; Negative limit: equivalent to total lines - (negative limit value).
               ((< limit 0)
                (max 0 (+ num-lines limit)))
               ;; Zero or positive limit: use as-is.
               (t
                limit))))
           (end-idx
            (if actual-limit
                (min num-lines (+ start-idx actual-limit))
              num-lines))
           (selected-lines (seq-subseq lines start-idx end-idx)))
      (cond
       (show-line-numbers
        ;; Format with line numbers (cat -n style).
        (let* ((actual-start-line (1+ start-idx))
               ;; `cat -n` includes the trailing newline if present, but doesn't number it. We
               ;; handle this as a special case. If:
               ;;
               ;; - the last line is empty
               ;; - we're looking at the actual last line of the content (i.e. not a blank line in
               ;;   the middle)
               ;; - there's more than one line (since `cat -n` on a completely blank file does show
               ;;   line number 1
               ;;
               ;; then exclude the last line from the list of lines to be numbered, and add it back
               ;; at the end.
               (should-exclude-final-empty
                (and (= end-idx num-lines) ; processing to end.
                     (> (length selected-lines) 1) ; we have more than one line.
                     (string-empty-p (car (last selected-lines))))) ; last line is empty.
               (lines-to-number
                (if should-exclude-final-empty
                    (butlast selected-lines)
                  selected-lines))
               (max-line-num (+ actual-start-line (length lines-to-number) -1))
               (line-num-width
                (if (> (length lines-to-number) 0)
                    (length (number-to-string max-line-num))
                  1))
               (formatted-lines
                (cl-loop
                 for
                 line
                 in
                 lines-to-number
                 for
                 line-num
                 from
                 actual-start-line
                 collect
                 (format (concat "%" (number-to-string line-num-width) "d\t%s") line-num line)))
               (result (string-join formatted-lines "\n")))
          ;; Add the final trailing newline if we excluded the final empty line
          (if should-exclude-final-empty
              (concat result "\n")
            result)))
       ;; Regular format without line numbers.
       (t
        (string-join selected-lines "\n"))))))

(defun macher--with-workspace-file (context path callback &optional set-dirty-p)
  "Helper function to execute CALLBACK with workspace file content.

CONTEXT is a `macher-context' struct containing workspace information.

PATH is the path to the file, relative to the workspace root.

CALLBACK is called with arguments (full-path new-content) where:
- full-path is the absolute path to the file
- new-content is the current content string of the file

If SET-DIRTY-P is non-nil, sets the dirty-p flag on the context."
  (let* ((workspace (macher-context-workspace context))
         (resolve-workspace-path (apply-partially #'macher--resolve-workspace-path workspace))
         (get-or-create-file-contents
          (lambda (file-path)
            "Get or create implementation contents for FILE-PATH, scoped to the current request.
Returns a cons cell (orig-content . new-content) of strings for the file.
Also updates the context's :contents alist."
            (let ((full-path (funcall resolve-workspace-path file-path)))
              (macher-context--contents-for-file full-path context))))
         (full-path (funcall resolve-workspace-path path))
         ;; Get implementation contents for this file.
         (contents (funcall get-or-create-file-contents path))
         (new-content (cdr contents)))
    ;; Check if the file exists for editing.
    (if (not new-content)
        (error (format "File '%s' not found in workspace" path))
      ;; Set the dirty flag to indicate changes are being made if requested.
      (when set-dirty-p
        (setf (macher-context-dirty-p context) t))
      ;; Call the callback with the file content.
      (funcall callback full-path new-content))))

;; The workspace tools are somewhat inspired by the reference filesystem MCP. See
;; https://github.com/modelcontextprotocol/servers/blob/main/src/filesystem/README.md.

(defun macher--tool-read-file (context path &optional offset limit show-line-numbers)
  "Read the contents of a file specified by PATH within the workspace.

CONTEXT is a `macher-context' struct containing workspace information.

PATH is the path to the file, relative to the workspace root.

OFFSET, if provided, specifies the line number to start reading
from (1-based).

LIMIT, if provided, specifies the number of lines to read.

SHOW-LINE-NUMBERS, if non-nil, formats output in cat -n style with line numbers.

Returns the file contents as a string, with optional
offset/limit/show-line-numbers processing. For symlinks, returns the target
path instead of following the link. Signals an error if the file is not
found in the workspace."
  (let* ((workspace (macher-context-workspace context))
         (resolve-workspace-path (apply-partially #'macher--resolve-workspace-path workspace))
         (full-path (funcall resolve-workspace-path path)))

    ;; Check if this is a symlink first (only for existing files).
    (if (file-symlink-p full-path)
        ;; For symlinks, return the target path instead of following the link.
        (let ((target (file-symlink-p full-path)))
          (format "Symlink target: %s" target))

      ;; Normal/non-symlink handling.
      (macher--with-workspace-file
       context
       path
       (lambda (_full-path new-content)
         ;; Some LLMs (for example qwen3-coder at time of writing) seem to have trouble invoking
         ;; tools with integer inputs - they'll always pass e.g. '1.0' instead of '1'. Therefore we
         ;; need to support float inputs, which in general we handle by rounding to the nearest
         ;; integer.
         (let* ((parsed-offset
                 (when offset
                   (round offset)))
                (parsed-limit
                 (when limit
                   (round limit)))
                (processed-content
                 (macher--read-string new-content parsed-offset parsed-limit show-line-numbers)))
           ;; Check if the processed content exceeds the maximum read length.
           (when (> (length processed-content) macher--max-read-length)
             (error
              "File content too large: %d bytes exceeds maximum read length of %d bytes"
              (length processed-content)
              macher--max-read-length))
           processed-content))
       nil))))

(defun macher--tool-list-directory (context path &optional recursive sizes)
  "List directory contents at PATH within the workspace.

CONTEXT is a `macher-context' struct containing workspace information.

PATH is the path to the directory, relative to the workspace root.

RECURSIVE, if non-nil, recursively lists subdirectories.

SIZES, if non-nil, includes file sizes in the output.

Returns a formatted string listing the directory contents. Files are
prefixed with 'file:' and directories with 'dir:' to be clear to LLMs.
Signals an error if the directory is not found in the workspace."
  (let* ((workspace (macher-context-workspace context))
         (resolve-workspace-path (apply-partially #'macher--resolve-workspace-path workspace))
         (full-path (funcall resolve-workspace-path path))
         (workspace-root (macher--workspace-root workspace))
         (results '())
         (context-contents (macher-context-contents context)))

    ;; Check if this path exists as a file in our context (would indicate it's not a directory).
    (when-let ((existing-entry (assoc (macher--normalize-path full-path) context-contents)))
      (let ((contents (cdr existing-entry)))
        ;; Has new-content, so it's a file.
        (when (cdr contents)
          (error (format "Path '%s' is a file, not a directory" path)))))

    ;; Check if the directory exists on disk OR has files in the context.
    ;; A directory is considered to exist if:
    ;; 1. It exists on disk as a directory, OR
    ;; 2. There are files in the context that have this directory as a parent
    (let ((directory-has-context-files-p
           (cl-some
            (lambda (entry)
              (let* ((file-path (car entry))
                     (contents (cdr entry))
                     (new-content (cdr contents)))
                ;; Only consider files that have new content (not deleted files)
                (when new-content
                  (let ((file-dir (file-name-directory file-path)))
                    ;; Check if this file's directory is or is a subdirectory of the requested path
                    (string-prefix-p
                     (file-name-as-directory full-path) (file-name-as-directory file-dir))))))
            context-contents)))
      (unless (or (file-directory-p full-path) directory-has-context-files-p)
        (error (format "Directory '%s' not found in workspace" path))))

    ;; Helper function to check if a file is deleted in the context.
    (cl-labels
        ((file-deleted-in-context-p
          (file-path) "Check if FILE-PATH is marked as deleted in the context."
          (when-let ((entry (assoc (macher--normalize-path file-path) context-contents)))
            (let ((contents (cdr entry)))
              (and
               ;; Has original content...
               (car contents)
               ;; ...but no new content.
               (not (cdr contents))))))

         (get-file-content-size
          (file-path) "Get the size of FILE-PATH, considering context modifications."
          (if-let ((entry (assoc (macher--normalize-path file-path) context-contents)))
            (let* ((contents (cdr entry))
                   (new-content (cdr contents)))
              (if new-content
                  (length new-content)
                ;; File is deleted in context, so size is 0.
                0))
            ;; Not in context, get size from disk.
            (if (file-exists-p file-path)
                (file-attribute-size (file-attributes file-path))
              0)))

         (collect-new-context-files
          (current-path)
          "Collect files that exist only in context (newly created) under CURRENT-PATH."
          (let ((new-files '()))
            (dolist (entry context-contents)
              (let* ((file-path (car entry))
                     (contents (cdr entry))
                     (orig-content (car contents))
                     (new-content (cdr contents)))
                ;; This is a newly created file if it has new-content but no orig-content.
                (when (and new-content (not orig-content))
                  ;; Check if this file is a direct child of the current directory.
                  (let ((file-dir (file-name-directory file-path))
                        (normalized-current-path (file-name-as-directory current-path)))
                    (when (string= (file-name-as-directory file-dir) normalized-current-path)
                      (push (file-name-nondirectory file-path) new-files))))))
            new-files))

         (collect-new-context-directories
          (current-path)
          "Collect directory names that are implied by context files under CURRENT-PATH."
          (let ((new-dirs '()))
            (dolist (entry context-contents)
              (let* ((file-path (car entry))
                     (contents (cdr entry))
                     (new-content (cdr contents)))
                ;; Only consider files that have new content (not deleted files).
                (when new-content
                  ;; Check if this file creates any intermediate directories under current-path.
                  (let* ((relative-path
                          (file-relative-name file-path (file-name-as-directory current-path))))
                    ;; Only process if the file is actually under the current path (not ..).
                    (when (not (string-prefix-p ".." relative-path))
                      ;; Split the path to find the first directory component
                      (when (string-match "/" relative-path)
                        (let ((first-component
                               (substring relative-path 0 (match-beginning 0))))
                          (unless (or (string-empty-p first-component)
                                      (member first-component new-dirs))
                            (push first-component new-dirs)))))))))
            new-dirs))

         (collect-entries
          (current-path current-rel-path depth)
          ;; Get all entries from the directory (if it exists on disk) plus any context files.
          (let* ((workspace-files (macher--workspace-files workspace))
                 ;; Only include disk entries that are in the workspace files list.
                 (disk-entries
                  (if (file-directory-p current-path)
                      (let ((all-disk-files (directory-files current-path)))
                        (cl-remove-if-not
                         (lambda (entry)
                           ;; Exclude . and .. meta-directories to prevent infinite recursion.
                           (unless (or (string= entry ".") (string= entry ".."))
                             (let ((entry-full-path (expand-file-name entry current-path)))
                               ;; Include if it's in the workspace files list OR it's a directory.
                               (or (cl-some
                                    (lambda (workspace-file)
                                      ;; Handle both absolute and relative paths in workspace-files.
                                      (let ((normalized-workspace-file
                                             (if (file-name-absolute-p workspace-file)
                                                 workspace-file
                                               (expand-file-name workspace-file workspace-root))))
                                        (string= entry-full-path normalized-workspace-file)))
                                    workspace-files)
                                   (file-directory-p entry-full-path)))))
                         all-disk-files))
                    '()))
                 ;; Add any newly created files from the context.
                 (context-new-files (collect-new-context-files current-path))
                 ;; Add any directories implied by context files.
                 (context-new-directories (collect-new-context-directories current-path))
                 ;; Combine and deduplicate.
                 (all-entries
                  (cl-remove-duplicates
                   (append disk-entries context-new-files context-new-directories)
                   :test #'string=)))
            ;; Process entries if we have any entries to process.
            (when (> (length all-entries) 0)
              (dolist (entry all-entries)
                (let* ((entry-full-path (expand-file-name entry current-path))
                       (entry-rel-path
                        (if (string-empty-p current-rel-path)
                            entry
                          (concat current-rel-path "/" entry)))
                       (entry-deleted-p (file-deleted-in-context-p entry-full-path))
                       (entry-exists-on-disk-p (file-exists-p entry-full-path))
                       (entry-is-symlink-p
                        (and (not entry-deleted-p) (file-symlink-p entry-full-path)))
                       (entry-exists-in-context-p
                        (when-let ((entry
                                    (assoc
                                     (macher--normalize-path entry-full-path) context-contents)))
                          (let ((contents (cdr entry)))
                            ;; Has new content.
                            (cdr contents))))
                       (entry-exists-p
                        (or entry-exists-on-disk-p
                            entry-exists-in-context-p entry-is-symlink-p
                            ;; Also exists if it's a directory implied by context files.
                            (member entry context-new-directories)))
                       (entry-is-dir-p
                        (and entry-exists-p
                             (not entry-deleted-p) (not entry-is-symlink-p)
                             ;; A path is a directory if it exists on disk as a directory OR if it's
                             ;; in our context-new-directories list.
                             (or (file-directory-p entry-full-path)
                                 (member entry context-new-directories))))
                       (size-info "")
                       (indent (make-string (* depth 2) ?\s)))

                  ;; Skip deleted files.
                  (unless entry-deleted-p
                    ;; Get size information if requested (not for directories or symlinks).
                    (when (and sizes (not entry-is-dir-p) (not entry-is-symlink-p))
                      (let ((file-size (get-file-content-size entry-full-path)))
                        (setq size-info (format " (%s)" (macher--format-size file-size)))))

                    ;; Add symlink target info if it's a symlink.
                    (when entry-is-symlink-p
                      (let ((target (file-symlink-p entry-full-path)))
                        (setq size-info (format " -> %s" target))))

                    ;; Add entry to results.
                    (push (format "%s%s: %s%s"
                                  indent
                                  (cond
                                   (entry-is-symlink-p
                                    "link")
                                   (entry-is-dir-p
                                    "dir")
                                   (t
                                    "file"))
                                  entry-rel-path size-info)
                          results)

                    ;; Recurse into subdirectories if requested (but not into symlinked
                    ;; directories).
                    (when (and recursive entry-is-dir-p)
                      (collect-entries entry-full-path entry-rel-path (1+ depth))))))))))

      ;; Start collection.
      (collect-entries full-path "" 0))

    ;; Return formatted results.
    (if results
        (string-join (reverse results) "\n")
      "Directory is empty")))

(defun macher--tool-edit-file (context path old-text new-text &optional replace-all)
  "Edit file specified by PATH within the workspace.

CONTEXT is a `macher-context' struct containing workspace information.

PATH is the path to the file, relative to the workspace root.

OLD-TEXT is the exact text to replace.

NEW-TEXT is the replacement text.

REPLACE-ALL, if non-nil, replaces all occurrences; otherwise errors if
multiple matches exist.

Returns nil on success. Signals an error if the file is not found or if
the edit operation fails. Sets the dirty-p flag on the context to
indicate changes."
  ;; Handle :json-false inputs for replace-all parameter.
  (let ((replace-all (and replace-all (not (eq replace-all :json-false)))))
    ;; Validate required parameters
    (unless (and old-text new-text)
      (error "Both old_text and new_text are required"))
    ;; Use the helper function to perform the edit.
    (macher--with-workspace-file context path
                                 (lambda (full-path new-content)
                                   (let ((result
                                          (macher--edit-string new-content old-text new-text
                                                               replace-all)))
                                     ;; Update the content in the context.
                                     (macher-context--set-new-content-for-file
                                      full-path result context)
                                     ;; Return nil to indicate success.
                                     nil))
                                 t)))

(defun macher--tool-multi-edit-file (context path edits)
  "Make multiple edits to a file specified by PATH within the workspace.

CONTEXT is a `macher-context' struct containing workspace information.

PATH is the path to the file, relative to the workspace root.

EDITS is a vector of edit operations, each containing :old_string and
:new_string. For compatibility with LLMs that don't support array
arguments, a JSON string representing an array is also accepted.

All edits are applied in sequence to the same file. Each edit requires
exact whitespace matching. If any edit fails, the entire operation
fails.

Returns nil on success. Signals an error if the file is not found or if
any edit operation fails. Sets the dirty-p flag on the context to
indicate changes."
  ;; Validate that edits is a vector, i.e. a JSON array. Ideally the argument should
  ;; have been sent as an actual array, but some LLMs seem to have trouble with this,
  ;; and instead send JSON strings which decode to an array. Allow both cases, as
  ;; although the string case technically violates the tool signature, we can still
  ;; handle it unambiguously.
  (unless (vectorp edits)
    (if (stringp edits)
        ;; Try to decode JSON string to vector.
        (condition-case nil
            (let ((decoded (json-parse-string edits :array-type 'vector :object-type 'plist)))
              (if (vectorp decoded)
                  (setq edits decoded)
                (error
                 "The 'edits' parameter must be an array, but the decoded JSON is not an array")))
          (error
           (error
            "The 'edits' parameter must be an array of objects, or a valid JSON string representing an array")))
      ;; Not a vector or string - invalid input.
      (error "The 'edits' parameter must be an array of objects, not %s" (type-of edits))))
  ;; Use the helper function to perform the edits.
  (macher--with-workspace-file context path
                               (lambda (full-path new-content)
                                 ;; Apply edits sequentially.
                                 (cl-loop
                                  for edit across edits do
                                  (let ((old-text (plist-get edit :old_text))
                                        (new-text (plist-get edit :new_text))
                                        (replace-all (plist-get edit :replace_all)))

                                    (unless (and old-text new-text)
                                      (error
                                       "Each edit must contain old_text and new_text properties"))
                                    ;; Handle :json-false inputs for replace-all parameter.
                                    (setq replace-all
                                          (and replace-all (not (eq replace-all :json-false))))
                                    (setq new-content
                                          (macher--edit-string new-content old-text new-text
                                                               replace-all))
                                    ;; Update the content in the context after each edit.
                                    (macher-context--set-new-content-for-file
                                     full-path new-content context)))
                                 ;; Return nil to indicate success.
                                 nil)
                               t))

(defun macher--tool-write-file (context path content)
  "Create a new file or completely overwrite an existing file with CONTENT.

CONTEXT is a `macher-context' struct containing workspace information.

PATH is the path to the file, relative to the workspace root.

CONTENT is the complete new content for the file.

Use with caution as it will overwrite existing files without warning.
Handles text content with proper encoding.

Returns nil on success. Sets the dirty-p flag on the context to indicate changes."
  (let* ((workspace (macher-context-workspace context))
         (resolve-workspace-path (apply-partially #'macher--resolve-workspace-path workspace))
         (full-path (funcall resolve-workspace-path path)))
    ;; Set the dirty flag to indicate changes are being made.
    (setf (macher-context-dirty-p context) t)
    ;; Set the new content in the context (this will create the entry if needed).
    (macher-context--set-new-content-for-file full-path content context)
    nil))

(defun macher--tool-move-file (context source-path destination-path)
  "Move or rename files within the workspace.

CONTEXT is a `macher-context' struct containing workspace information.

SOURCE-PATH is the source path relative to the workspace root.

DESTINATION-PATH is the destination path relative to the workspace root.

Can move files between directories and rename them in a single operation.
If the destination exists, the operation will fail. Works across different
directories and can be used for simple renaming within the same directory.

Returns nil on success. Signals an error if the source file is not found or
if the destination already exists. Sets the dirty-p flag on the context to
indicate changes."
  (let* ((workspace (macher-context-workspace context))
         (resolve-workspace-path (apply-partially #'macher--resolve-workspace-path workspace))
         (dest-full-path (funcall resolve-workspace-path destination-path)))
    ;; Check if destination already exists.
    (let ((dest-contents (macher-context--contents-for-file dest-full-path context)))
      (when (cdr dest-contents)
        (error (format "Destination '%s' already exists" destination-path))))
    ;; Use the helper function to move the file.
    (macher--with-workspace-file context source-path
                                 (lambda (source-full-path source-new-content)
                                   ;; Copy content from source to destination.
                                   (macher-context--set-new-content-for-file
                                    dest-full-path source-new-content context)
                                   ;; Mark source for deletion by setting its content to nil.
                                   (macher-context--set-new-content-for-file
                                    source-full-path nil context)
                                   ;; Return nil to indicate success.
                                   nil)
                                 t)))

(defun macher--tool-delete-file (context rel-path)
  "Delete a file specified by REL-PATH within the workspace.

CONTEXT is a `macher-context' struct containing workspace information.

REL-PATH is the path to the file, relative to the workspace root.

The file must exist and will be marked for deletion in the patch.
Permanently removes the file from the workspace.

Returns nil on success. Signals an error if the file is not found.
Sets the dirty-p flag on the context to indicate changes."
  ;; Use the helper function to delete the file.
  (macher--with-workspace-file context rel-path
                               (lambda (full-path _editable-content)
                                 ;; For deletion, set the new content to nil to indicate deletion.
                                 (macher-context--set-new-content-for-file full-path nil context)
                                 ;; Return nil to indicate success.
                                 nil)
                               t))

(cl-defun macher--search-get-xref-matches (context pattern &key path file-regexp case-insensitive)
  "Get raw xref matches as an alist of ((filename . matches)).

CONTEXT is the macher-context, which will be taken into account when
searching for matches (including new and deleted files).

PATTERN is the regexp pattern to search for.

PATH is the directory or file to search, defaulting to the context's
workspace root.

FILE-REGEXP is a regexp to specify which files should be included - it
will be matched against each file's path relative to the search PATH.

CASE-INSENSITIVE, if non-nil, performs a case-insensitive search.

Returns an alist containing 'xref-match-item' structs for each file,
with structure:

 ((rel-path . (xref-match-item1 xref-match-item2 ...)) ...)

The search will be performed using `xref-matches-in-files', which uses
the 'xref-search-program' to perform the search."
  (require 'xref)
  (let* (
         ;; Set case-fold-search to enable case-insensitive search when needed. Note: xref uses
         ;; `grep-expand-template' to generate the command, which performs a case-insensitive search
         ;; if/only if `case-fold-search' is truthy and `isearch-no-upper-case-p' is true for the
         ;; regexp in question.
         (case-fold-search case-insensitive)
         (workspace (macher-context-workspace context))
         (workspace-root (macher--workspace-root workspace))
         (resolve-workspace-path (apply-partially #'macher--resolve-workspace-path workspace))
         (search-path (funcall resolve-workspace-path (or path ".")))
         (context-contents (macher-context-contents context))
         (workspace-files (macher--workspace-files workspace))
         (path
          (when path
            (expand-file-name path workspace-root)))
         ;; When case-insensitive is true, downcase the pattern so that `isearch-no-upper-case-p'
         ;; returns true, which is required for grep's case-insensitive mode to activate.
         (search-pattern
          (if case-insensitive
              (downcase pattern)
            pattern))
         (temp-files-alist '())
         (results '()))

    (let* (
           ;; Create a list of files to search, filtering by path and glob.
           (files-to-search
            (cl-remove-if-not
             (lambda (file-path)
               (let ((full-path (expand-file-name file-path workspace-root)))
                 (and
                  ;; File is under the search path.
                  (string-prefix-p search-path full-path)
                  ;; File matches regexp if specified.
                  (if file-regexp
                      (let ((rel-path
                             (if path
                                 (file-relative-name full-path search-path)
                               (file-relative-name full-path workspace-root))))
                        (string-match-p file-regexp rel-path))
                    t)
                  ;; File exists or has content in context.
                  (or (file-exists-p full-path)
                      (let ((entry (assoc (macher--normalize-path full-path) context-contents)))
                        (and entry (cdr (cdr entry))))))))
             workspace-files))
           ;; Add any context-only files that match our criteria.
           (context-only-files
            (cl-remove-if-not
             (lambda (entry)
               (let* ((file-path (car entry))
                      (contents (cdr entry))
                      (orig-content (car contents))
                      (new-content (cdr contents)))
                 (and
                  ;; File has new content (not deleted).
                  new-content
                  ;; File is not already in workspace-files.
                  (not
                   (cl-find
                    file-path
                    files-to-search
                    :test (lambda (a b) (string= a (expand-file-name b workspace-root)))))
                  ;; File is under search path.
                  (string-prefix-p search-path file-path)
                  ;; File matches regexp if specified.
                  (if file-regexp
                      (let ((rel-path
                             (if path
                                 (file-relative-name file-path search-path)
                               (file-relative-name file-path workspace-root))))
                        (string-match-p file-regexp rel-path))
                    t))))
             context-contents))
           ;; Combine and convert to absolute paths
           (all-files-to-search
            (append
             (mapcar
              (lambda (f)
                (if (file-name-absolute-p f)
                    f
                  (expand-file-name f workspace-root)))
              files-to-search)
             (mapcar #'car context-only-files))))

      ;; Create temporary files for context modifications.
      (setq all-files-to-search
            (mapcar
             (lambda (file-path)
               (let ((entry (assoc (macher--normalize-path file-path) context-contents)))
                 (if entry
                     (let* ((contents (cdr entry))
                            (new-content (cdr contents)))
                       (if new-content
                           ;; Create temp file with modified content.
                           (let ((temp-file (make-temp-file "macher-search")))
                             (with-temp-buffer
                               (insert new-content)
                               (write-region (point-min) (point-max) temp-file nil 'silent))
                             (push (cons temp-file file-path) temp-files-alist)
                             temp-file)
                         ;; File is deleted in context, skip it.
                         nil))
                   ;; No context modification, use original file.
                   file-path)))
             all-files-to-search))

      ;; Remove nil entries (deleted files).
      (setq all-files-to-search (cl-remove-if #'null all-files-to-search))

      ;; Use xref-matches-in-files to search and clean up temp files afterwards.
      (unwind-protect
          (progn
            (when all-files-to-search
              (let* ((xref-matches (xref-matches-in-files search-pattern all-files-to-search)))
                (dolist (match xref-matches)
                  (let*
                      ((location (xref-item-location match))
                       (file-path (xref-file-location-file location))
                       ;; Map temp file back to original file.
                       (original-file (or (cdr (assoc file-path temp-files-alist)) file-path))
                       ;; Ensure matched files are loaded into the macher-context, so their contents
                       ;; won't change before they're accessed.
                       (_ (macher-context--contents-for-file original-file context))
                       ;; Always show results relative to workspace root (like grep relative to
                       ;; cwd). Special case: if path points to a single file, show the original
                       ;; path parameter.
                       (rel-path
                        (if (and path
                                 (file-exists-p search-path)
                                 (not (file-directory-p search-path)))
                            ;; Path is a single file, use the original path parameter.
                            path
                          ;; Otherwise, always relative to workspace root.
                          (file-relative-name original-file workspace-root))))

                    ;; Group results by file for proper formatting.
                    (let ((file-entry (assoc rel-path results)))
                      (if file-entry
                          ;; Add to the existing file's match list - file-entry structure is
                          ;; (rel-path . xref-match-item-list).
                          (setcdr file-entry (append (cdr file-entry) (list match)))
                        ;; Create new file entry - structure is (rel-path . xref-match-item-list).
                        ;; Note this pushes to the beginning of the list.
                        (push (cons rel-path (list match)) results)))))))

            ;; Return the results - temp files are cleaned up in the unwind-protect. Each entry was
            ;; prepended to the results array, so we need to use `reverse' to restore the original
            ;; order of the workspace's file list.
            (reverse results))

        ;; Cleanup: delete temporary files.
        (dolist (temp-entry temp-files-alist)
          (when (file-exists-p (car temp-entry))
            (delete-file (car temp-entry))))))))

(defun macher--search-format-files-mode (matches-alist)
  "Format search results for files mode output.

MATCHES-ALIST has structure ((rel-path . (xref-match-item1 xref-match-item2 ...)) ...)."
  (let ((output "")
        (total-matches
         (apply #'+ (mapcar (lambda (file-entry) (length (cdr file-entry))) matches-alist))))
    ;; Files mode: show file paths with match counts
    (dolist (file-entry matches-alist)
      (let ((file-path (car file-entry))
            (matches (cdr file-entry)))
        (setq output
              (concat
               output
               (format "%s (%d %s)\n"
                       file-path (length matches)
                       (if (= (length matches) 1)
                           "match"
                         "matches"))))))
    (when matches-alist
      (setq output
            (concat
             output
             (format "\nTotal: %d %s in %d %s"
                     total-matches
                     (if (= total-matches 1)
                         "match"
                       "matches")
                     (length matches-alist)
                     (if (= (length matches-alist) 1)
                         "file"
                       "files")))))
    output))

(defun macher--search-format-content-mode
    (context matches-alist lines-before lines-after show-line-numbers)
  "Format search results for content mode output.

CONTEXT is the macher-context struct.

LINES-BEFORE and LINES-AFTER specify additional lines of context to
include.

SHOW-LINE-NUMBERS toggles inclusion of line numbers in the output.

MATCHES-ALIST is an alist of the form:

   ((rel-path . (xref-match-item1 xref-match-item2 ...)) ...)

The inner lists of the MATCHES-ALIST contain 'xref-match-item' structs."
  (let* ((workspace (macher-context-workspace context))
         (workspace-root (macher--workspace-root workspace))
         (context-contents (macher-context-contents context))
         (output ""))

    ;; Content mode: show matching lines with grep-like format.
    (dolist (file-entry matches-alist)
      (let* ((file-path (car file-entry))
             (matches (cdr file-entry)))

        ;; For content mode with context lines, we need to read the file content.
        (let* ((original-file (expand-file-name file-path workspace-root))
               (entry (assoc (macher--normalize-path original-file) context-contents))
               (has-context (or lines-before lines-after)))

          ;; Create a hash table mapping line numbers to lists of matches.
          (let ((line-matches (make-hash-table :test 'eq)))
            (dolist (match matches)
              (let ((line-num (xref-file-location-line (xref-item-location match))))
                (push match (gethash line-num line-matches))))

            (if has-context
                ;; Context mode: merge overlapping ranges and show continuous output.
                (let*
                    ((file-content
                      (if entry
                          ;; File is in context, use the current content (new-content). Note: deleted
                          ;; files (where new-content is nil) are filtered out during the search phase, so
                          ;; we should never encounter them here, but it doesn't really matter if we do -
                          ;; we'll just have nil file-content in that case.
                          (cdr (cdr entry))
                        ;; File not in context, read from disk.
                        (when (file-exists-p original-file)
                          (with-temp-buffer
                            (insert-file-contents original-file)
                            (buffer-substring-no-properties (point-min) (point-max))))))
                     (lines
                      (when file-content
                        (split-string file-content "\n")))

                     (line-ranges '()))

                  ;; Calculate line ranges for each match (including context)
                  (dolist (match matches)
                    (let* ((line-num (xref-file-location-line (xref-item-location match)))
                           (start-line (max 1 (- line-num (or lines-before 0))))
                           (end-line (min (length lines) (+ line-num (or lines-after 0)))))
                      (push (list start-line end-line) line-ranges)))

                  ;; Sort ranges by start line.
                  (setq line-ranges (sort line-ranges (lambda (a b) (< (car a) (car b)))))

                  ;; Merge overlapping or adjacent ranges.
                  (let ((merged-ranges '())
                        (current-start nil)
                        (current-end nil))
                    (dolist (range line-ranges)
                      (let ((start (car range))
                            (end (cadr range)))
                        (if (or (null current-start)
                                ;; ; Not overlapping/adjacent.
                                (> start (1+ current-end)))
                            (progn
                              (when current-start
                                (push (list current-start current-end) merged-ranges))
                              (setq
                               current-start start
                               current-end end))
                          ;; Overlapping or adjacent - merge.
                          (setq current-end (max current-end end)))))
                    (when current-start
                      (push (list current-start current-end) merged-ranges))
                    (setq merged-ranges (reverse merged-ranges))

                    ;; Output merged ranges with separators between non-adjacent ranges.
                    (let ((need-separator nil))
                      (dolist (range merged-ranges)
                        (let ((start-line (car range))
                              (end-line (cadr range)))
                          ;; Add separator between non-adjacent ranges.
                          (when need-separator
                            (setq output (concat output "--\n")))
                          (setq need-separator t)

                          ;; Output lines in this range.
                          (let ((i start-line))
                            (while (<= i end-line)
                              (let*
                                  ((line (nth (1- i) lines))
                                   (line-match-list (gethash i line-matches))
                                   (match-count (length line-match-list))
                                   (is-match (> match-count 0))
                                   ;; Replace line with ripgrep-style placeholder if it exceeds the
                                   ;; maximum length.
                                   (truncated-line
                                    (if (> (length line) macher-match-max-columns)
                                        (if is-match
                                            (format "[Omitted long line with %d matches]"
                                                    match-count)
                                          "[Omitted long context line]")
                                      line)))
                                (let* ((separator
                                        (if is-match
                                            ":"
                                          "-"))
                                       (line-format
                                        (if show-line-numbers
                                            (format "%s%s%d%s%s\n"
                                                    file-path
                                                    separator
                                                    i
                                                    separator
                                                    truncated-line)
                                          (format "%s%s%s\n" file-path separator truncated-line))))
                                  (setq output (concat output line-format))
                                  (setq i (1+ i)))))))))))

              ;; Simple content mode without before/after lines. In this case, xref has already
              ;; loaded enough information to render each line, so we don't need to read the entire
              ;; file/split it into lines.
              (let ((processed-lines (make-hash-table :test 'eq)))
                ;; Loop over unique line numbers and output concatenated summaries.
                (maphash
                 (lambda (line-num line-match-list)
                   (unless (gethash line-num processed-lines)
                     (puthash line-num t processed-lines)
                     (let* ((summaries
                             (mapcar
                              #'xref-item-summary
                              ;; We need to reverse the list, since entries were prepended in order.
                              (reverse line-match-list)))
                            (combined-summary (substring-no-properties (string-join summaries "")))
                            (match-count (length line-match-list))
                            ;; Replace summary with ripgrep-style placeholder if it exceeds the
                            ;; maximum length.
                            (truncated-summary
                             (if (> (length combined-summary) macher-match-max-columns)
                                 (format "[Omitted long line with %d matches]" match-count)
                               combined-summary))
                            (line-format
                             (if show-line-numbers
                                 (format "%s:%d:%s\n" file-path line-num truncated-summary)
                               (format "%s:%s\n" file-path truncated-summary))))
                       (setq output (concat output line-format)))))
                 line-matches)))))))

    output))

(cl-defun macher--tool-search-helper
    (context
     pattern
     &key
     path
     file-regexp
     mode
     case-insensitive
     lines-after
     lines-before
     show-line-numbers
     head-limit)
  "Search for PATTERN within the workspace using grep-like functionality.

CONTEXT is a `macher-context' struct containing workspace information.

PATTERN is the regular expression to search for (required).

Keyword arguments:
- PATH: directory or file to search, relative to workspace root.
  Defaults to workspace root if not provided.

- GLOB: file pattern relative to PATH to filter results (e.g., \"*.js\",
  \"**/docker-compose*.yml\").

- MODE: output format - \"files\" (default) shows file paths with match
  counts, \"content\" shows grep-style matching lines with context.

- CASE-INSENSITIVE: if non-nil, performs a case-insensitive
  search (default: nil).

- LINES-AFTER: number of lines to show after each match (content mode only).

- LINES-BEFORE: number of lines to show before each match (content mode only).
- SHOW-LINE-NUMBERS: if non-nil, includes line numbers in output (content
  mode only).

- HEAD-LIMIT: limits output to first N lines (like `head -N`).

Returns formatted search results as a string. Considers workspace context
including any pending changes, creations, or deletions.

HEAD-LIMIT applies to the final formatted output and works exactly like
piping the results through `head -N`."
  (let ((search-mode (or mode "files")))
    ;; Validate parameters.
    (unless (and pattern (not (string-empty-p pattern)))
      (error "Pattern is required and cannot be empty"))

    (when (and head-limit (< head-limit 0))
      (error "Head-limit cannot be negative"))

    ;; Handle float inputs by rounding to integers, like the read tool does. Some LLMs seem to have
    ;; trouble invoking tools with integer inputs.
    (let* ((parsed-lines-after
            (when lines-after
              (round lines-after)))
           (parsed-lines-before
            (when lines-before
              (round lines-before)))
           (parsed-head-limit
            (when head-limit
              (round head-limit))))

      ;; Get raw xref matches.
      (let* ((matches-alist
              (macher--search-get-xref-matches
               context
               pattern
               :path path
               :file-regexp file-regexp
               :case-insensitive case-insensitive))
             (output "No matches found."))

        ;; Check if any matches were found.
        (when matches-alist
          ;; Format results based on mode.
          (cond
           ((string= search-mode "files")
            (setq output (macher--search-format-files-mode matches-alist)))

           ((string= search-mode "content")
            (setq output
                  (macher--search-format-content-mode
                   context matches-alist parsed-lines-before parsed-lines-after show-line-numbers)))

           (t
            (error "Mode must be either 'files' or 'content'"))))

        ;; Apply head-limit if specified.
        (when parsed-head-limit
          (let ((lines (split-string output "\n")))
            (when (> (length lines) parsed-head-limit)
              (setq output (string-join (seq-take lines parsed-head-limit) "\n")))))
        output))))

(defun macher--tool-search
    (context
     pattern
     &optional
     path
     file-regexp
     mode
     case-insensitive
     lines-after
     lines-before
     show-line-numbers
     head-limit)
  "Search for PATTERN within the workspace using grep-like functionality.

CONTEXT is a `macher-context' struct containing workspace information.

PATTERN is the regular expression to search for (required).

PATH is the directory or file to search, relative to workspace root.
Defaults to workspace root if not provided.

FILE-REGEXP is a regular expression to filter files by path relative to
search path.

MODE specifies output format:
- \"files\" (default) - Show file paths with match counts
- \"content\" - Show grep-style matching lines with context

CASE-INSENSITIVE, if non-nil, performs a case-insensitive search (default: nil).

LINES-AFTER specifies number of lines to show after each match (content
mode only).

LINES-BEFORE specifies number of lines to show before each
match (content mode only).

SHOW-LINE-NUMBERS, if non-nil, includes line numbers in output (content
mode only).

HEAD-LIMIT limits output to first N lines (equivalent to piping through
`head -N`).

Returns formatted search results as a string. Considers workspace
context including any pending changes, creations, or deletions."
  (macher--tool-search-helper
   context
   pattern
   :path path
   :file-regexp file-regexp
   :mode mode
   :case-insensitive case-insensitive
   :lines-after lines-after
   :lines-before lines-before
   :show-line-numbers show-line-numbers
   :head-limit head-limit))

(defun macher--read-tools (context make-tool-function)
  "Generate read-only tools for workspace operations with CONTEXT.

CONTEXT is a `macher-context' struct with slots for workspace info.
MAKE-TOOL-FUNCTION is a function that takes the same arguments as
`gptel-make-tool' but must be used instead of `gptel-make-tool' to create tools.

Returns a list of read-only tools that allow the LLM to inspect files
in the workspace.

Tools are created using MAKE-TOOL-FUNCTION so they can be properly removed from
the global gptel registry when the request completes."
  (list
   (funcall
    make-tool-function
    :name "read_file_in_workspace"
    :function (apply-partially #'macher--tool-read-file context)
    :description
    (concat
     "Read file contents in the workspace.\n"
     "\n"
     "USAGE RULES:\n"
     "1. NEVER re-read files that were already provided in the REQUEST CONTEXT\n"
     "2. For files NOT in the REQUEST CONTEXT: try reading the ENTIRE file first "
     "(no offset/limit) to understand its structure\n"
     "\n"
     "!!! CRITICAL: You MUST NOT use this tool to read files whose contents were already "
     "provided to you in the REQUEST CONTEXT, except to resolve confusion or verify edits.\n"
     "\n"
     "Returns the file contents as a string.")
    :confirm nil
    :include nil
    :args
    `((:name "path" :type string :description "Path to the file, relative to workspace root")
      (:name
       "offset"
       :type number
       :optional t
       :description
       ,(concat
         "Line number to start reading from (1-based). For negative values, starts at that many "
         "lines before the end of the file. ONLY use for targeted re-reads - read entire file first!"))
      (:name
       "limit"
       :type number
       :optional t
       :description
       ,(concat
         "Number of lines to read from the start position. For negative values, the actual "
         "limit is computed as (total_lines + limit). For example, with a 100-line file: "
         "limit=10 reads 10 lines, limit=-10 reads 90 lines (100 + (-10)). "
         "ONLY use for targeted re-reads - read entire file first!"))
      (:name
       "show_line_numbers"
       :type boolean
       :optional t
       :description
       ,(concat
         "Include line numbers in output (cat -n style: each line prefixed with right-aligned "
         "line number and a tab character)"))))

   (funcall make-tool-function
            :name "list_directory_in_workspace"
            :function (apply-partially #'macher--tool-list-directory context)
            :description
            (concat
             "List directory contents in the workspace. "
             "Shows files and directories with clear prefixes (file: or dir:). "
             "Can optionally recurse into subdirectories and show file sizes. "
             "Takes the current workspace state into account, including pending changes.\n\n"
             "NOTE: The full workspace file listing is usually already provided in the "
             "WORKSPACE CONTEXT. Only use this tool if you need specific directory "
             "details or file sizes.")
            :confirm nil
            :include nil
            :args
            `((:name
               "path"
               :type string
               :description "Path to the directory, relative to workspace root")
              (:name
               "recursive"
               :type boolean
               :optional t
               :description "If true, recursively list subdirectories")
              (:name
               "sizes"
               :type boolean
               :optional t
               :description "If true, include file sizes in the output")))

   (funcall
    make-tool-function
    :name "search_in_workspace"
    :function (apply-partially #'macher--tool-search context)
    :description
    (concat
     "Search for patterns within the workspace using grep or something like it.\n"
     "\n"
     "Supports two output modes:\n"
     "- 'files' (default): Shows file paths with match counts\n"
     "- 'content': Shows matching lines with optional context\n"
     "\n"
     "!!! CRITICAL: You MUST NOT use this tool to search files whose contents were "
     "already provided to you in the REQUEST CONTEXT. You already have their contents!")
    :confirm nil
    :include nil
    :args
    `((:name
       "pattern"
       :type string
       :description "Regular expression pattern to search for (required)")
      (:name
       "path"
       :type string
       :optional t
       :description "Directory or file to search, relative to workspace root (defaults to workspace root)")
      (:name
       "file_regexp"
       :type string
       :optional t
       :description
       ,(concat
         "Filter files by regular expression matched against file path relative to search path. "
         "Examples: '\\.py$' for Python files, 'src/.*\\.js$' for JS files in src/, "
         "'test.*\\.py$' for test files"))
      (:name
       "mode"
       :type string
       :optional t
       :description
       ,(concat
         "Output mode: 'files' (default, shows paths + counts) or "
         "'content' (shows grep-style matching lines)"))
      (:name
       "case_insensitive"
       :type boolean
       :optional t
       :description "If true, perform case-insensitive search (default: false)")
      (:name
       "lines_after"
       :type number
       :optional t
       :description "Number of lines to show after each match (content mode only). Like `grep -A`.")
      (:name
       "lines_before"
       :type number
       :optional t
       :description "Number of lines to show before each match (content mode only). Like `grep -B`.")
      (:name
       "show_line_numbers"
       :type boolean
       :optional t
       :description "Include line numbers in output (content mode only). Like `grep -n`.")
      (:name
       "head_limit"
       :type number
       :optional t
       :description "Limit output to first N lines (equivalent to piping through `head -N`)")))))

(defun macher--edit-tools (context make-tool-function)
  "Generate file editing tools for workspace operations with CONTEXT.

CONTEXT is a `macher-context' struct with slots for workspace info.
MAKE-TOOL-FUNCTION is a function that takes the same arguments as
`gptel-make-tool' but must be used instead of `gptel-make-tool' to create tools.

Returns a list of tools that allow the LLM to edit files in the workspace.

Tools are created using MAKE-TOOL-FUNCTION so they can be properly removed from
the global gptel registry when the request completes."
  (list
   (funcall
    make-tool-function
    :name "edit_file_in_workspace"
    :function (apply-partially #'macher--tool-edit-file context)
    :description
    (concat
     "Make exact string replacements in a text file. "
     "The old_text must match exactly including all whitespace, newlines, and indentation. "
     "Do NOT include line numbers in old_text or new_text - use only the actual file content. "
     "If replace_all is false and multiple matches exist, the operation fails - "
     "provide more specific context in old_text to make the match unique. "
     "Returns null on success.")
    :confirm nil
    :include nil
    :args
    `((:name "path" :type string :description "Path to the file, relative to workspace root")
      (:name
       "old_text"
       :type string
       :description
       ,(concat
         "Exact text to find and replace. Must match precisely including whitespace and "
         "newlines. Do NOT include line numbers."))
      (:name "new_text" :type string :description "Text to replace the old_text with")
      (:name
       "replace_all"
       :type boolean
       :optional t
       :description "If true, replace all occurrences. If false (default), error if multiple matches exist")))

   (funcall
    make-tool-function
    :name "multi_edit_file_in_workspace"
    :function (apply-partially #'macher--tool-multi-edit-file context)
    :description
    (concat
     "Make multiple exact string replacements in a single file. "
     "Edits are applied sequentially in array order to the same file. "
     "Each edit requires exact whitespace matching. Do NOT include line numbers in old_text or new_text. "
     "If any edit fails, no changes are made. Returns null on success.")
    :confirm nil
    :include nil
    :args
    `((:name "path" :type string :description "Path to the file, relative to workspace root")
      (:name
       "edits"
       :type array
       :description "Array of edit operations to apply in sequence"
       :items
       (:type
        object
        :properties
        (:old_text
         (:type
          string
          :description
          ,(concat
            "Exact text to find and replace. Must match precisely including whitespace "
            "and newlines. Do NOT include line numbers."))
         :new_text (:type string :description "Text to replace the old_text with")
         :replace_all
         (:type
          boolean
          :description "If true, replace all occurrences. If false (default), error if multiple matches exist"))
        :required ["old_text" "new_text"]))))

   (funcall make-tool-function
            :name "write_file_in_workspace"
            :function (apply-partially #'macher--tool-write-file context)
            :description
            (concat
             "Create a new file or completely overwrite an existing file. "
             "WARNING: This replaces ALL existing content without warning. "
             "Use edit_file_in_workspace for partial changes. "
             "Returns null on success.")
            :confirm nil
            :include nil
            :args
            '((:name
               "path"
               :type string
               :description "Path to the file, relative to workspace root")
              (:name
               "content"
               :type string
               :description "Complete new content that will replace the entire file")))

   (funcall make-tool-function
            :name "move_file_in_workspace"
            :function (apply-partially #'macher--tool-move-file context)
            :description
            (concat
             "Move or rename files within the workspace. "
             "Can move files between directories and rename them in a single operation. "
             "Fails if the destination already exists. "
             "Returns null on success.")
            :confirm nil
            :include nil
            :args
            '((:name
               "source_path"
               :type string
               :description "Current path of the file to move, relative to workspace root")
              (:name
               "destination_path"
               :type string
               :description "New path for the file, relative to workspace root")))

   (funcall make-tool-function
            :name "delete_file_in_workspace"
            :function (apply-partially #'macher--tool-delete-file context)
            :description
            (concat
             "Delete a file from the workspace. "
             "Fails if the file does not exist. "
             "Returns null on success.")
            :confirm nil
            :include nil
            :args
            '((:name
               "path"
               :type string
               :description "Path to the file to delete, relative to workspace root")))))

(defun macher--edit-string (content old-string new-string &optional replace-all)
  "In CONTENT string, replace OLD-STRING with NEW-STRING.

If REPLACE-ALL is non-nil, replace all occurrences. Otherwise, error if
multiple matches exist and replace only single occurrences.

Return the new content string if the replacement was successful, or signal
an error if it was not."
  (let ((case-fold-search nil))
    ;; Error if old-string and new-string are identical
    (when (string-equal old-string new-string)
      (error "No changes to make: old_string and new_string are exactly the same"))
    ;; Handle empty old-string specially.
    (if (string-empty-p old-string)
        (if (string-empty-p content)
            ;; If content is empty, return the new string.
            new-string
          ;; If content is not empty and old-string is empty, throw an error.
          (error "Cannot replace empty string in non-empty content"))
      ;; Normal case: old-string is not empty.
      (let* ((start 0)
             (matches 0)
             (match-positions '()))
        ;; Count matches and collect positions.
        (while (setq start (string-search old-string content start))
          (setq matches (1+ matches))
          (push start match-positions)
          (setq start (+ start (length old-string))))

        (cond
         ((= matches 0)
          (error "String to replace not found in file"))
         ((and (> matches 1) (not replace-all))
          (error
           (concat
            "Found %d matches of the string to replace, but replace_all is false. "
            "To replace all occurrences, set replace_all to true. To replace only one "
            "occurrence, please provide more context to uniquely identify the instance")
           matches))
         (t
          ;; Perform replacement(s)
          (if replace-all
              ;; Replace all occurrences (work backwards to preserve positions)
              (let ((result content))
                (dolist (pos (sort match-positions '>))
                  (setq result
                        (concat
                         (substring result 0 pos)
                         new-string
                         (substring result (+ pos (length old-string))))))
                result)
            ;; Replace single occurrence
            (let ((match-pos (car (reverse match-positions))))
              (concat
               (substring content 0 match-pos)
               new-string
               (substring content (+ match-pos (length old-string))))))))))))

(defun macher--generate-patch-diff (context)
  "Generate a raw diff to populate the patch buffer.
CONTEXT is the 'macher-context' object. Returns the generated diff text."
  (let* ((contents-alist (macher-context-contents context))
         (workspace (macher-context-workspace context))
         (base-dir (macher--workspace-root workspace))
         (result ""))

    ;; Use the system diff command to generate a unified diff for each file.
    ;; Sort the contents list by filename for consistent patch ordering.
    (dolist (entry (sort (copy-sequence contents-alist) (lambda (a b) (string< (car a) (car b)))))
      (let* ((filename (car entry))
             (contents (cdr entry))
             (orig-content (car contents))
             (new-content (cdr contents))
             ;; Get the path relative to the base directory.
             (rel-path (file-relative-name filename base-dir))
             ;; Check if file has actually changed.
             (file-changed-p (not (equal orig-content new-content))))

        ;; Only generate diff if the file has actually changed.
        (when file-changed-p
          (let ((temp-orig (make-temp-file "gptel-diff-orig"))
                (temp-new (make-temp-file "gptel-diff-new")))

            ;; Write original content (or empty file for new files).
            (with-temp-buffer
              (when orig-content
                (insert orig-content))
              (write-region (point-min) (point-max) temp-orig nil 'silent))

            ;; Write new content or create empty file for deletions.
            (with-temp-buffer
              (when new-content
                (insert new-content))
              (write-region (point-min) (point-max) temp-new nil 'silent))

            ;; Generate diff and append to result.
            (with-temp-buffer
              ;; Add the standard git diff header, which allows diff-mode to create new files.
              (insert (format "diff --git a/%s b/%s\n" rel-path rel-path))

              ;; Use diff to generate a unified patch with the correct file path.
              (when (or orig-content new-content)
                (call-process "diff"
                              nil t nil "-u" "--label"
                              (if orig-content
                                  (concat "a/" rel-path)
                                ;; Use /dev/null to denote file creations.
                                "/dev/null")
                              "--label"
                              (if new-content
                                  (concat "b/" rel-path)
                                ;; Use /dev/null to denote file deletions.
                                "/dev/null")
                              temp-orig temp-new))

              ;; Append the diff to the result.
              (setq result (concat result (buffer-string))))

            ;; Clean up the temp files.
            (delete-file temp-orig)
            (delete-file temp-new)))))

    result))

;;; Context Content Management

(defun macher-context--set-new-content-for-file (path content context)
  "Set the new content for PATH to CONTENT in the given CONTEXT.

PATH is any absolute file path. By default, macher will only call this
for paths within the CONTEXT's workspace..

CONTENT is the string content to use for the file, or nil if the file
is being deleted.

CONTEXT must be a `macher-context' struct.

Updates the CONTEXT's :contents alist with the new content mapping."
  (cl-assert (macher-context-p context) nil "CONTEXT must be a macher-context struct")

  (let* ((normalized-path (macher--normalize-path path))
         (context-contents (macher-context-contents context))
         (existing-entry (assoc normalized-path context-contents)))
    (if existing-entry
        ;; Update existing entry.
        (let ((contents (cdr existing-entry)))
          (setcdr contents content)
          contents)
      ;; Create new entry. For new files, orig-content is nil.
      (let* ((orig-content
              (if (file-exists-p normalized-path)
                  (with-temp-buffer
                    (insert-file-contents normalized-path)
                    (buffer-substring-no-properties (point-min) (point-max)))
                nil))
             (content-pair (cons orig-content content)))
        ;; Add to context.
        (setf (macher-context-contents context)
              (cons
               (cons normalized-path content-pair)
               (assoc-delete-all normalized-path context-contents)))
        content-pair))))

(defun macher-context--contents-for-file (path context)
  "Get or create content strings for PATH in the macher CONTEXT.

Returns a cons cell \\=(orig-content new-content).

PATH can be any absolute file path. By default, macher will only call
this for paths within the CONTEXT's workspace.

Returns a cons cell (orig-content . new-content) of strings for the file.
If the orig-content is nil, the file is being created; if the new-content
is nil, the file is being deleted.

Also updates the CONTEXT's :contents alist if the relevant entry was not
yet present. In that case, loads file contents if the file exists;
otherwise returns (nil . nil)."
  (cl-assert (macher-context-p context) nil "CONTEXT must be a macher-context struct")
  ;; Normalize the path for consistent lookup.
  (let* ((normalized-path (macher--normalize-path path))
         ;; Check if we already have contents for this file.
         (existing-contents (assoc normalized-path (macher-context-contents context))))
    (if existing-contents
        ;; Return the existing contents.
        (cdr existing-contents)
      ;; Handle file existence check.
      (if (not (file-exists-p normalized-path))
          ;; For non-existent files, store (nil . nil) in context and return it.
          (let ((context-contents (macher-context-contents context))
                (content-pair (cons nil nil)))
            ;; Add to context.
            (push (cons normalized-path content-pair) context-contents)
            (setf (macher-context-contents context) context-contents)
            content-pair)
        ;; For existing files, load the file content.
        (let* ((file-content
                (with-temp-buffer
                  (insert-file-contents normalized-path)
                  (buffer-substring-no-properties (point-min) (point-max))))
               (context-contents (macher-context-contents context))
               ;; Both original and new content start as the same.
               (content-pair (cons file-content file-content)))

          ;; Add to context.
          (push (cons normalized-path content-pair) context-contents)
          (setf (macher-context-contents context) context-contents)

          ;; Return the content pair.
          content-pair)))))

;;; Default Prompt Functions

(defun macher--implement-prompt (content)
  "Generate an implementation prompt for CONTENT in the current buffer."
  (let* ((workspace (macher-workspace))
         (filename (buffer-file-name))
         (relpath
          (when filename
            (file-relative-name filename (macher--workspace-root workspace))))
         (source-description
          (cond
           ;; No file associated with buffer.
           ((null filename)
            "")
           ;; Directory case.
           ((file-directory-p filename)
            (format "The request was sent from the workspace directory `%s`. " relpath))
           ;; Regular file case.
           (t
            (let* ((lang
                    (downcase
                     (if (stringp mode-name)
                         mode-name
                       (car mode-name)))))
              (format (concat
                       "The request was sent from the %s file `%s` in the workspace. "
                       "If the request text appears as a comment or placeholder in the file, "
                       "replace it with the actual implementation. ")
                      lang relpath))))))
    (format (concat
             "TASK: Implement the following request using workspace tools.\n\n"
             "INSTRUCTIONS:\n"
             "1. Read and understand the implementation request below\n"
             "2. Use the workspace tools to edit files as needed\n"
             "3. Create working, complete code that fulfills the request\n\n"
             "%s"
             "\n\nIMPLEMENTATION REQUEST:\n\n%s")
            source-description content)))

(defun macher--revise-prompt (content &optional patch-buffer)
  "Generate a prompt for revising based on CONTENT (revision instructions).

The contents of the PATCH-BUFFER (defaulting to the current workspace's
patch buffer) are included in the generated prompt."
  (let* ((patch-buffer (or patch-buffer (macher-patch-buffer)))
         (patch-content
          (if patch-buffer
              (with-current-buffer patch-buffer
                (buffer-substring-no-properties (point-min) (point-max)))
            ;; Doesn't make sense to call this without a patch.
            (user-error "No patch buffer found for revision"))))
    (format (concat
             "TASK: Revise your previous implementation based on new feedback.\n\n"
             "WHAT YOU NEED TO DO:\n"
             "1. Read the revision instructions below (if any)\n"
             "2. Review your previous patch and its original prompt\n"
             "3. Understand what needs to be changed or improved\n"
             "4. Create a NEW implementation that addresses the feedback\n"
             "5. Use the workspace editing tools to make the changes\n\n"
             "%s"
             "\n\n"
             "==================================\n"
             "YOUR PREVIOUS WORK (for reference)\n"
             "==================================\n\n"
             "%s")
            (if (and content (not (string-empty-p content)))
                (format "REVISION INSTRUCTIONS:\n%s\n\n" content)
              "")
            patch-content)))

(defun macher--patch-prepare-diff (context fsm callback)
  "Generate and add a diff to the current buffer for CONTEXT.
CONTEXT is the macher-context object.
FSM is the gptel-fsm struct for the request.
CALLBACK must be called when preparation is complete."
  (let ((diff-text (macher--generate-patch-diff context)))
    (insert diff-text)
    (funcall callback)))

(defun macher--patch-prepare-metadata (context fsm callback)
  "Add metadata to the current patch buffer content for CONTEXT.
CONTEXT is the macher-context object.
FSM is the gptel-fsm struct for the request.
CALLBACK must be called when preparation is complete."
  (let* ((workspace (macher-context-workspace context))
         (proj-name (macher--workspace-name workspace))
         (initial-text (buffer-string))
         ;; Generate a unique patch ID as a random 8-character alphanumeric string. The result here
         ;; isn't really important - it's just used to help the LLM link the prompt with its
         ;; eventual generated patch.
         (patch-id
          (let ((chars "abcdefghijklmnopqrstuvwxyz0123456789")
                (result ""))
            (dotimes (_ 8 result)
              (let ((idx (random (length chars))))
                (setq result (concat result (substring chars idx (1+ idx))))))))
         (info (gptel-fsm-info fsm))
         (data (plist-get info :data))
         (prompt (macher-context-prompt context))
         (header
          ;; Always start with the patch metadata header.
          (format "# Patch ID: %s\n# Project: %s\n" patch-id proj-name)))

    (goto-char (point-min))
    (insert header)

    ;; If the original patch text is empty, add a message.
    (when (string-empty-p initial-text)
      (insert "\n# No changes were made to any files.\n"))

    ;; Add the prompt as metadata if it exists.
    (goto-char (point-max))
    (when prompt
      (insert
       (concat
        ;; Generate a clear separator for the prompt.
        "\n# -----------------------------\n"
        (format "# PROMPT for patch ID %s:\n" patch-id)
        "# -----------------------------\n\n"
        ;; Add comment prefix to each line of the prompt.
        (replace-regexp-in-string "^" "# " prompt)
        "\n")))

    ;; Note: The original prompt is no longer stored in the context structure.
    ;; If prompt tracking is needed, it should be added to the context structure.

    (funcall callback)))

;;; Tool Management
;;
;; We use a potentially-too-clever overload of the :category field on gptel tool objects to allow
;; the 'macher-context' to be retrieved from existing tool definitions. Normally the :category is
;; expected to be a string, but since we're not adding tools to the global registry, it should
;; hopefully never be accessed elsewhere.
;;
;; This enables sharing of the 'macher-context' across all tools for a request, even when they're
;; added across multiple presets. It also allows for sensible merging of tools with request forms
;; like "@macher-ro @macher my query".

(defun macher--tool-context (tool)
  "Get the 'macher-context' for a TOOL created with `macher--make-tool'.
Returns nil if the tool was not created with `macher--make-tool'."
  (let ((category (gptel-tool-category tool)))
    (when (macher-context-p category)
      category)))

(defun macher--make-tool (context &rest slots)
  "Make an ephemeral tool associated with a macher CONTEXT.
SLOTS supports the same keyword arguments as `gptel-make-tool' except
for :category.

The context can be retrieved later using `macher--tool-context'."

  ;; Extract the original function from slots.
  (let ((slots (plist-put (copy-sequence slots) :category context)))
    ;; Use the internal constructor to avoid interacting with the global registry at all.
    (apply #'gptel--make-tool slots)))

(defun macher--merge-tools (preset tools-function)
  "Merge macher tools generated by TOOLS-FUNCTION into the PRESET spec.

PRESET is a gptel spec containing at least the :tools and
:prompt-transform-functions keys, containing the existing values for
these fields in the environment at this point.

TOOLS-FUNCTION is a function that accepts two arguments: CONTEXT and
MAKE-TOOL-FUNCTION. The correct `macher-context' will be extracted from
any existing macher tools, or created if no matching tools are present.

If any tools with the same name as the generated ones are present in
the preset :tools, they will be removed.

Returns the updated preset spec."
  (let ((context))
    (if-let ((matching-tool
              (cl-find-if
               #'macher--tool-context (seq-filter #'gptel-tool-p (plist-get preset :tools)))))
      (setq context (macher--tool-context matching-tool))
      (let ((make-context-result (macher--make-context-for-preset preset)))
        (setq context (car make-context-result))
        (setq preset (cdr make-context-result))))
    (if context
        (let* ((existing-tools (seq-filter #'gptel-tool-p (plist-get preset :tools)))
               ;; Create a make-tool function using the correct context.
               (make-tool-function (apply-partially #'macher--make-tool context))
               ;; Generate new tools using the tools function.
               (new-tools (funcall tools-function context make-tool-function))
               ;; Get names of new tools for removal from existing tools.
               (new-tool-names (mapcar #'gptel-tool-name new-tools))
               ;; Remove existing tools with same names as new tools.
               (filtered-existing-tools
                (cl-remove-if
                 (lambda (tool) (member (gptel-tool-name tool) new-tool-names)) existing-tools))
               ;; Combine filtered existing tools with new tools.
               (updated-tools (append filtered-existing-tools new-tools)))
          ;; Return updated preset with new tools.
          (plist-put (copy-sequence preset) :tools updated-tools))

      ;; If no context (i.e. no current workspace, print a warning and return the preset as-is.
      (display-warning '(macher requests) "No macher workspace found for the current buffer")
      preset)))

(defun macher--make-context-for-preset (preset)
  "Create a 'macher-context' and add a termination handler to the PRESET spec.

PRESET is a plist containing at least the existing
:prompt-transform-functions for this preset; the new preset will append
to this list.

Returns a cons cell (macher-context . updated-preset).

If no workspace can be determined from the current buffer, returns (nil
. preset) with no modifications"

  (if-let ((workspace (macher-workspace)))
    (let* ((context
            (macher--make-context
             :workspace workspace
             :process-request-function macher-process-request-function))
           (prompt-transforms (plist-get preset :prompt-transform-functions))
           (termination-handler
            (lambda (fsm)
              "Process termination of a macher request."
              (let ((process-fn (macher-context-process-request-function context)))
                (when process-fn
                  (funcall process-fn context fsm)))))

           ;; Transform to capture the prompt and store it on the context.
           (prompt-transform-store-prompt
            (lambda (_fsm) (setf (macher-context-prompt context) (buffer-string))))

           ;; Transform to preload files from the gptel context into the macher context.
           (prompt-transform-preload-context
            (lambda (_fsm) (macher--load-gptel-context-files (gptel-context--collect) context)))

           ;; Transform to hook into the request lifecycle with our callback.
           (prompt-transform-termination-handler
            (lambda (callback fsm)
              (macher--partial-prompt-transform-add-termination-handler
               termination-handler callback fsm)))

           ;; Global transforms list including our custom ones. Note that our custom transforms
           ;; won't actually modify the prompt - they're simply there to capture information and
           ;; hook into the request lifecycle.
           (updated-prompt-transforms
            (append
             prompt-transforms
             (list
              prompt-transform-store-prompt
              prompt-transform-preload-context
              prompt-transform-termination-handler))))
      ;; Return the context and the updated preset.
      (cons
       context
       (plist-put (copy-sequence preset) :prompt-transform-functions updated-prompt-transforms)))
    (cons nil preset)))

;;; Preset Definitions

(defun macher--functional-preset (spec-function &rest baseline-spec)
  "Get a spec for a gptel preset whose values are set dynamically.

- SPEC-FUNCTION is a function which returns a partial spec based on the
  current environment. This spec should only contain keys that directly
  correspond to 'gptel-*' variables, e.g. :use-tools,
  :prompt-transform-functions, etc.

- BASELINE-SPEC is the baseline preset specification (minus :pre and :post
  functions). Keys returned by SPEC-FUNCTION should appear in this baseline
  definition.

For the moment, this function is a bit of a hack, and dependent on some
implementation details of the gptel preset system."
  ;; Local value to store the spec between invocations of the created pre/post functions. We assume
  ;; that these functions will always be called in succession, i.e. if :pre is called, :post will be
  ;; called synchronously afterwards, with no other calls to :pre in between.
  ;;
  ;; We (ab)use the fact that, between the :pre and :post functions, `gptel--apply-preset' will use
  ;; its provided setter to set the values of all the 'gptel-*' variables associated with the
  ;; baseline spec. Although the provided setter won't always be a normal `set', it appears that it
  ;; will always work such that using normal `set' in the :post function will behave correctly. In
  ;; some cases, `gptel--apply-preset' will run with a buffer-local setter, after which we can use
  ;; `set' and still only affect the buffer-local value. In the setter associated with the transient
  ;; menu's oneshot setting, a post-response hook is added to reset to the original value, so it
  ;; doesn't matter if we change it again in :post.
  ;;
  ;; This might be brittle if `gptel--apply-preset' is used with other setters, though.
  ;;
  ;; Hopefully something like https://github.com/karthink/gptel/pull/940 will get merged so we can
  ;; do this in a less hacky way.
  (let ((current-spec nil)
        ;; Extract all keys from the baseline spec to validate against
        (baseline-keys
         (let (keys)
           (cl-loop for (key _value) on baseline-spec by #'cddr do (push key keys))
           keys)))

    (let ((initial-spec (copy-sequence baseline-spec)))
      ;; Set the spec's :pre to a function which gets the spec based on the current environment and
      ;; stores it.
      (setq initial-spec
            (plist-put
             initial-spec
             :pre
             (lambda ()
               "Get the current dynamic spec and store it."
               (setq current-spec (funcall spec-function))
               current-spec)))

      ;; Set the spec's :post function, which runs `set' for each of the keys in the spec stored by
      ;; :pre. the allowed KEYS array.
      (setq initial-spec
            (plist-put
             initial-spec
             :post
             (lambda ()
               "Apply the stored dynamic spec values to gptel variables."
               (when current-spec
                 (cl-loop
                  for (key value) on current-spec by #'cddr do
                  ;; Verify that the key is in the allowed baseline keys list.
                  (unless (memq key baseline-keys)
                    (error "Key %s not in allowed baseline keys list %s" key baseline-keys))
                  ;; Convert key to gptel variable name and set it.
                  (let ((gptel-var (intern-soft (concat "gptel-" (substring (symbol-name key) 1)))))
                    (when (and gptel-var (boundp gptel-var))
                      (set gptel-var value)))))
               ;; Reset for cleanliness.
               (setq current-spec nil))))

      initial-spec)))

(defvar macher--presets-alist
  `((macher
     .
     ,(macher--functional-preset
       #'macher--preset-default
       :description "Send macher workspace context + tools to read files and propose edits"
       :prompt-transform-functions nil
       :tools nil
       :use-tools nil))
    (macher-ro
     .
     ,(macher--functional-preset
       #'macher--preset-ro
       :description "Send macher workspace context + tools to read files"
       :prompt-transform-functions nil
       :tools nil
       :use-tools nil))
    (macher-notools
     .
     ,(macher--functional-preset
       #'macher--preset-notools
       :description "Send macher workspace context without tools"
       :prompt-transform-functions nil)))
  "Alist of definitions for macher presets.

Entries have the form (NAME . KEYS). NAME is the name to use (by
default) when installing presets globally with `macher-install'. KEYS
are the keys to pass to `gptel-make-preset'.

This list will be used to install presets globally, and also to create
ephemeral presets when using workspace actions. These ephemeral presets
allow us to reuse gptel's preset functionality when making requests
programmatically, without relying on certain names or config existing in
the global state.")

(defun macher--with-preset (preset callback)
  "Run the CALLBACK with the macher PRESET applied.

PRESET is a key from the 'macher--presets-alist', or a raw preset spec
like \\='(:use-tools t). Note that this function does not accept a name
from the global gptel registry - it's for cases where you want to use a
well-defined preset independent of how gptel is currently configured.

CALLBACK takes no arguments."
  (let ((spec
         (if (symbolp preset)
             (cdr (assq preset macher--presets-alist))
           preset)))
    (let ((gptel-known-presets nil))
      ;; Adapted from `gptel-with-preset'. Currently that macro sets all symbol values to nil before
      ;; applying the preset (since it doesn't expect dynamic values), but we want to pull in
      ;; existing values, so we can extend rather than overwrite things like 'gptel-tools'.
      (let* ((preset-syms (gptel--preset-syms spec))
             ;; Get the current values of all variables affected by the preset.
             (preset-sym-values (mapcar #'symbol-value preset-syms)))
        (cl-progv preset-syms preset-sym-values
          (gptel--apply-preset spec)
          (funcall callback))))))

(defun macher--preset-default ()
  "Set up the default macher preset with full editing capabilities."
  ;; Start with the read-only preset.
  (let ((preset (macher--preset-ro)))
    ;; Add editing tools to the existing setup.
    (setq preset (macher--merge-tools preset macher-edit-tools-function))
    preset))

(defun macher--preset-ro ()
  "Set up the read-only macher preset."
  ;; Start with the notools preset.
  (let ((preset (copy-sequence (macher--preset-notools))))
    (setq preset (plist-put preset :tools gptel-tools))
    ;; Add reading tools to the existing setup.
    (setq preset (macher--merge-tools preset macher-read-tools-function))
    ;; Make sure tool use is enabled (but don't overwrite it if it's set to anything non-nil, like
    ;; 'force).
    (setq preset (plist-put preset :use-tools (or gptel-use-tools t)))
    preset))

(defun macher--preset-notools ()
  "Set up the macher preset without tools (context only)."
  ;; The global prompt transforms appear to get overwritten to nil while first applying presets. In
  ;; this case, we need to restore the defaults to make sure the -add-context transform and any
  ;; other preset transforms get applied.
  (let ((prompt-transforms
         (or gptel-prompt-transform-functions
             '(gptel--transform-apply-preset gptel--transform-add-context))))
    `(:prompt-transform-functions
      ,(append prompt-transforms (list #'macher--prompt-transform-add-context)))))

(defun macher--prompt-transform-add-context (callback fsm)
  "A gptel prompt transformer to add context from the current workspace.

CALLBACK and FSM are as described in the
'gptel-prompt-transform-functions' documentation.

Adds the result of the 'macher-context-string-function' to the prompt,
in the same place as the default gptel context as specified by
'gptel-use-context'."
  (when macher-context-string-function
    (when-let* (
                ;; plist containing information about the upcoming request.
                (info (gptel-fsm-info fsm))
                ;; Buffer where the request is being sent.
                (buffer (plist-get info :buffer))
                (_ (buffer-live-p buffer))
                (workspace-string
                 (with-current-buffer buffer
                   (funcall macher-context-string-function (gptel-context--collect)))))
      (gptel-context--wrap-in-buffer workspace-string)))
  (funcall callback))

;; Note it's not really necessary in this to use the async form with a CALLBACK argument, but this
;; three-argument signature makes it a (maybe) bit more obvious/foolproof that this can't just be
;; used as a transform function directly.
(defun macher--partial-prompt-transform-add-termination-handler (handler callback fsm)
  "A partializable prompt transform to call HANDLER when the request terminates.

To use this as an actual prompt transform, wrap it in a two-argument
lambda. Note that gptel checks the function arity to determine the call
signature, so things like `apply-partially' can't be used as they don't
preserve arity.

The HANDLER will receive one argument when the request terminates
successfully or otherwise:

- FSM: the FSM for the request (the same one passed to this function).
  This can be used to extract a more specific termination reason - for
  example, standard gptel requests will end up in the the 'DONE or 'ERRS
  state, which can be extracted from the FSM.

The CALLBACK and FSM arguments are as described in the
'gptel-prompt-transform-functions' documentation."
  (let* (
         ;; An alist of states mapped to potential next states. See 'gptel-request--transitions'.
         (transitions (gptel-fsm-table fsm))

         ;; An alist of states mapped to their handler functions.
         (handlers (gptel-fsm-handlers fsm))

         ;; Find all states that appear as a potential next state in one of the rules.
         (all-states
          (cl-remove-duplicates
           (append
            (mapcar #'car transitions)
            (cl-mapcan (lambda (entry) (mapcar #'cdr (cdr entry))) transitions))))

         ;; Filter down to states that either don't appear as keys, or appear as keys but have no
         ;; possible next states - that is, states which can't transition to any other states.
         ;;
         ;; In the case of 'gptel-request--transitions', this will be '(DONE ERRS). This is
         ;; currently the only transitions list used by gptel, but this logic ensures we catch
         ;; termination even when using a custom list.
         (terminal-states
          (cl-remove-if-not
           (lambda (state)
             (let ((entry (assq state transitions)))
               ;; Terminal if: no entry exists, or entry exists but has no transitions.
               (or (null entry) (null (cdr entry)))))
           all-states))

         ;; The handler to add when the FSM reaches a terminal state.
         (termination-handler
          (lambda (fsm)
            "macher FSM termination handler."
            (funcall handler fsm)))

         ;; Alist whose keys are the terminal states, and values are their new lists of handlers.
         (terminal-state-handlers
          (cl-loop
           for state in terminal-states for existing-entry = (assq state handlers) collect
           (if existing-entry
               (cons state (append (cdr existing-entry) (list termination-handler)))
             (cons state (list termination-handler)))))

         ;; Create a new handlers list for this FSM.
         (augmented-handlers
          (append
           ;; Copy existing non-terminal handlers.
           (cl-remove-if (lambda (entry) (member (car entry) terminal-states)) handlers)
           ;; Add our terminal state handlers.
           terminal-state-handlers)))

    ;; Update the handlers list.
    (setf (gptel-fsm-handlers fsm) augmented-handlers))

  (funcall callback))

(defun macher--gptel-request (callback &optional prompt &rest keys)
  "Send PROMPT to the LLM and invoke the CALLBACK when the request terminates.

The CALLBACK will receive two arguments when the request terminates
successfully or otherwise:

- EXIT-CODE: nil if the request terminated via a normal FSM
  flow (including terminations due to an error response), or the symbol
  'abort if the request was aborted.

- FSM: the FSM for the request (the same one passed to this function).
  This can be used to extract a more specific termination reason - for
  example, standard gptel requests will end up in the the 'DONE or 'ERRS
  state, which can be extracted using `gptel-fsm-state'.

described in `macher--partial-prompt-transform-add-termination-handler'.

This is a thin wrapper around `gptel-request'. The PROMPT and KEYS will
be passed directly to `gptel-request'. Note that the CALLBACK parameter
here is different than the :callback key accepted by `gptel-request',
which might also be included."
  (let* (
         ;; Get the transforms that were already passed (possibly nil). Note gptel generally expects
         ;; callers to pass `gptel-prompt-transform-functions' for this argument, but we don't make
         ;; any assumptions here.
         (transforms (plist-get keys :transforms))

         ;; Prompt transform which hooks into the lifecycle and causes the callback to be invoked at
         ;; the end of the request lifecycle.
         (prompt-transform
          (when callback
            (lambda (cb fsm)
              (macher--partial-prompt-transform-add-termination-handler
               (lambda (fsm)
                 "Invoke the user-provided callback after the FSM reached a terminal state"
                 (funcall callback nil fsm))
               cb fsm))))

         ;; Transforms list including our callback transform.
         (updated-transforms
          (append
           transforms
           (when prompt-transform
             (list prompt-transform)))))
    (setq keys (plist-put (copy-sequence keys) :transforms updated-transforms))

    (when-let* (
                ;; Send the request and get the state machine.
                (fsm (apply #'gptel-request prompt keys))

                ;; Extract the actual gptel callback for handling responses. By default this will
                ;; generally be gptel--insert-response or gptel-curl--stream-insert-response.
                (info (gptel-fsm-info fsm))
                (fsm-callback (plist-get info :callback))

                ;; Wrap the callback to catch the abort signal.
                (wrapped-callback
                 (lambda (response &rest rest)
                   "Invoke the user-provided callback after the request is aborted.
Then pass arguments through to the original callback."
                   (when (eq response 'abort)
                     (funcall callback 'abort fsm))
                   (apply fsm-callback response rest))))
      (setf (gptel-fsm-info fsm) (plist-put info :callback wrapped-callback))
      fsm)))

;;; Core Functions

;;;###autoload
(defun macher-install (&optional names)
  "Register macher presets with gptel.

Once presets are registered, you can use macher functionality in any
gptel request using the \\=\"@preset\" syntax, for example:

  @macher Add an eslint config to this project.

This function registers three presets:

- @macher: Send contextual information about the workspace + tools to
  read files and propose edits. At the end of the request, update the
  patch buffer.

- @macher-ro: Send contextual information about the workspace + tools to
  read files.

- @macher-notools: Send contextual information about the workspace, but
  no tools.

NAMES is an optional alist of name overrides, whose entries are
like (PRESET . NAME). PRESET is the preset's standard name symbol (e.g.
\\='macher-notools) and NAME is the symbol to actually register with
gptel. You can also pass a nil NAME to disable registering a preset
globally. For example:

\\='((macher . m) (macher-ro . mr) (macher-notools . nil))"

  (dolist (preset-entry macher--presets-alist)
    (let* ((preset-name (car preset-entry))
           (preset-config (cdr preset-entry))
           ;; Check if there's a name override in the NAMES alist.
           (override-entry (assq preset-name names))
           (actual-name
            (if override-entry
                (cdr override-entry)
              preset-name)))
      ;; Only register the preset if actual-name is not nil.
      (when actual-name
        (apply #'gptel-make-preset actual-name preset-config)))))

;;;###autoload
(defun macher-action (action &optional user-input callback)
  "Execute an ACTION with optional USER-INPUT.

When called interactively, prompts the user to select an action from
available actions.

ACTION should be a symbol defined in `macher-actions-alist'.
If USER-INPUT is not provided, the user will be prompted based on the
action configuration.

If CALLBACK is provided, it will be called when the action completes.
The callback will receive three arguments, the same as functions in
'macher-after-action-functions': ERROR (nil on success, or an error
description on failure), EXECUTION (the 'macher-action-execution' object
for the action), and FSM (the `gptel-fsm' object for the request)."
  (interactive (let* ((actions (mapcar #'car macher-actions-alist))
                      (action-names (mapcar #'symbol-name actions))
                      (selected-name (completing-read "Action: " action-names nil t))
                      (selected-action (intern selected-name)))
                 (list selected-action)))

  ;; Run the action dispatch hook in the source buffer.
  (run-hooks 'macher-action-dispatch-hook)

  ;; Prompt to save any unsaved buffers.
  (save-some-buffers nil (lambda () (and (buffer-file-name) (buffer-modified-p))))

  (let* ((action-config (alist-get action macher-actions-alist))
         (prompt-text (or (plist-get action-config :prompt) "Prompt: "))
         (preset (or (plist-get action-config :preset) 'macher))
         (transform-fn (plist-get action-config :transform)))

    (unless action-config
      (error "Unknown action: %s" action))

    (unless transform-fn
      (error "No :transform function specified for action %s" action))

    ;; Get user input if not provided.
    (let* ((input
            (or user-input
                (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string prompt-text))))
           ;; Transform the input into the final prompt.
           (final-prompt (funcall transform-fn input))
           ;; Get the action buffer for this workspace.
           (action-buffer (macher-action-buffer nil t))
           ;; Store the source buffer (where the action was initiated).
           (source-buffer (current-buffer))
           ;; Create execution object to pass to callbacks.
           (execution
            (macher--make-action-execution
             :action action
             :input input
             :prompt final-prompt
             :buffer action-buffer
             :source source-buffer))
           ;; Create a callback wrapper that includes the action hooks.
           (request-callback
            (lambda (exit-code fsm)
              (let* ((state (gptel-fsm-state fsm))
                     (error
                      (cond
                       ;; If we have a non-nil exit code (i.e. 'abort), just use it as the error.
                       (exit-code)
                       ;; If the FSM is in an errored state, extract the error text.
                       ((eq state 'ERRS)
                        (let* ((info (gptel-fsm-info fsm))
                               (error (plist-get info :error))
                               (http-msg (plist-get info :status))
                               (error-type (plist-get error :type))
                               (error-msg (plist-get error :message)))
                          (or error-msg (format "%s: %s" error-type http-msg))))
                       ;; Otherwise, consider the request successful. In practice the state should
                       ;; always be 'DONE here.
                       (t
                        nil))))

                ;; Call the original callback if provided. Don't change the buffer for this, to
                ;; avoid any potential issues with killed buffers.
                (when (functionp callback)
                  (funcall callback error execution fsm))

                ;; Run the after-action hook if the action buffer is still live.
                (when (buffer-live-p action-buffer)
                  (with-current-buffer action-buffer
                    (run-hook-with-args 'macher-after-action-functions error execution fsm)))))))

      ;; Run the before-action hook from the shared buffer.
      (with-current-buffer action-buffer
        (run-hook-with-args 'macher-before-action-functions execution))
      ;; It's possible for the before-action hook to change the current buffer, so re-enter the
      ;; shared buffer explicitly.
      (with-current-buffer action-buffer
        (macher--with-preset
         preset
         (lambda ()
           ;; Just like `gptel-send', but with a prompt specified directly, and with the callback on
           ;; termination. Use the potentially modified prompt and context from the execution
           ;; object.
           (macher--gptel-request request-callback
                                  (macher-action-execution-prompt execution)
                                  :context (macher-action-execution-context execution)
                                  ;; Insert at the end of the buffer.
                                  :position (point-max)
                                  :stream gptel-stream
                                  :transforms gptel-prompt-transform-functions
                                  :fsm (gptel-make-fsm :handlers gptel-send--handlers))))))))

;;;###autoload
(defun macher-abort (&optional buf)
  "Abort any active macher action associated with buffer BUF.

In other words, abort gptel requests running in the macher
action buffer associated with the `macher-workspace' for BUF.

If a callback was provided to the original request, it will be called
with the 'abort symbol as the error parameter.

BUF defaults to the current buffer if not specified."
  (interactive)
  (with-current-buffer (or buf (current-buffer))
    (when-let* ((action-buffer (macher-action-buffer))
                (_ (buffer-live-p action-buffer)))
      (gptel-abort action-buffer))))

;;; Convenience methods for built-in actions.

;;;###autoload
(defun macher-implement (&optional instructions callback)
  "Propose a patch to implement INSTRUCTIONS.

When called interactively, get INSTRUCTIONS from the selected region or
prompt for them in the minibuffer.

When called non-interactively, INSTRUCTIONS can be a string or nil.
If nil, the user will be prompted.

If CALLBACK is provided, it will be called when the implementation
process completes. The callback will receive three arguments: ERROR (nil
on success, a string error description on failure, or the symbol 'abort
if the request was aborted), EXECUTION (the `macher-action-execution'
object for the action), and FSM (the `gptel-fsm' object for the request)."
  (interactive)
  (macher-action 'implement instructions callback))

;;;###autoload
(defun macher-revise (&optional instructions callback)
  "Propose a revision to a patch based on INSTRUCTIONS.

When called interactively, get instructions from the selected region or
prompt for them in the minibuffer.

When called non-interactively, INSTRUCTIONS can be a string or nil.
If nil, the user will be prompted.

The function identifies the patch buffer associated with the current buffer's
project and sends a revision request based on the provided instructions.

If CALLBACK is provided, it will be called when the revision process
completes. The callback will receive three arguments: ERROR (nil on
success, or an error description on failure), EXECUTION (the
`macher-action-execution' object for the action), and FSM (the
`gptel-fsm' object for the request)."
  (interactive)
  (macher-action 'revise instructions callback))

;;;###autoload
(defun macher-discuss (&optional question callback)
  "Send a QUESTION about the current project to the LLM.

When called interactively, get the question from the selected region or
prompt for it in the minibuffer.

When called non-interactively, QUESTION can be a string or nil. If nil,
the user will be prompted.

This creates a conversational request without proposing any changes
to the codebase.

If CALLBACK is provided, it will be called when the discussion
completes. The callback will receive three arguments: ERROR (nil on
success, or an error description on failure), EXECUTION (the
`macher-action-execution' object for the action), and FSM (the
`gptel-fsm' object for the request)."
  (interactive)
  (macher-action 'discuss question callback))

;; Local variables:
;; elisp-autofmt-load-packages-local: ("cl-macs")
;; end:

(provide 'macher)
;;; macher.el ends here
