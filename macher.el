;;; macher.el --- LLM implementation toolset -*- lexical-binding: t -*-

;; Author: Kevin Montag
;; Version: 0.2.0
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
  (unless (and (stringp project-id) (file-directory-p project-id) (project-current nil project-id))
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
Returns the root directory path for the workspace."
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
    ;; Verify that the root is a real directory
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
        ;; Ensure all paths are absolute
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
         ;; Extract filenames from contexts for comparison.
         (context-filenames
          (let (filenames)
            (dolist (context contexts)
              (let ((source (car context)))
                (when (stringp source)
                  (push (file-name-nondirectory source) filenames))))
            filenames)))

    ;; Generate workspace description with context indicators.
    (when workspace-files
      (with-temp-buffer
        (insert "WORKSPACE CONTEXT:\n")
        (insert "=========================\n\n")
        (insert "IMPORTANT: This workspace contains files you can edit using workspace tools. ")
        (when (and contexts (> (length contexts) 0))
          (insert
           "Files marked with [*] below are already shown in the REQUEST CONTEXT above - "
           "you do NOT need to read them again."))
        (insert "\n\n")
        (insert (format "Workspace: %s\n" workspace-name))
        (insert
         (format "Description: In-memory editing environment for '%s' workspace. " workspace-name))
        (insert "Edit freely using workspace tools. ")
        (insert "No permissions required - this is a safe virtual editing space.\n\n")
        (insert "Files in workspace:\n---------\n")

        (let ((has-context-markers nil))
          (dolist (file-path (sort (copy-sequence workspace-files) #'string<))
            (let* ((rel-path (file-relative-name file-path (macher--workspace-root workspace)))
                   (filename (file-name-nondirectory file-path))
                   (in-context-p (member filename context-filenames))
                   (marker
                    (if in-context-p
                        "[*] "
                      "    ")))
              (when in-context-p
                (setq has-context-markers t))
              (insert (format "%s%s\n" marker rel-path))))

          (when has-context-markers
            (insert "\n[*] = File contents already provided in REQUEST CONTEXT above\n"))
          (buffer-string))))))

(defun macher--insert-file-string (path)
  "Output information about the file at PATH for the context string.

This behaves similarly to `gptel--insert-file-string', but it includes
additional information about the file's workspace and its relative path
from the workspace root.

You might want to use this as follows to add more detailed information
about files in the default gptel context string (though note the
internal gptel API is subject to change without warning):

  (advice-add #'gptel--insert-file-string
              :override #'macher--insert-file-string)

Hopefully at some point there will be a simple way to do this using
public-facing gptel functionality, without re-implementing the entire
context string generation."
  (let* ((file-dir (file-name-directory path))
         (workspace-info
          (with-temp-buffer
            (setq default-directory file-dir)
            (cl-some #'funcall macher-workspace-functions)))
         (workspace-path (cdr workspace-info))
         (relpath
          (if workspace-info
              (file-relative-name path workspace-path)
            (file-name-nondirectory path)))
         ;; Create description based on workspace context.
         (description
          (if workspace-info
              (format "In file `%s` in workspace `%s`:"
                      relpath
                      (macher--workspace-name workspace-info))
            (format "In file `%s`:" relpath))))
    ;; Insert the enhanced description.
    (insert description "\n\n```\n")
    ;; The rest is just copied from the original function.
    (insert-file-contents path)
    (goto-char (point-max))
    (insert "\n```\n")))

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
     ;; Basic UI: just hooks
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
         ;; Extract the first non-whitespace line from the prompt and truncate to fill-column
         (truncated-input
          (let* ((lines (split-string input "\n" t "[[:space:]]*"))
                 (first-line (or (car lines) ""))
                 ;; Calculate available space: total fill-column minus prefix, action, and spacing
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

    ;; Enter a selected-buffer context, so we preserve the currently-selected buffer after exiting
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
          (setq-local default-directory (file-truename base-dir)))))
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
                    (and workspace-root
                         (string-prefix-p
                          (file-truename workspace-root) (file-truename full-path))))))
            ;; If this context file is in the workspace, load implementation contents for it.
            (when in-workspace-p
              (macher-context--contents-for-file full-path macher-context))))))))

;; The workspace tools roughly mirror the standard filesystem MCP interface, of which many LLMs
;; already have some understanding. See
;; https://github.com/modelcontextprotocol/servers/blob/main/src/filesystem/README.md.

(defun macher--resolve-workspace-path (workspace rel-path)
  "Get the full path for REL-PATH within the WORKSPACE.
Resolves paths according to workspace type.
Signals an error if the resolved path is invalid for the current workspace."
  (let ((workspace-type (car workspace))
        (workspace-id (cdr workspace)))
    (unless (consp workspace)
      (error "Workspace must be a cons cell"))
    (if (eq workspace-type 'file)
        ;; Single-file workspace case.
        (let* ((allowed-file workspace-id))
          ;; In the single-file workspace, only allow operations on the specific file.
          (if (string= rel-path (file-name-nondirectory allowed-file))
              allowed-file
            (error
             "In a single-file workspace, only operations on '%s' are allowed, not '%s'"
             (file-name-nondirectory allowed-file)
             rel-path)))
      ;; General workspace case.
      (let ((workspace-root (macher--workspace-root workspace)))
        (let ((full-path (expand-file-name rel-path workspace-root))
              (true-root (file-truename workspace-root)))
          ;; Verify the resolved path is inside the workspace root.
          (unless (string-prefix-p true-root (file-truename full-path))
            (error "Path '%s' resolves outside the workspace" rel-path))
          full-path)))))

(defun macher--read-tools (context make-tool-function)
  "Generate read-only tools for workspace operations with CONTEXT.

CONTEXT is a `macher-context' struct with slots for workspace info.
MAKE-TOOL-FUNCTION is a function that takes the same arguments as
`gptel-make-tool' but must be used instead of `gptel-make-tool' to create tools.

Returns a list of read-only tools that allow the LLM to inspect files
in the workspace.

Tools are created using MAKE-TOOL-FUNCTION so they can be properly removed from
the global gptel registry when the request completes."
  (let* ((workspace (macher-context-workspace context))
         (workspace-type (car workspace))
         (workspace-id (cdr workspace))
         (workspace-name (macher--workspace-name workspace))
         (resolve-workspace-path (apply-partially #'macher--resolve-workspace-path workspace))

         ;; Shared helper to get or create implementation contents for a file.
         (get-or-create-file-contents
          (lambda (file-path)
            "Get or create implementation contents for FILE-PATH, scoped to the current request.
Returns a cons cell (orig-content . new-content) of strings for the file.
Also updates the context's :contents alist."
            (let ((full-path (funcall resolve-workspace-path file-path)))
              (macher-context--contents-for-file full-path context)))))
    (list
     (funcall
      make-tool-function
      :name "read_file_in_workspace"
      :function
      `,(lambda (path)
          "Read the contents of a file specified by PATH within the workspace."
          (let* ((full-path (funcall resolve-workspace-path path))
                 ;; Get implementation contents for this file.
                 (contents (funcall get-or-create-file-contents path))
                 (new-content (cdr contents)))
            ;; Check if the file exists for reading.
            (if (not new-content)
                (error (format "File '%s' not found in workspace" path))
              ;; Return the content directly.
              new-content)))
      :description
      (concat
       "Read file contents from the workspace. "
       "Returns the current contents of a file. Use this ONLY when you need to see a file "
       "that wasn't included in the initial context, or to verify changes after editing. "
       "Do NOT use this to re-read files whose contents were already provided in the request context.")
      :confirm nil
      :include nil
      :args
      '((:name "path" :type string :description "Path to the file, relative to workspace root"))))))

(defun macher--edit-tools (context make-tool-function)
  "Generate file editing tools for workspace operations with CONTEXT.

CONTEXT is a `macher-context' struct with slots for workspace info.
MAKE-TOOL-FUNCTION is a function that takes the same arguments as
`gptel-make-tool' but must be used instead of `gptel-make-tool' to create tools.

Returns a list of tools that allow the LLM to edit files in the workspace.

Tools are created using MAKE-TOOL-FUNCTION so they can be properly removed from
the global gptel registry when the request completes."
  (let* ((workspace (macher-context-workspace context))
         (workspace-type (car workspace))
         (workspace-id (cdr workspace))
         (workspace-name (macher--workspace-name workspace))
         (resolve-workspace-path (apply-partially #'macher--resolve-workspace-path workspace))
         ;; Shared helper to get or create implementation contents for a file.
         (get-or-create-file-contents
          (lambda (file-path)
            "Get or create implementation contents for FILE-PATH, scoped to the current request.
Returns a cons cell (orig-content . new-content) of strings for the file.
Also updates the context's :contents alist."
            (let ((full-path (funcall resolve-workspace-path file-path)))
              (macher-context--contents-for-file full-path context))))
         ;; Wrapper function that sets the dirty-p flag before calling the actual tool function.
         (wrap-edit-fn
          (lambda (tool-fn)
            "Wrap TOOL-FN to set the dirty-p flag before calling it."
            (lambda (&rest args)
              "Set dirty-p flag and call the wrapped tool function."
              ;; Set the dirty flag to indicate changes are being made.
              (setf (macher-context-dirty-p context) t)
              ;; Call the original tool function.
              (apply tool-fn args)))))
    (list
     (funcall
      make-tool-function
      :name "edit_file_in_workspace"
      :function
      (funcall
       wrap-edit-fn
       (lambda (path edits &optional dry-run)
         "Edit file specified by PATH within the workspace.
EDITS is a vector of edit operations, each containing :oldText and :newText.
If DRY-RUN is non-nil, preview changes without applying them."
         (let* ((full-path (funcall resolve-workspace-path path))
                ;; Get implementation contents for this file.
                (contents (funcall get-or-create-file-contents path))
                (new-content (cdr contents))
                ;; Handle :json-false inputs for dry-run parameter.
                (dry-run (and dry-run (not (eq dry-run :json-false)))))
           ;; Check if the file exists for editing.
           (if (not new-content)
               (error (format "File '%s' not found in workspace" path))
             ;; Validate that edits is a vector, i.e. a JSON array. Ideally the argument should
             ;; have been sent as an actual array, but some LLMs seem to have trouble with this,
             ;; and instead send JSON strings which decode to an array. Allow both cases, as
             ;; although the string case technically violates the tool signature, we can still
             ;; handle it unambiguously.
             (unless (vectorp edits)
               (if (stringp edits)
                   ;; Try to decode JSON string to vector.
                   (condition-case nil
                       (let ((decoded
                              (json-parse-string edits :array-type 'vector :object-type 'plist)))
                         (if (vectorp decoded)
                             (setq edits decoded)
                           (error
                            "The 'edits' parameter must be an array, but the decoded JSON is not an array")))
                     (error
                      (error
                       "The 'edits' parameter must be an array of objects, or a valid JSON string representing an array")))
                 ;; Not a vector or string - invalid input.
                 (error
                  "The 'edits' parameter must be an array of objects, not %s" (type-of edits))))
             ;; Apply edits sequentially.
             (let ((applied-edits 0))
               (cl-loop
                for edit across edits do
                (let ((old-text (plist-get edit :oldText))
                      (new-text (plist-get edit :newText)))

                  (unless (and old-text new-text)
                    (error "Each edit must contain oldText and newText properties"))
                  (unless dry-run
                    (setq new-content (macher--edit-string new-content old-text new-text))
                    ;; Update the content in the context.
                    (macher-context--set-new-content-for-file full-path new-content context))
                  (setq applied-edits (1+ applied-edits))))

               (unless dry-run
                 nil))))))
      :description
      (concat
       "Make line-based edits to a text file within the workspace. "
       "Each edit replaces exact line sequences with new content. "
       "Returns null on success, or an error message if any edit fails.")
      :confirm nil
      :include nil
      :args
      '((:name "path" :type string :description "Path to the file, relative to workspace root")
        (:name
         "edits"
         :type array
         :description "Array of edit operations. Each element must be an object with oldText and newText properties."
         :items
         (:type
          object
          :properties
          (:oldText
           (:type string :description "Text to search for - must match exactly")
           :newText (:type string :description "Text to replace with"))
          :required ["oldText" "newText"]))
        (:name
         "dryRun"
         :type boolean
         :optional t
         :description "Preview changes without applying them (optional)")))

     (funcall make-tool-function
              :name "write_file_in_workspace"
              :function
              (funcall
               wrap-edit-fn
               (lambda (path content)
                 "Create a new file or completely overwrite an existing file with CONTENT.
Use with caution as it will overwrite existing files without warning."
                 (let* ((full-path (funcall resolve-workspace-path path)))
                   ;; Set the new content in the context (this will create the entry if needed).
                   (macher-context--set-new-content-for-file full-path content context)
                   nil)))
              :description
              (concat
               "Create a new file or completely overwrite an existing file in the workspace. "
               "Use with caution as it will overwrite existing files without warning. "
               "Handles text content with proper encoding.")
              :confirm nil
              :include nil
              :args
              '((:name
                 "path"
                 :type string
                 :description "Path to the file, relative to workspace root")
                (:name "content" :type string :description "Complete new content for the file")))

     (funcall
      make-tool-function
      :name "move_file_in_workspace"
      :function
      (funcall wrap-edit-fn
               (lambda (source destination)
                 "Move or rename files within the workspace.
SOURCE and DESTINATION are both relative to the workspace root.
If the destination exists, the operation will fail."
                 (let* ((source-full-path (funcall resolve-workspace-path source))
                        (dest-full-path (funcall resolve-workspace-path destination))
                        ;; Get implementation contents for the source file.
                        (source-contents (funcall get-or-create-file-contents source))
                        (source-new-content (cdr source-contents)))
                   ;; Check if the source file exists.
                   (unless source-new-content
                     (error (format "Source file '%s' not found in workspace" source)))
                   ;; Check if destination already exists.
                   (let ((dest-contents (macher-context--contents-for-file dest-full-path context)))
                     (when (cdr dest-contents)
                       (error (format "Destination '%s' already exists" destination))))
                   ;; Copy content from source to destination.
                   (macher-context--set-new-content-for-file
                    dest-full-path source-new-content context)
                   ;; Mark source for deletion.
                   (macher-context--set-new-content-for-file source-full-path nil context)
                   nil)))
      :description
      (concat
       "Move or rename files within the workspace."
       "Can move files between directories and rename them in a single operation. "
       "If the destination exists, the operation will fail. Works across different "
       "directories and can be used for simple renaming within the same directory.")
      :confirm nil
      :include nil
      :args
      '((:name "source" :type string :description "Source path relative to workspace root")
        (:name
         "destination"
         :type string
         :description "Destination path relative to workspace root")))

     (funcall make-tool-function
              :name "delete_file_in_workspace"
              :function
              (funcall
               wrap-edit-fn
               (lambda (rel-path)
                 "Delete a file specified by REL-PATH within the workspace.
The file must exist and will be marked for deletion in the patch."
                 (let* ((full-path (funcall resolve-workspace-path rel-path))
                        ;; Get implementation contents for this file to properly record deletion.
                        (contents (funcall get-or-create-file-contents rel-path))
                        (editable-content (cdr contents)))
                   ;; Check if the file exists before deleting.
                   (if (not editable-content)
                       (error (format "File '%s' not found in workspace" rel-path))
                     ;; For deletion, set the new content to nil to indicate deletion.
                     (macher-context--set-new-content-for-file full-path nil context)
                     nil))))
              :description
              (concat
               "Delete a file from the workspace. "
               "Permanently removes the file. The file must exist in the workspace.")
              :confirm nil
              :include nil
              :args
              '((:name
                 "path"
                 :type string
                 :description "Path to the file, relative to workspace root"))))))

(defun macher--edit-string (content old-string new-string)
  "In CONTENT string, replace OLD-STRING with NEW-STRING.

Return the new content string if the replacement was successful, or signal
an error if it was not."
  (let ((case-fold-search nil))
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
             (match-pos nil))
        ;; Count matches.
        (while (setq start (string-search old-string content start))
          (setq matches (1+ matches))
          (when (= matches 1)
            (setq match-pos start))
          (setq start (+ start (length old-string))))

        (cond
         ((= matches 0)
          (error "Could not find text to replace in content"))
         ((> matches 1)
          (error "Found %d matches for the text to replace in content" matches))
         (t
          ;; Exactly one match, perform the replacement.
          (concat
           (substring content 0 match-pos)
           new-string
           (substring content (+ match-pos (length old-string))))))))))

(defun macher--write (fsm text)
  "Write TEXT to the buffer for the action request managed by FSM."
  (let* ((info (gptel-fsm-info fsm))
         ;; Buffer and position to display a message.
         (gptel-buffer (plist-get info :buffer))
         (start-marker (plist-get info :position))
         (tracking-marker (or (plist-get info :tracking-marker) start-marker)))
    (with-current-buffer gptel-buffer
      (goto-char tracking-marker)
      (insert text)
      ;; Update stored point for insertion. Adapted from `gptel-curl--stream-insert-response'.
      (setq tracking-marker (set-marker (make-marker) (point)))
      (set-marker-insertion-type tracking-marker t)
      (plist-put info :tracking-marker tracking-marker))))

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
PATH should be an absolute file path within the project.
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
  ;; `gptel-with-preset' only works with presets that are globally registered. Momentarily add to
  ;; the known presets list while executing the callback.
  (let ((name (gensym "__macher-temp-preset-")))
    (unwind-protect
        (progn
          (let ((spec
                 (if (symbolp preset)
                     (cdr (assq preset macher--presets-alist))
                   preset)))
            (apply #'gptel-make-preset name spec))
          (eval `(gptel-with-preset ,name (funcall ,callback))))
      (setq gptel--known-presets (assq-delete-all name gptel--known-presets)))))

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
    (plist-put keys :transforms updated-transforms)

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
