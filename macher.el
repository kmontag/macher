;;; macher.el --- LLM implementation toolset -*- lexical-binding: t -*-

;; Author: Kevin Montag
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (gptel "0.9.8.5"))
;; Keywords: convenience, gptel, llm
;; URL: https://github.com/kmontag/macher

;;; Commentary:
;; macher provides a toolset for project-aware LLM file editing based on gptel.
;;
;; Key features:
;; - Send implementation requests using `macher-implement'
;; - View proposed changes as diff-mode-friendly patches
;; - Request revisions to patches with `macher-revise'
;; - File-aware project context management
;;
;; Conceptually, when making a request, macher provides the LLM with a "workspace" containing files
;; from the current project (or just the current file if not in a project), and tools to read/edit
;; files in the workspace. Edits are not persisted to disk, but rather stored in the request's
;; associated `macher-context' object, where they can later be used to generate patches.
;;
;; The main interactive entrypoints are `macher-implement' and `macher-revise'. When using those
;; functions, any changes made to the workspace by the LLM will be rendered to a patch and displayed
;; in a diff buffer. You can customize this behavior using the `macher-patch-text-function' and
;; `macher-patch-ready-function'.
;;
;; You can also build your own workflows using `macher-send' (which sets up the workspace/tools and
;; makes a request, but doesn't do any handling of the results) and `macher-edit' (which does the
;; same, and also displays a patch at the end of the request).

;;; Code:

(require 'gptel)
(require 'gptel-context)
(require 'cl-lib)

;;; Customization

(defgroup macher nil
  "Project-aware LLM implementation toolset."
  :group 'convenience
  :prefix "macher-")

(defcustom macher-workspace-hook '(macher--project-workspace macher--file-workspace)
  "Hook to determine the workspace for the current buffer.

Each function in this hook is called with no arguments in the current
buffer until one returns a non-nil workspace cons cell of the form
\(TYPE . ID).

Functions should return nil if they cannot determine a workspace for
the current buffer, allowing other functions in the hook to try.

Built-in workspace functions:
- `macher--project-workspace': Uses project.el to find project workspaces
- `macher--file-workspace': Falls back to single-file workspaces

The output and patch buffers will be shared among files in the same
workspace (i.e. same type and ID).

To add custom workspace detection, add functions to this hook.

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
     (:root-function
      macher--project-root
      :name-function macher--project-name
      :files-function macher--project-files))
    (file
     .
     (:root-function
      file-name-directory
      :name-function file-name-nondirectory
      :files-function list)))
  "Alist mapping workspace types to their defining functions.

Each entry is of the form (TYPE . PLIST) where TYPE is a symbol
representing the workspace type and PLIST contains the following keys:

:root-function - Function to get the workspace root, i.e. the base
                 directory for unified diffs and the \"root\" for relative
                 filenames provided to tools.
:name-function - Function to get a descriptive name for the workspace.
:files-function - Function to get files in the workspace. Returned paths
                  can be absolute, or relative to the workspace root.

All functions receive the workspace ID as a string argument, i.e. the
cdr of a workspace cons cell whose type matches the associated alist key.

To add a new workspace type, add an entry to this alist and update
'macher-workspace-hook' to detect it."
  :type '(alist :key-type symbol :value-type (plist :key-type keyword :value-type function))
  :group 'macher)

(defcustom macher-workspace-string-function #'macher--workspace-string
  "Function for generating workspace information strings during macher requests.

This function receives two arguments:
- CONTEXTS: the list of contexts as passed to the context string function
- MACHER-CONTEXT: the `macher-context' object for the current request

The function should return a workspace information string that will be
added to the request context.

The default function adds information about the current workspace (e.g.
the project being edited) including file listings and context indicators."
  :type '(function :tag "Workspace string function")
  :group 'macher)

(defcustom macher-implement-prompt-function #'macher--implement-prompt
  "Function for generating prompts for implementation requests.
The function should take a string argument containing the selected
content to implement. It will be called with the current buffer equal
to the buffer where the implementation was invoked."
  :type '(function :tag "Prompt function")
  :group 'macher)

(defcustom macher-revise-prompt-function #'macher--revise-prompt
  "Function for generating prompts for revision requests.
The function should take a string argument containing the patch to
revise, and an optional second argument with revision instructions. It
will be called with the current buffer equal to the patch buffer."
  :type '(function :tag "Prompt function")
  :group 'macher)

(defcustom macher-patch-text-function #'macher--patch-text
  "Function for generating patch text content.
This function is called with one argument:

- CONTEXT: the macher-context object

The function should return the text to be placed in the patch buffer."
  :type 'function
  :group 'macher)

(defcustom macher-patch-ready-function #'macher--patch-ready
  "Function called when a patch buffer is fully populated and ready for display.
This function is called with the current buffer set to the patch buffer
and receives one argument:

- CONTEXT: the 'macher-context' object

This function can perform any final setup of the patch buffer, such as
setting buffer-local variables, enabling specific modes, and/or
displaying the buffer.

Note: The patch-ready-function value is captured at request time, so you
can set it in e.g. a let-binding to modify it for a single request."
  :type 'function
  :group 'macher)

(defcustom macher-patch-ready-hook nil
  "Hook run after a patch buffer is ready for display.
Functions in this hook are called with no arguments after
'macher-patch-ready-function' has been called. The current buffer is set
to the patch buffer.

This hook is convenient for performing additional setup operations on
the patch buffer that aren't dependent on the request context, and/or
operations that you want to run regardless of modifications to the
'macher-patch-ready-function'."
  :type 'hook
  :group 'macher)

(defcustom macher-before-send-function #'ignore
  "Function called before sending a request to the LLM.
This function is called with one argument:

- CONTEXT: the 'macher-context' object

The function can perform any pre-request setup operations, such as
modifying the context, adding additional data, or performing
validation checks."
  :type 'function
  :group 'macher)

(defcustom macher-before-send-hook nil
  "Hook run before sending a request to the LLM.
Functions in this hook are called with no arguments after
'macher-before-send-function' has been called.

This hook is convenient for performing additional pre-request setup
operations that don't require direct access to the context object,
and/or operations that you want to run regardless of modifications to
the 'macher-before-send-function'."
  :type 'hook
  :group 'macher)

(defcustom macher-output-buffer-setup-hook '(macher--output-buffer-setup)
  "Hook run when creating new macher output buffers.
Functions in this hook are called with no arguments in the newly created
output buffer. The buffer will already have its workspace set up and
'default-directory' configured.

The default function enables 'gptel-mode' and sets up key bindings and
other buffer-local settings appropriate for macher output buffers."
  :type 'hook
  :group 'macher)

;;; Constants

(defconst macher--buffer-type-context "context"
  "Buffer type constant for macher context buffers.")

;;; Variables

(defvar-local macher--workspace nil
  "Workspace information for the current macher session.
When nil (the default), uses `macher-workspace-hook' to
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

(cl-defstruct
 (macher-context (:constructor macher--make-context))
 "Structure holding context information for an implementation request.

BUFFERS is an alist of (filename . (orig-buffer . new-buffer)) pairs.
WORKSPACE is the workspace information (same format as 'macher--workspace').
PROMPT is the original prompt sent to the LLM.
CALLBACK is a function to call when the request completes (with error or nil).
DATA is an arbitrary user-defined data object.
TOOL-CATEGORY is a unique category name for the tools created for this request."
 (buffers nil) (workspace nil) (prompt nil) (callback nil) (data nil) (tool-category nil))

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
    (error "Cannot determine files for a null project ID"))
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
runs the functions in `macher-workspace-hook' until one returns a
non-nil workspace cons cell. If no function returns a workspace,
it signals an error."
  (with-current-buffer (or buffer (current-buffer))
    (or macher--workspace
        (run-hook-with-args-until-success 'macher-workspace-hook)
        (error
         (concat
          "Cannot determine workspace: "
          "no function in macher-workspace-hook returned a workspace")))))

(defun macher--workspace-root (workspace)
  "Get the workspace root for WORKSPACE.
WORKSPACE is a cons cell (TYPE . ID) where TYPE is a workspace type.
Returns the root directory path for the workspace."
  (let* ((workspace-type (car workspace))
         (workspace-id (cdr workspace))
         (type-config (alist-get workspace-type macher-workspace-types-alist))
         (root-fn (plist-get type-config :root-function))
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
         (name-fn (plist-get type-config :name-function)))
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
         (files-fn (plist-get type-config :files-function))
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

(defun macher--workspace-string (contexts macher-context)
  "Generate workspace information string for CONTEXTS and MACHER-CONTEXT.

CONTEXTS is the list of contexts as passed to the context string
function, potentially nil.

MACHER-CONTEXT is the macher-context object for the current request.

Returns a workspace information string to be added to the request context."
  (let* ((workspace (macher-context-workspace macher-context))
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
            (run-hook-with-args-until-success 'macher-workspace-hook)))
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

(defun macher--workspace-hash (workspace)
  "Generate a short unique hash for WORKSPACE.
This can be used, for example, to ensure unique buffer names per
workspace, without needing to put the full path in the buffer name."
  (let* ((workspace-type (car workspace))
         (workspace-id (cdr workspace))
         (hash-input (secure-hash 'sha256 (concat (format "%s" workspace-type) workspace-id)))
         (chars "abcdefghijklmnopqrstuvwxyz0123456789")
         (result ""))
    ;; Take the starting characters and map them to our character set.
    (dotimes (i 4 result)
      (let* ((hex-char (aref hash-input i))
             (idx
              (mod
               (if (>= hex-char ?a)
                   (- hex-char ?a -10)
                 (- hex-char ?0))
               (length chars))))
        (setq result (concat result (substring chars idx (1+ idx))))))))

(defun macher--output-buffer-setup ()
  "Default setup function for macher output buffers.
This function enables 'gptel-mode' and configures the buffer for
macher-specific behavior including disabling text input and
enabling auto-scrolling."
  ;; Enable gptel-mode for nice header and LLM interaction support.
  (unless (derived-mode-p 'gptel-mode)
    (gptel-mode 1))

  ;; Create a keymap that disables text input.
  (let ((map (make-sparse-keymap)))
    ;; Remove self-insert bindings to prevent buffer modification.
    (suppress-keymap map t)
    (use-local-map map))

  ;; Enable auto-scrolling during streaming responses using gptel's built-in mechanism.
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll nil t))

(defun macher--get-buffer (buffer-type &optional buffer create)
  "Get a macher buffer with BUFFER-TYPE associated with BUFFER.

BUFFER-TYPE can be any string or nil. The return value for a given
BUFFER-TYPE is shared across all files in the same workspace.

If CREATE is non-nil, create the buffer if it doesn't already exist.
Otherwise, return nil if the buffer doesn't already exist.

When BUFFER is nil, use the current buffer.

Returns a cons cell (BUFFER . CREATED-P) where BUFFER is the target
buffer and CREATED-P is t if the buffer was newly created, nil
otherwise."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((workspace (macher-workspace))
           (workspace-type (car workspace))
           (workspace-id (cdr workspace))
           (workspace-name (macher--workspace-name workspace))
           (buffer-type-segment
            (if buffer-type
                (format "-%s" buffer-type)
              ""))
           (buffer-name
            (format "*macher%s:%s@%s<%s>*"
                    buffer-type-segment
                    workspace-type
                    workspace-name
                    (macher--workspace-hash workspace)))
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
        (cons target-buffer created-p)))))

(defun macher--output-buffer (&optional buffer create)
  "Get the macher output buffer associated with BUFFER.

The output buffer is used for displaying LLM responses and is shared
across all files in the same workspace.

If CREATE is non-nil, create the buffer if it doesn't already exist.
Otherwise, return nil if the buffer doesn't already exist.

When BUFFER is nil, use the current buffer."
  (let ((result (macher--get-buffer nil buffer create)))
    (when result
      (let ((target-buffer (car result))
            (created-p (cdr result)))
        (when created-p
          (with-current-buffer target-buffer
            ;; Use gptel-default-mode as the major mode.
            (funcall gptel-default-mode)
            ;; Run the output buffer setup hook for macher-specific behavior.
            (run-hooks 'macher-output-buffer-setup-hook)))
        target-buffer))))

(defun macher--patch-buffer (&optional buffer create)
  "Get the macher patch buffer associated with BUFFER.

The patch buffer is used for displaying proposed changes as diffs and is
shared across all files in the same workspace.

If CREATE is non-nil, create the buffer if it doesn't already exist.
Otherwise, return nil if the buffer doesn't already exist.

When BUFFER is nil, use the current buffer."
  (let ((result (macher--get-buffer "patch" buffer create)))
    (when result
      (car result))))

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
            ;; If this context file is in the workspace, create implementation buffers for it.
            (when in-workspace-p
              (macher-context-buffers-for-file full-path macher-context))))))))

(defun macher--generate-tool-category ()
  "Generate a unique category name for macher workspace tools.
Returns a string like 'workspace-1234' where the last four characters
are random alphanumeric characters."
  (let ((chars "abcdef0123456789")
        (result "macher-workspace-"))
    ;; Generate 4 random alphanumeric characters.
    (dotimes (_ 4 result)
      (let ((idx (random (length chars))))
        (setq result (concat result (substring chars idx (1+ idx))))))))

(defun macher--cleanup-tools (category)
  "Clean up gptel tools for the given CATEGORY.
Removes all tools from `gptel--known-tools' and 'gptel-tools' that
have the specified category.

See `gptel-mcp-disconnect' for an example of something similar."
  (when (and category (stringp category))
    ;; The tools should never have been added to the global 'gptel-tools' (just the tools list for
    ;; the request), so we don't need to do any cleanup there. Just remove them from registry.
    (setf (alist-get category gptel--known-tools nil t #'equal) nil)))

;; The workspace tools roughly mirror the standard filesystem MCP interface, of which many LLMs
;; already have some understanding. See
;; https://github.com/modelcontextprotocol/servers/blob/main/src/filesystem/README.md.
(defun macher--tools (context)
  "List of tools to use during `macher-implement' with CONTEXT.

CONTEXT is a `macher-context' struct with slots for buffers, workspace info,
and original prompt. Using a struct makes it easier to pass
around and modify the context throughout the implementation process."
  (let* ((workspace (macher-context-workspace context))
         (workspace-type (car workspace))
         (workspace-id (cdr workspace))
         (workspace-name (macher--workspace-name workspace))
         (category (macher-context-tool-category context))
         ;; Shared function to resolve full paths from relative paths.
         (resolve-workspace-path
          (lambda (rel-path)
            "Get the full path for REL-PATH within the workspace.
Resolves paths according to workspace type.
Signals an error if the resolved path is invalid for the current workspace."
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

         ;; Shared helper to get or create implementation buffers for a file.
         (get-or-create-file-buffers
          (lambda (file-path)
            "Get or create implementation buffers for FILE-PATH, scoped to the current request.
Returns a cons cell (orig-buffer . new-buffer) of buffers for the file.
Also updates the context's :buffers alist."
            (let ((full-path (funcall resolve-workspace-path file-path)))
              (macher-context-buffers-for-file full-path context)))))
    (list
     (gptel-make-tool
      :name "edit_file_in_workspace"
      :category category
      :function
      `,(lambda (path edits &optional dry-run)
          "Edit file specified by PATH within the workspace.
EDITS is a vector of edit operations, each containing :oldText and :newText.
If DRY-RUN is non-nil, preview changes without applying them."
          (let* ((full-path (funcall resolve-workspace-path path))
                 ;; Get implementation buffers for this file.
                 (buffers (funcall get-or-create-file-buffers path))
                 (new-buffer (cdr buffers))
                 ;; Handle :json-false inputs for dry-run parameter.
                 (dry-run (and dry-run (not (eq dry-run :json-false)))))
            ;; Check if the file exists for editing.
            (if (not new-buffer)
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
                     (macher--edit-buffer new-buffer old-text new-text))
                   (setq applied-edits (1+ applied-edits))))

                (unless dry-run
                  nil)))))
      :description
      (concat
       "Make line-based edits to a text file within the workspace. "
       "Each edit replaces exact line sequences with new content. "
       "Returns null on success, or an error message if any edit fails.")
      :confirm nil
      :include t
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

     (gptel-make-tool
      :name "write_file_in_workspace"
      :category category
      :function
      `,(lambda (path content)
          "Create a new file or completely overwrite an existing file with CONTENT.
Use with caution as it will overwrite existing files without warning."
          (let* ((full-path (funcall resolve-workspace-path path))
                 ;; Get implementation buffers for this file.
                 (buffers (funcall get-or-create-file-buffers path))
                 (new-buffer (cdr buffers)))
            ;; If the file doesn't exist, create a new buffer for it.
            (unless new-buffer
              (let* ((basename (file-name-nondirectory full-path))
                     (new-buffer-name
                      (generate-new-buffer-name
                       (concat " *macher-implement-file:new@" basename "*")))
                     (editable-buffer (get-buffer-create new-buffer-name)))
                ;; Set the new buffer in the context.
                (macher-context-set-new-buffer-for-file full-path editable-buffer context)
                (setq new-buffer editable-buffer)))
            ;; Write the content to the buffer.
            (with-current-buffer new-buffer
              (save-excursion
                (erase-buffer)
                (insert content)
                nil))))
      :description
      (concat
       "Create a new file or completely overwrite an existing file in the workspace. "
       "Use with caution as it will overwrite existing files without warning. "
       "Handles text content with proper encoding.")
      :confirm nil
      :include t
      :args
      '((:name "path" :type string :description "Path to the file, relative to workspace root")
        (:name "content" :type string :description "Complete new content for the file")))

     (gptel-make-tool
      :name "read_file_in_workspace"
      :category category
      :function
      `,(lambda (path)
          "Read the contents of a file specified by PATH within the workspace."
          (let* ((full-path (funcall resolve-workspace-path path))
                 ;; Get implementation buffers for this file.
                 (buffers (funcall get-or-create-file-buffers path))
                 (new-buffer (cdr buffers)))
            ;; Check if the file exists for reading.
            (if (not new-buffer)
                (error (format "File '%s' not found in workspace" path))
              ;; Read the buffer content.
              (with-current-buffer new-buffer
                (buffer-substring-no-properties (point-min) (point-max))))))
      :description
      (concat
       "Read file contents from the workspace. "
       "Returns the current contents of a file. Use this ONLY when you need to see a file "
       "that wasn't included in the initial context, or to verify changes after editing. "
       "Do NOT use this to re-read files whose contents were already provided in the request context.")
      :confirm nil
      :include t
      :args '((:name "path" :type string :description "Path to the file, relative to workspace root")))


     (gptel-make-tool
      :name "move_file_in_workspace"
      :category category
      :function
      `,(lambda (source destination)
          "Move or rename files within the workspace.
SOURCE and DESTINATION are both relative to the workspace root.
If the destination exists, the operation will fail."
          (let* ((source-full-path (funcall resolve-workspace-path source))
                 (dest-full-path (funcall resolve-workspace-path destination))
                 ;; Get implementation buffers for the source file.
                 (source-buffers (funcall get-or-create-file-buffers source))
                 (source-new-buffer (cdr source-buffers)))
            ;; Check if the source file exists.
            (unless source-new-buffer
              (error (format "Source file '%s' not found in workspace" source)))
            ;; Check if destination already exists.
            (let ((dest-buffers (macher-context-buffers-for-file dest-full-path context)))
              (when (cdr dest-buffers)
                (error (format "Destination '%s' already exists" destination))))
            ;; Create new buffer for destination with source content.
            (let* ((dest-basename (file-name-nondirectory dest-full-path))
                   (dest-buffer-name
                    (generate-new-buffer-name
                     (concat " *macher-implement-file:moved@" dest-basename "*")))
                   (dest-buffer (get-buffer-create dest-buffer-name)))
              ;; Copy content from source to destination.
              (with-current-buffer dest-buffer
                (insert-buffer-substring source-new-buffer))
              ;; Set destination buffer in context.
              (macher-context-set-new-buffer-for-file dest-full-path dest-buffer context)
              ;; Mark source for deletion.
              (macher-context-set-new-buffer-for-file source-full-path nil context)
              ;; Kill the source buffer.
              (kill-buffer source-new-buffer)
              nil)))
      :description
      (concat
       "Move or rename files within the workspace."
       "Can move files between directories and rename them in a single operation. "
       "If the destination exists, the operation will fail. Works across different "
       "directories and can be used for simple renaming within the same directory.")
      :confirm nil
      :include t
      :args
      '((:name "source" :type string :description "Source path relative to workspace root")
        (:name
         "destination"
         :type string
         :description "Destination path relative to workspace root")))

     (gptel-make-tool
      :name "delete_file_in_workspace"
      :category category
      :function
      `,(lambda (rel-path)
          "Delete a file specified by REL-PATH within the workspace.
The file must exist and will be marked for deletion in the patch."
          (let* ((full-path (funcall resolve-workspace-path rel-path))
                 ;; Get implementation buffers for this file to properly record deletion.
                 (buffers (funcall get-or-create-file-buffers rel-path))
                 (editable-buffer (cdr buffers)))
            ;; Check if the file exists before deleting.
            (if (not editable-buffer)
                (error (format "File '%s' not found in workspace" rel-path))
              ;; For deletion, set the new buffer to nil to indicate deletion.
              (macher-context-set-new-buffer-for-file full-path nil context)
              ;; Kill the buffer since we no longer need it.
              (kill-buffer editable-buffer)
              nil)))
      :description
      (concat
       "Delete a file from the workspace. "
       "Permanently removes the file. The file must exist in the workspace.")
      :confirm nil
      :include t
      :args '((:name "path" :type string :description "Path to the file, relative to workspace root"))))))

(defun macher--edit-buffer (buffer old-string new-string)
  "In BUFFER, replace OLD-STRING with NEW-STRING.

Return nil if the replacement was successful, or an error string if it
was not."
  (with-current-buffer buffer
    (let ((buffer-name (buffer-name))
          (case-fold-search nil))
      (save-excursion
        ;; Handle empty old-string specially.
        (if (string-empty-p old-string)
            (if (= (buffer-size) 0)
                ;; If buffer is empty, insert the new string.
                (progn
                  (insert new-string)
                  nil)
              ;; If buffer is not empty and old-string is empty, throw an error.
              (error "Cannot replace empty string in non-empty buffer"))
          ;; Normal case: old-string is not empty.
          (goto-char (point-min))
          (let ((count 0))
            (while (search-forward old-string nil t)
              (setq count (1+ count)))
            (if (= count 0)
                (error (format "Could not find text to replace in buffer %s" buffer-name))
              (if (> count 1)
                  (error
                   (format "Found %d matches for the text to replace in buffer %s"
                           count
                           buffer-name))
                (goto-char (point-min))
                (search-forward old-string)
                (replace-match new-string t t)
                nil))))))))

(defun macher--write (fsm text)
  "Write TEXT to the buffer for the `macher-send' request managed by FSM."
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

(defun macher--context-cleanup (context)
  "Clean up buffers and tools associated with CONTEXT.
CONTEXT is a `macher-context' struct."
  (let ((buffers (macher-context-buffers context))
        (category (macher-context-tool-category context)))
    ;; Clean up buffers.
    (dolist (entry buffers)
      (let ((orig-buffer (car (cdr entry)))
            (new-buffer (cdr (cdr entry))))
        ;; Kill the original buffer if it exists.
        (when orig-buffer
          (kill-buffer orig-buffer))
        ;; Kill the new buffer if it exists.
        (when new-buffer
          (kill-buffer new-buffer))))
    ;; Clean up tools from this category.
    (macher--cleanup-tools category)))

(defun macher--fsm-cleanup (fsm)
  "Clean up buffers and other resources for the `macher-send' request managed by FSM."
  (let* ((info (gptel-fsm-info fsm))
         (context (plist-get info :context)))
    (macher--context-cleanup context)))

(defun macher--generate-patch-diff (context)
  "Generate a raw diff to populate the patch buffer.
CONTEXT is the 'macher-context' object. Returns the generated diff text."
  (let* ((buffers-alist (macher-context-buffers context))
         (workspace (macher-context-workspace context))
         (base-dir (macher--workspace-root workspace))
         (result ""))

    ;; Use the system diff command to generate a unified diff for each file.
    ;; Sort the buffers list by filename for consistent patch ordering.
    (dolist (entry (sort (copy-sequence buffers-alist) (lambda (a b) (string< (car a) (car b)))))
      (let* ((filename (car entry))
             (buffers (cdr entry))
             (orig-buffer (car buffers))
             (new-buffer (cdr buffers))
             ;; Get the path relative to the base directory.
             (rel-path (file-relative-name filename base-dir))
             ;; Check if file has actually changed.
             (file-changed-p
              (if (and orig-buffer new-buffer)
                  ;; Both buffers exist, compare their contents.
                  (not
                   (string=
                    (with-current-buffer orig-buffer
                      (buffer-substring-no-properties (point-min) (point-max)))
                    (with-current-buffer new-buffer
                      (buffer-substring-no-properties (point-min) (point-max)))))
                ;; File is being created or deleted.
                t)))

        ;; Only generate diff if the file has actually changed.
        (when file-changed-p
          (let ((temp-orig (make-temp-file "gptel-diff-orig"))
                (temp-new (make-temp-file "gptel-diff-new")))

            ;; Write source buffer (or empty file for new files).
            (if orig-buffer
                (with-current-buffer orig-buffer
                  (write-region (point-min) (point-max) temp-orig nil 'silent))
              (with-temp-buffer
                (write-region (point-min) (point-max) temp-orig nil 'silent)))

            ;; Write new buffer contents or create empty file for deletions.
            (if new-buffer
                (with-current-buffer new-buffer
                  (write-region (point-min) (point-max) temp-new nil 'silent))
              ;; If new-buffer is nil, it indicates a file deletion.
              (with-temp-buffer
                (write-region (point-min) (point-max) temp-new nil 'silent)))

            ;; Generate diff and append to result.
            (with-temp-buffer
              ;; Add the standard git diff header, which allows diff-mode to create new files.
              (insert (format "diff --git a/%s b/%s\n" rel-path rel-path))

              ;; Use diff to generate a unified patch with the correct file path.
              (when (or orig-buffer new-buffer)
                (call-process "diff"
                              nil t nil "-u" "--label"
                              (if orig-buffer
                                  (concat "a/" rel-path)
                                ;; Use /dev/null to denote file creations.
                                "/dev/null")
                              "--label"
                              (if new-buffer
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

(defun macher--add-patch-metadata (text context)
  "Add metadata and prompt information to the patch text.
TEXT is the current patch text.
CONTEXT is the macher-context object.
Returns the text to be passed to the next function in the hook."
  (let* ((workspace (macher-context-workspace context))
         (base-dir (macher--workspace-root workspace))
         (proj-name (macher--workspace-name workspace))
         ;; Generate a unique patch ID as a random 8-character alphanumeric string.
         (patch-id
          (let ((chars "abcdefghijklmnopqrstuvwxyz0123456789")
                (result ""))
            (dotimes (_ 8 result)
              (let ((idx (random (length chars))))
                (setq result (concat result (substring chars idx (1+ idx))))))))
         (result
          ;; Always start with the patch metadata header.
          (format "# Patch ID: %s\n# Generated: %s | Project: %s\n"
                  patch-id
                  (format-time-string "%Y-%m-%d %H:%M:%S")
                  proj-name)))

    ;; If the original patch text is empty, add a message; otherwise add the diff content.
    (if (string-empty-p text)
        (setq result (concat result "# No changes were made to any files."))
      (setq result (concat result text)))

    ;; Add the current prompt to the patch, if available.
    (when-let ((prompt (macher-context-prompt context)))
      (setq result
            (concat
             result
             ;; Generate a clear separator for the prompt.
             "\n# -----------------------------\n"
             (format "# PROMPT for patch ID %s:\n" patch-id)
             "# -----------------------------\n\n"
             ;; Add comment prefix to each line of the prompt.
             (replace-regexp-in-string "^" "# " prompt)
             "\n")))

    result))

(defun macher--handle-done (fsm)
  "Process completion of a gptel implementation request managed by FSM.
Writes a finished message and calls the request callback."
  (macher--write fsm (format "\n--> [%s] Finished\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))

  ;; Extract the callback and call it.
  (let* ((info (gptel-fsm-info fsm))
         (context (plist-get info :context))
         (callback (macher-context-callback context)))
    ;; Call the callback function if provided.
    (when (functionp callback)
      (funcall callback nil context))))

(defun macher--handle-error (fsm)
  "Process error events for a gptel implementation request managed by FSM.
Extracts error information from the FSM's info plist and displays it in
the output buffer."
  (let* ((info (gptel-fsm-info fsm))
         (error (plist-get info :error))
         (http-msg (plist-get info :status))
         (error-type (plist-get error :type))
         (error-msg (plist-get error :message))
         (context (plist-get info :context))
         (callback (and context (macher-context-callback context)))
         (error-value (or error-msg (format "%s: %s" error-type http-msg))))
    (macher--write
     fsm
     (format "\n--> [%s] Error: (%s) `%s` %s"
             (format-time-string "%Y-%m-%d %H:%M:%S")
             http-msg
             error-type
             error-msg))

    ;; Call the callback function with the error if provided.
    (when (functionp callback)
      (funcall callback error-value context))))

;;; Context Buffer Management

(defun macher-context-set-new-buffer-for-file (path buffer context)
  "Set the new buffer for PATH to BUFFER in the given CONTEXT.
PATH should be an absolute file path within the project.
BUFFER is the buffer to use for the file's content, or nil if the file
is being deleted.
CONTEXT must be a `macher-context' struct.

Updates the CONTEXT's :buffers alist with the new buffer mapping."
  (cl-assert (macher-context-p context) nil "CONTEXT must be a macher-context struct")

  ;; Get or create buffer pair for this file.
  (let* ((buffers (macher-context-buffers-for-file path context))
         (normalized-path (macher--normalize-path path))
         (context-buffers (macher-context-buffers context)))
    ;; Update the editable buffer (the cdr of the pair).
    (setcdr buffers buffer)

    ;; Update the entry in the context's buffers alist for persistence.
    (setf (macher-context-buffers context)
          (cons
           (cons normalized-path (cons (car buffers) buffer))
           (assoc-delete-all normalized-path context-buffers)))

    ;; Return the updated buffer pair.
    buffers))

(defun macher-context-buffers-for-file (path context)
  "Get or create implementation buffers for PATH, scoped to the current CONTEXT.

PATH should be an absolute file path within the project.
CONTEXT must be a `macher-context' struct.

Returns a cons cell (orig-buffer . new-buffer) of buffers for the file.
If the orig-buffer is nil, the file is being created; if the new-buffer
is nil, the file is being deleted.

Also updates the CONTEXT's :buffers alist if the relevant entry was not
yet present.

Only creates buffers if the file exists; otherwise returns (nil . nil)."
  (cl-assert (macher-context-p context) nil "CONTEXT must be a macher-context struct")
  ;; Normalize the path for consistent lookup.
  (let* ((normalized-path (macher--normalize-path path))
         ;; Check if we already have buffers for this file.
         (existing-buffers (assoc normalized-path (macher-context-buffers context))))
    (if existing-buffers
        ;; Return the existing buffers.
        (cdr existing-buffers)
      ;; Handle file existence check.
      (if (not (file-exists-p normalized-path))
          ;; For non-existent files, store (nil . nil) in context and return it.
          (let ((context-buffers (macher-context-buffers context))
                (buffer-pair (cons nil nil)))
            ;; Add to context.
            (push (cons normalized-path buffer-pair) context-buffers)
            (setf (macher-context-buffers context) context-buffers)
            buffer-pair)
        ;; For existing files, create both original and editable buffers.
        (let* ((basename (file-name-nondirectory normalized-path))
               (read-only-buffer-name
                (generate-new-buffer-name (concat " *macher-implement-file:orig@" basename "*")))
               (editable-buffer-name
                (generate-new-buffer-name
                 (concat " *macher-implement-file:modified@" basename "*")))
               (read-only-buffer (get-buffer-create read-only-buffer-name))
               (editable-buffer (get-buffer-create editable-buffer-name))
               (context-buffers (macher-context-buffers context)))

          ;; Initialize the read-only buffer.
          (with-current-buffer read-only-buffer
            (insert-file-contents normalized-path)
            (set-buffer-modified-p nil)
            (read-only-mode))

          ;; Initialize the editable buffer.
          (with-current-buffer editable-buffer
            (insert-file-contents normalized-path)
            (set-buffer-modified-p nil))

          ;; Add to context.
          (push (cons normalized-path (cons read-only-buffer editable-buffer)) context-buffers)
          (setf (macher-context-buffers context) context-buffers)

          ;; Return the implementation buffers.
          (cons read-only-buffer editable-buffer))))))

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

(defun macher--revise-prompt (patch &optional content)
  "Generate a prompt for revising the given PATCH text.

The PATCH should include the previous prompt in the comments. If CONTENT
is provided, it contains revision instructions to insert above the
previous patch."
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
          (if content
              (format "REVISION INSTRUCTIONS:\n%s\n\n" content)
            "")
          patch))

(defun macher--patch-text (context)
  "Generate the text content for a patch buffer.
CONTEXT is the macher-context object.
Returns the generated patch text."
  (macher--add-patch-metadata (macher--generate-patch-diff context) context))

(defun macher--patch-ready (context)
  "Set up the patch buffer with appropriate modes and settings.
CONTEXT is the macher-context object."

  ;; Enable diff-mode if needed.
  (unless (derived-mode-p 'diff-mode)
    (diff-mode)

    ;; If we're just now enabling diff-mode, i.e. if we're performing initial setup for the
    ;; buffer, make it read-only. Otherwise, the existing read-only status will be respected.
    (read-only-mode 1))

  ;; Re-detect patch type (i.e. 'git) now that the buffer has been populated.
  (diff-setup-buffer-type)

  ;; Display the buffer.
  (display-buffer (current-buffer)))

;;; Core Functions

(defun macher-send (prompt &optional callback)
  "Send PROMPT to the LLM for implementation and process the result.
This function handles the LLM request lifecycle for implementation
requests, including setting up the context, creating an output buffer,
configuring tools, and registering appropriate handlers for processing
the response. It does not generate or display a patch.

If CALLBACK is provided, it will be called when the process completes with
two arguments:
- ERROR: nil on success, an error message string on failure, or the symbol
  'abort if the request was aborted via `macher-abort'
- CONTEXT: the macher-context object associated with this request"
  (let*
      ((workspace (macher-workspace))
       ;; Create the context as a struct.
       (context
        (macher--make-context
         :buffers nil
         :workspace workspace
         :prompt prompt
         :callback callback
         :tool-category (macher--generate-tool-category)))
       (output-buffer (macher--output-buffer nil t))

       ;; We need information about the gptel context to generate rich workspace information. To
       ;; avoid using private gptel methods/variables to retrieve gptel context info, just capture
       ;; contexts when they're passed to the 'gptel-context-string-function'. Note this function
       ;; will never be invoked (and the variable will remain nil) if there's nothing in the
       ;; context.
       (current-gptel-contexts nil)
       (orig-context-string-function gptel-context-string-function)
       (gptel-context-string-function
        ;; Use the async form (two arguments), which we can translate either into the sync or async
        ;; form depending on the original function's signature.
        (lambda (callback contexts)
          ;; Create workspace buffers immediately for the files in the gptel context, so that if
          ;; they're edited while the request is running, their initial contents don't get out of
          ;; sync with the contents included in the context string.
          (macher--load-gptel-context-files contexts context)

          ;; Store the contexts for this request so they can be used by our workspace transformer.
          (setq current-gptel-contexts contexts)

          ;; Call the original context string function with appropriate arity.
          ;; See `gptel-context--wrap' for examples of handling both sync and async forms.
          (if (= (car (func-arity orig-context-string-function)) 2)
              ;; Async form: takes callback and contexts.
              (funcall orig-context-string-function callback contexts)
            ;; Sync form: takes only contexts, call callback with result.
            (funcall callback (funcall orig-context-string-function contexts)))))

       ;; Temporarily enable tools and extend gptel-tools with a request-specific toolset.
       (gptel-use-tools t)
       (gptel-tools (append gptel-tools (macher--tools context)))

       ;; Custom transformer to add workspace information to the context. See e.g.
       ;; `gptel--transform-add-context' for an example of the expected arguments and behavior.
       (workspace-transformer
        (lambda (callback fsm)
          (let* ((info (gptel-fsm-info fsm))
                 (data-buf (plist-get info :data))
                 (workspace-string
                  (funcall macher-workspace-string-function
                           current-gptel-contexts
                           (plist-get info :context))))
            (when workspace-string
              (with-current-buffer data-buf
                ;; This appends to the system or user message according to 'gptel-use-context', just
                ;; like `gptel--transform-add-context'. It would be nice if we could do this without
                ;; referencing a gptel private method, but the logic is nontrivial and better to
                ;; avoid re-implementing.
                (gptel-context--wrap-in-buffer workspace-string)))
            (funcall callback)))))

    ;; Call the before-send function and run the before-send hooks.
    (funcall macher-before-send-function context)
    (run-hooks 'macher-before-send-hook)

    (display-buffer output-buffer)

    (with-current-buffer output-buffer
      ;; Ensure the buffer is visually scrolled all the way down.
      (when-let ((window (get-buffer-window output-buffer 'visible)))
        (set-window-point window (point-max)))

      ;; Make sure the cursor is correctly positioned to start inserting content.
      (end-of-buffer)

      (gptel-request
       prompt
       :stream gptel-stream
       :context context
       ;; We need to explicitly include prompt transforms with the request so that the context
       ;; string gets added. Also append our workspace information transformer.
       :transforms (append gptel-prompt-transform-functions (list workspace-transformer))
       :fsm
       (gptel-make-fsm
        ;; Build handlers list from `gptel-send--handlers', adding macher-specific
        ;; functions to DONE and ERRS states for additional runtime information
        ;; and processing the final implementation results.
        :handlers
        (let ((handlers (copy-alist gptel-send--handlers)))
          ;; Add macher-specific handlers to DONE state
          (when-let ((done-entry (assq 'DONE handlers)))
            (setcdr
             done-entry
             (append (cdr done-entry) (list #'macher--handle-done #'macher--fsm-cleanup))))
          ;; Add macher-specific handlers to ERRS state
          (when-let ((errs-entry (assq 'ERRS handlers)))
            (setcdr
             errs-entry
             (append (cdr errs-entry) (list #'macher--handle-error #'macher--fsm-cleanup))))
          handlers))
       :callback
       ;; The stream-event callback seems to be the only place that abort events can be picked up,
       ;; so we need to provide it explicitly. See `gptel--insert-response' for a description of the
       ;; arguments.
       (lambda (response info &optional raw)
         (let* ((stream (plist-get info :stream))
                (buffer (plist-get info :buffer))
                ;; The callback if we hadn't provided a :callback argument.
                (default-cb
                 (if (and stream gptel-use-curl)
                     ;; The callback that would be set by `gptel-curl-get-response'.
                     #'gptel-curl--stream-insert-response
                   ;; The callback that would be set by `gptel--url-get-response'.
                   #'gptel--insert-response)))

           ;; Call the default callback.
           (funcall default-cb response info raw)

           ;; Show abort events in the buffer and handle cleanup.
           (when (and (eq response 'abort) (buffer-live-p buffer))
             (with-current-buffer buffer
               (end-of-buffer)
               (insert (format "\n--> [%s] Aborted\n\n" (format-time-string "%Y-%m-%d %H:%M:%S"))))

             ;; Call user callback with abort signal.
             (let* ((context (plist-get info :context))
                    (callback (and context (macher-context-callback context))))
               (when (functionp callback)
                 (funcall callback 'abort context))

               ;; Clean up the implementation buffers.
               (macher--context-cleanup context)))))))))

(defun macher-edit (prompt &optional callback)
  "Send PROMPT to the LLM and generate a patch from the results.
This function uses `macher-send' to process the prompt, then generates
a unified patch from the changes and displays it in a dedicated buffer.

If CALLBACK is provided, it will be called when the process completes.
See `macher-send' for details about callback behavior."
  ;; Store the patch functions and request buffer locally for use in the callback.
  (let ((text-function macher-patch-text-function)
        (ready-function macher-patch-ready-function)
        (request-buffer (current-buffer)))
    (macher-send prompt
                 (lambda (error context)
                   ;; If error is non-nil, just pass it to the callback.
                   (if error
                       (when (functionp callback)
                         (funcall callback error context))
                     ;; On success, use the context to generate the patch.
                     (let*
                         (
                          ;; Get or create the patch buffer for the original request's workspace.
                          ;; Use the captured request-buffer to ensure we get the correct workspace.
                          (patch-buffer (macher--patch-buffer request-buffer t))
                          ;; Generate the patch text.
                          (patch-text (funcall text-function context)))

                       ;; Populate the patch buffer.
                       (with-current-buffer patch-buffer
                         ;; Save read-only status to restore later.
                         (let ((was-read-only buffer-read-only))
                           ;; Temporarily disable read-only mode to update the buffer.
                           (when was-read-only
                             (read-only-mode -1))
                           (erase-buffer)
                           (insert (or patch-text ""))
                           (when was-read-only
                             (read-only-mode 1)))

                         ;; Move to the beginning of the buffer.
                         (goto-char (point-min))

                         ;; Use the captured patch-ready function to set up the buffer.
                         (funcall ready-function context)

                         ;; Run the patch-ready hooks.
                         (run-hooks 'macher-patch-ready-hook))

                       ;; Call the callback function if provided.
                       (when (functionp callback)
                         (funcall callback nil context))))))))

;;; Interactive Functions

;;;###autoload
(defun macher-implement (instructions &optional callback)
  "Propose a patch to implement INSTRUCTIONS.
When called interactively, get INSTRUCTIONS from the selected region or
prompt for them in the minibuffer.

When called non-interactively, INSTRUCTIONS must be a string.

If CALLBACK is provided, it will be called when the implementation
process completes. See `macher-send' for details about callback behavior."
  (interactive (list
                (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string "To implement: "))))
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (user-error "Current buffer has no associated file" (buffer-name))

      (unless (stringp instructions)
        (error "INSTRUCTIONS must be a string"))

      ;; Prompt to save any unsaved buffers.
      (save-some-buffers nil (lambda () (and (buffer-file-name) (buffer-modified-p))))

      (let* ((prompt (funcall macher-implement-prompt-function instructions)))
        (macher-edit prompt callback)))))

;;;###autoload
(defun macher-revise (instructions &optional callback)
  "Propose a revision to a patch based on INSTRUCTIONS.
When called interactively, get instructions from the selected region or
prompt for them in the minibuffer.

When called non-interactively, INSTRUCTIONS must be a string.

The function identifies the patch buffer associated with the current buffer's
project and sends a revision request based on the provided instructions.

If CALLBACK is provided, it will be called when the revision process
completes. See `macher-send' for details about callback behavior."
  (interactive (list
                (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string "Revision instructions: "))))

  ;; Ensure instructions are a string.
  (unless (stringp instructions)
    (error "INSTRUCTIONS must be a string"))

  ;; Find the associated patch buffer.
  (let* ((patch-buffer (macher--patch-buffer)))

    (unless patch-buffer
      (user-error "No patch buffer associated with the current buffer"))

    ;; Extract patch content.
    (let* ((patch-content
            (with-current-buffer patch-buffer
              (buffer-substring-no-properties (point-min) (point-max))))
           ;; Generate the revision prompt.
           (prompt (funcall macher-revise-prompt-function patch-content instructions)))

      ;; Send the revision request to the LLM.
      (macher-edit prompt callback))))

;;;###autoload
(defun macher-abort (&optional buf)
  "Abort any active macher request associated with buffer BUF.
In other words, abort gptel requests running in the macher output buffer
according to `macher-workspace' for BUF.

If a callback was provided to the original request, it will be called
with the 'abort symbol as the error parameter. See `macher-send' for
details about callback behavior.

BUF defaults to the current buffer if not specified."
  (interactive)
  (when-let* ((output-buffer (macher--output-buffer buf))
              (_ (buffer-live-p output-buffer)))
    (gptel-abort output-buffer)))

(provide 'macher)
;;; macher.el ends here
