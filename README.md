# macher

A project-aware LLM implementation toolset for Emacs, built on
[gptel](https://github.com/karthink/gptel).

https://github.com/user-attachments/assets/a6fd1dee-d0e8-4772-a192-5cb6ff8b0620

## What is macher?

**macher** provides multi-file, project-aware LLM editing capabilities within Emacs. It allows you
to send complex implementation requests and receive proposed changes as reviewable patches, which
can include changes to multiple files. You can then apply the changes and/or send them back to the
LLM for revision.

The LLM receives contextual information about the current project and file, as well as tools to read
additional project context.

macher is a lightweight Emacs-native take on the editing workflows of more full-featured tools like
[Aider](https://aider.chat/) or [Plandex](https://plandex.ai/). It works with any gptel backend that
supports tool use, and aims for a nonintrusive and easily-customizable UX. Your existing gptel
configuration gets extended when making macher requests, so you can use it alongside other tools.

The API is still very much under development and subject to change.

## Installation and configuration

Example configuration with elpaca + `use-package` integration:

```elisp
;; Make sure gptel is also installed and configured.
(use-package macher
  :ensure (:host github :repo "kmontag/macher")
  :hook
  ;; Add the current file to the gptel context when making macher requests.
  ;; (macher-before-send
  ;;  .
  ;;  (lambda ()
  ;;    (when-let* ((filename (buffer-file-name))
  ;;                ((not (file-directory-p filename))))
  ;;      (gptel-add-file filename))))
  :config
  ;; Adjust buffer positioning to taste.
  ;; (add-to-list
  ;;  'display-buffer-alist
  ;;  '("\\*macher:.*\\*"
  ;;    (display-buffer-in-side-window)
  ;;    (side . bottom)))
  ;; (add-to-list
  ;;  'display-buffer-alist
  ;;  '("\\*macher-patch:.*\\*"
  ;;    (display-buffer-in-side-window)
  ;;    (side . right)))

  ;; Customize patch display action. The 'macher-context' struct
  ;; contains data from the current request, including the contents of
  ;; any files that were edited.
  ;; (setopt macher-patch-ready-function
  ;;         (lambda (macher-context)
  ;;           (ediff-patch-file nil (current-buffer))))
  )
```

Check `M-x customize-group RET macher` for additional hooks and customization options.

## Usage

### Typical workflow

1. **Navigate to a file** in your project and select a region of text describing something you want
   to implement.
2. **Send an implementation request** with `M-x macher-implement`.
3. **Review the proposed patch** in the automatically-opened diff buffer.
4. **Apply changes** using standard diff-mode commands (e.g. `C-c C-a` to apply hunks).
5. **Request revisions** if needed with `M-x macher-revise`.

### Main commands

- `macher-implement`: Send an implementation request based on selected text or manual input.
- `macher-revise`: Send a revision request along with the contents of the patch buffer.
- `macher-abort`: Cancel any running macher requests for the current project.

### Functions for custom workflows

- `macher-send`: Send a prompt to the LLM with workspace tools and context, but handle the results
  yourself via callback.
- `macher-edit`: Like `macher-send`, but automatically generate and display a patch when complete.

## Architecture

macher takes inspiration from gptel's flexibility and visibility. It avoids relying on stored
history or state - for example, the content for revision requests comes directly from the patch
buffer, which you can inspect and edit manually.

Every macher request is self-contained. When you make a request, macher creates a `macher-context`
struct that acts as an ephemeral file-editing workspace for files in the current project. This
context maintains two versions of each file that gets accessed:

- **Original buffers**: Read-only snapshots of files at first-access time
- **Modified buffers**: Editable copies where the LLM makes changes

The LLM receives a request-specific set of tools for reading and editing "files" (i.e. buffers on
the `macher-context` struct) in the workspace. The tools are structured similarly to a subset of the
[reference filesystem MCP server](https://github.com/modelcontextprotocol/servers). Only files in
the current project can be accessed by the tools (or, if the current file is not in a project, only
the current file can be accessed).

Once the LLM completes its edits, macher generates a unified diff by comparing the original and
modified buffers. The full request prompt is included in a comment at the bottom of the diff, which
provides a sort of conversation history when using `macher-revise`.
