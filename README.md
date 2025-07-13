# macher

A project-aware LLM implementation toolset for Emacs, built on
[gptel](https://github.com/karthink/gptel).

https://github.com/user-attachments/assets/82c822fe-35e9-47a2-87db-b4dba2432d1b

## What is macher?

**macher** provides multi-file, project-aware LLM editing capabilities within Emacs. It allows you
to send complex implementation requests and receive proposed changes as reviewable patches, which
can include changes to multiple files. You can then apply the changes and/or send them back to the
LLM for revision.

The LLM receives contextual information about the current project and file, as well as tools to read
additional project context and propose changes.

macher is a lightweight Emacs-native take on the editing workflows of more full-featured tools like
[Aider](https://aider.chat/) or [Plandex](https://plandex.ai/). It works with any gptel backend that
supports tool use.

macher takes inspiration from gptel's flexibility and visibility. It aims for an unintrusive and
easily-customizable UI - or you can simply add it to your existing workflow using gptel presets.

The API is still very much under development and subject to change.

## Installation and configuration

Example configuration with elpaca + `use-package` integration:

```elisp
(use-package macher
  :ensure (:host github :repo "kmontag/macher")

  :custom
  ;; The org UI has structured navigation and nice content folding.
  (macher-action-buffer-ui 'org)

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
  )

;; Optional - register macher presets for use with any gptel request.
(use-package gptel
  ;; ...
  :config
  (macher-install))
```

## Usage

### Typical workflow

1. **Navigate to a file** in your project and select a region of text describing something you want
   to implement.
1. **Send an implementation request** with `M-x macher-implement`.
1. **Review the proposed patch** in the automatically-opened diff buffer.
1. **Apply changes** using standard diff-mode commands (e.g. `C-c C-a` to apply hunks).
1. **Request revisions** if needed with `M-x macher-revise`.

You can also use macher commands when editing files that aren't part of a project - see
_[workspaces](#key-concepts)_.

### Main commands

- `macher-implement`: Send an implementation request based on selected text or manual input.
- `macher-revise`: Send a revision request along with the contents of the patch buffer.
- `macher-discuss`: Send a question about the current workspace.
- `macher-abort`: Cancel any running macher requests for the current workspace.

### Using gptel presets

After calling `(macher-install)`, you can use macher functionality in any gptel request:

- `@macher`: Full editing capabilities - workspace context + tools to read and edit files
- `@macher-ro`: Read-only - workspace context + tools to read files only
- `@macher-notools`: Context only - workspace information without tools

https://github.com/user-attachments/assets/9b3e0734-5907-4e01-a356-6f9066d7b844

## Details

Read this section if you want to know more about what's actually going on when you send a macher
request.

### Key concepts

- The **workspace** refers to the set of files that macher can read and propose to edit. By default,
  `project` (meaning a project.el project) and `file` (meaning a single non-project file) workspaces
  are supported.

    The current workspace for a buffer is determined using the `macher-workspace-functions`. When
    making macher requests, reads and (proposed) writes will be limited to files in the current
    buffer's workspace.

    See `macher-workspace-types-alist` for a more detailed description of the built-in workspace
    types, or to add custom workspace types.

    To get the workspace associated with the current buffer, call `(macher-workspace)`.

- The **`macher-context`** is a struct created with every macher request, which acts as an ephemeral
  file-editing environment. It maintains two versions of each file that gets accessed:

    - _Original content_: Read-only snapshots of files at first-access time.

    - _Modified content_: Editable copies where the LLM makes changes using tools.

    The LLM uses tools to read/write the content stored on the `macher-context`. At the end of the
    request, if changes were made, the content is used by the default
    `macher-process-request-function` to generate and display a patch in the workspace's
    `(macher-patch-buffer)`.

    The `macher-context` also supports a few additional fields - see the docstring for more details.

### Presets

macher works by extending gptel with three presets, each building on the previous:

1. **`@macher-notools`**: Adds workspace context (file listings, project information) to your
   request without any tools. Use this when you want the LLM to understand your project structure
   but don't need it to read or edit files. The workspace context will be added in the same place as
   the main gptel context, as per the value of `gptel-use-context`.

1. **`@macher-ro`**: Builds on `@macher-notools` by adding read-only tools. The LLM can now read
   files from your workspace to better understand the codebase, but cannot propose changes.

1. **`@macher`**: Builds on `@macher-ro` by adding editing tools. The LLM can now propose changes to
   files. If changes are made, the default `macher-process-request-function` will generate a patch
   at the end of the request, and display it in the workspace's patch buffer (shared across all
   requests within the workspace).

Though not required to use macher's interactive commands, you can install these presets in the
global gptel registry using `(macher-install)`. Then, you can use them in any gptel request.

### Actions

The commands `macher-discuss`, `macher-implement`, and `macher-revise` are wrappers around the more
general `macher-action`. Their behavior is configured in the `macher-actions-alist`. Actions are
requests that follow a specific UI pattern:

- the initial request text is taken from the selected region, if any. Otherwise, the user is
  prompted directly.

- the request text is passed through a transformer function before being sent - for example, the
  `macher-revise` transformation adds some instructions and the patch text.

- one of the macher presets is applied - `@macher-ro` for `macher-discuss`, or `@macher` for the
  others. Note this will work regardless of whether the presets have been installed globally.

- the request is sent from the current workspace's `(macher-action-buffer)`. The UI for this buffer
  can be customized with `macher-action-buffer-ui` and the associated hooks - see
  [Customization](#customization).

You can define your own actions by customizing `macher-actions-alist`. You can also just ignore
these commands and use the presets directly, if you prefer a different workflow.

### Tools

When using presets that include tools (`@macher-ro` and `@macher`), macher generates an ephemeral
set of tools for each request. The tools are structured similarly to a subset of the [reference
filesystem MCP server](https://github.com/modelcontextprotocol/servers).

The tools are generated using `macher-read-tools-function` and (for `@macher`)
`macher-edit-tools-function`.

Security note: the tools' access to the real filesystem is restricted to reading files in the
current workspace. From the tools' perspective, they're getting a single editable root directory
(e.g. the project root) which initially contains only the files in the workspace. Any "edits" are
captured in memory and used to generate diffs, not applied directly to the filesystem.

### Revisions

By default, the patch buffer includes metadata about the request, including the prompt. When using
`macher-revise`, the full patch text is included in the prompt, so successive calls will create a
sort of summarized conversation history directly within the patch.

You can use standard undo/redo within the patch buffer to move through your revision history.

## Customization

### UI

The entire macher workflow/UI can be customized or replaced.

- **`macher-process-request-function`**: Controls what happens when a macher request completes. By
  default, this generates and displays a patch buffer, but you could customize it to auto-apply
  changes, send them to a code review system, etc.

    If you stick with the default for this function, you can also customize the patch behavior:

    - **`macher-patch-prepare-functions`**: Generate patch content (e.g. a unified diff, metadata,
      ...).

    - **`macher-patch-buffer-ui`**: Controls the baseline patch buffer setup. Currently this can be:

        - `'diff`: The default, sets up `diff-mode` and adds a buffer-local `macher-patch-ready-hook`
          to display the buffer.

        - `nil`: No setup. Manual control via the `macher-patch-buffer-setup-hook`.

    - **`macher-patch-buffer-setup-hook`**: Additional functions run when the patch buffer is created.

    - **`macher-patch-ready-hook`**: Controls behavior when the patch buffer is ready to display to
      the user. You could customize this to add alternate behavior like auto-applying changes or
      opening ediff.

- **`macher-action-buffer-ui`**: Controls prompt formatting and general UI configuration in action
  buffers, i.e. when using `macher-implement`, `macher-revise`, etc.

    You can set this to:

    - `'default`: Reasonably-nicely-formatted prompts/responses using `gptel-mode` with your
      `gptel-default-mode`.

    - `'org`: Uses `org-mode` and `gptel-org` for structured navigation and
      nice content folding.

    - `'basic`: Sets up prompt formatting without any particular major mode or other UI tweaks.

    - `nil`: No setup. Use this for full manual control via the `macher-action-buffer-setup-hook`.

    All options except `nil` will add buffer-local behavior to these hooks:

    - **`macher-before-action-functions`**: Handles displaying the action buffer and inserting a
      formatted version of the prompt.

    - **`macher-after-action-functions`**: Handles output cleanup after errored/aborted requests.
      You could customize this to add completion timestamps, request status logs, etc.

- **`macher-action-buffer-setup-hook`**: Runs after the base UI configuration is applied to new
  action buffers. Use this for additional customization on top of the chosen
  `macher-action-buffer-ui` configuration.

### Tools

You can add custom per-request tools by customizing the `macher-edit-tools-function` and/or
`macher-read-tools-function`. For example, you could add a tool for providing a commit message, and
include the result in the patch buffer via a custom entry in the `macher-patch-prepare-functions`.
See the docstrings for more details.

### Contextual information

The `macher-context-string-function` controls the contextual workspace information that gets added
to macher requests.

You might also want to customize the gptel system message and/or the
`gptel-context-string-function`, for example by setting them buffer-locally in the
`macher-action-buffer-setup-hook`.

### Etc.

Check `M-x customize-group RET macher` for the full list of hooks and customization options.
