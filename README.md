# macher

A project-aware LLM editing toolset for Emacs, built on [gptel](https://github.com/karthink/gptel).

https://github.com/user-attachments/assets/82c822fe-35e9-47a2-87db-b4dba2432d1b

## What is macher?

**macher** allows your LLM to read/search files in the current project, and propose edits in the
form of patches, potentially involving multiple files. You can apply the patches directly to your
project, or send them back for revision.

macher is a lightweight Emacs-native take on the editing workflows of more full-featured tools like
[Aider](https://aider.chat/) or [Plandex](https://plandex.ai/). It works with any gptel backend that
supports tool use.

It's great for "pseudo-agentic" workflows, for example when you want your LLM to implement a set of
changes, maybe call other tools to do research, etc - but you want to review the aggregate changes
before writing them to disk.

macher takes inspiration from gptel's flexibility and visibility. It doesn't touch your gptel
globals - it's just a set of presets and tools that you can use as you like. Or you can use the
built-in action commands for a quick and easy workflow.

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

(use-package gptel
  ;; ...
  :config
  ;; Recommended - register macher tools and presets with gptel.  This doesn't
  ;; touch any gptel settings or activate any tools/presets; it just makes them
  ;; available in the gptel menu.
  (macher-install)
  ;; Optional - apply the "@macher-base" preset globally.  You probably want to
  ;; do this if you're managing macher tools directly from the gptel menu.
  ;; This is a utility preset that must be applied for macher tools to work -
  ;; see the docstring for details.
  (gptel-apply-preset macher-preset-base))
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

- the prompt is generated by calling a function configured for the action. The function can access
  the current buffer context, selected region, etc., to generate an appropriate prompt.

- one of the macher presets is applied - `@macher-ro` for `macher-discuss`, or `@macher` for the
  others. Note this will work regardless of whether the presets have been installed globally.

- the request is sent from the current workspace's `(macher-action-buffer)`. The UI for this buffer
  can be customized with `macher-action-buffer-ui` and the associated hooks - see
  [Customization](#customization).

You can define your own actions by customizing `macher-actions-alist`. You can also just ignore
these commands and use the presets directly, if you prefer a different workflow.

The built-in actions (`discuss`, `implement`, `revise`) all generate the prompt based on the
selected region text, or manual user input if no region is selected. You can use
`macher-action-from-region-or-input` to implement custom actions with similar behavior.

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

You can see customizable variables/sub-groups with `M-x customize-group RET macher`.

### Actions

| Variable                          | Description                                                         |
| --------------------------------- | ------------------------------------------------------------------- |
| `macher-actions-alist`            | Defines available actions (implement, revise, discuss, etc.)        |
| `macher-action-buffer-ui`         | UI style for action buffers: `'default`, `'org`, `'basic`, or `nil` |
| `macher-action-buffer-setup-hook` | Hook run when creating action buffers                               |
| `macher-action-dispatch-hook`     | Hook run when invoking an action                                    |
| `macher-before-action-functions`  | Functions run before sending the request                            |
| `macher-after-action-functions`   | Functions run after request completes                               |

### Workspace

| Variable                          | Description                                          |
| --------------------------------- | ---------------------------------------------------- |
| `macher-workspace-functions`      | Functions to determine the workspace for a buffer    |
| `macher-workspace-types-alist`    | Defines workspace types (project, file, etc.)        |
| `macher-context-string-function`  | Generates workspace context information for requests |
| `macher-context-string-max-files` | Max files to list in workspace context               |

### Processing

| Variable                          | Description                                                           |
| --------------------------------- | --------------------------------------------------------------------- |
| `macher-process-request-function` | What to do when a request completes (shows a patch buffer by default) |
| `macher-patch-prepare-functions`  | Generate patch content (diff, metadata, etc.)                         |
| `macher-patch-buffer-ui`          | Patch buffer UI: `'diff` or `nil`                                     |
| `macher-patch-buffer-setup-hook`  | Hook run when creating patch buffers                                  |
| `macher-patch-ready-hook`         | Hook run when patch is ready to display                               |

### Tools

| Variable                   | Description                                  |
| -------------------------- | -------------------------------------------- |
| `macher-tools`             | Tool definitions for reading/editing files   |
| `macher-presets-alist`     | Preset definitions (macher, macher-ro, etc.) |
| `macher-tool-category`     | Category for macher tools in gptel registry  |
| `macher-match-max-columns` | Max line length for search results           |
