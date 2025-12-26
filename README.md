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
globals (unless you tell it to) - it's just a set of presets, tools, and transforms that you can use
as you like. Or you can use the built-in action commands for a quick and easy workflow.

## Installation and configuration

Example configuration with elpaca + `use-package` integration:

```elisp
(use-package macher
  :ensure (:host github :repo "kmontag/macher")

  :custom
  ;; The org UI has structured navigation and nice content folding.
  (macher-action-buffer-ui 'org)

  :config
  ;; Recommended - register macher tools and presets with gptel.
  (macher-install)

  ;; Recommended - enable macher infrastructure for tools/prompts in
  ;; any buffer.  (Actions and presets will still work without this.)
  (macher-enable)

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
  :config
  ;; Optional - set up macher as soon as gptel is loaded.
  (require 'macher))
```

`macher-install` and `macher-enable` are intended to be very unintrusive, and shouldn't have any
effect on non-macher requests, but they do modify the global gptel environment. See the docstrings
if you want more specifics. Action commands will work regardless of whether you've run these.

## Usage

### Presets

After calling `(macher-install)`, you can use macher presets in any gptel request or buffer.

https://github.com/user-attachments/assets/9b3e0734-5907-4e01-a356-6f9066d7b844

- **`@macher`**: Full editing capabilities. Adds tools to read, search, and edit files. Changes are
  captured in memory and displayed as a patch. Also inherits `@macher-system`.

- **`@macher-ro`**: Read-only access. Adds tools to read and search files,
  but no editing tools. Also inherits `@macher-system`.

- **`@macher-tools`**: Tools only. Enables all macher tools without modifying the system prompt.

- **`@macher-system`**: Ensure system prompt includes workspace context (project name and files
  list). Useful when you want the LLM to understand your project structure but don't need file
  access. See `macher-context-string-placeholder` and `macher-context-string-function`.

- **`@macher-system-commit`**: Commits the current workspace context into the system prompt,
  replacing the dynamic placeholder with a static snapshot. Useful to avoid cache churn in long
  conversations.

- **`@macher-base`**: Utility preset that enables macher tool infrastructure. This gets applied
  automatically when using other macher presets, but you may want to apply it globally via
  `macher-enable`, if you want to use macher tools and/or dynamic context in more generic contexts. This adds transforms that run on every outgoing request, which:
    - provide macher tools with a shared editing environment
    - replace instances of the `macher-context-string-placeholder` in the system prompt

    This preset shouldn't have any effect on requests that don't use macher tools or include the
    placeholder.

All built-in presets can safely be repeatedly applied.

### Actions

Actions are convenience commands that use macher presets with a specific workflow:

- A prompt is captured based on the current selection/cursor position.
- The prompt is sent in a dedicated actions buffer, with a macher preset applied.

The built-in actions are:

- **`macher-implement`**: Request an implementation based on selected text or manual input. Uses
  `@macher`.
- **`macher-revise`**: Request revisions to the current patch. The patch content and original prompt
  are included automatically. Uses `@macher`.
- **`macher-discuss`**: Ask questions about the workspace. Uses `@macher-ro`.

**Typical workflow:**

1. Select text describing what you want to implement
2. Run `M-x macher-implement`
3. Review the patch in the diff buffer
4. Apply changes with `C-c C-a` (apply hunk) or `M-x diff-apply-buffer`
5. Use `M-x macher-revise` if you need changes

**Other action-related commands**:

- **`macher-action`**: Run any action from the `macher-actions-alist`.
- **`macher-abort`**: Cancel running action requests for the current workspace.

The actions buffer UI can be customized with `macher-action-buffer-ui` (see
[Customization](#customization)).

You can define custom actions in `macher-actions-alist`.

### Workspace context

macher can add information about the current workspace (project name, location, file listings) to
your system prompt.

If you've called `(macher-enable)` and/or activated `@macher-base`, any instances of the
`macher-context-string-placeholder` (default: `"[macher_placeholder]"`) in your system prompt will
be replaced with a description of the request buffer's workspace.

The `@macher`, `@macher-ro`, and `@macher-system` presets will append the placeholder automatically
to your system prompt (unless it's already there).

You can also use the `macher-context-string-placeholder` in your own directives to control placement
or add info about the current project to any gptel request.

The workspace description can be customized by setting the `macher-context-string-function`.

## Advanced usage

### Workspaces

The **workspace** is the set of files macher can read and edit. By default, macher supports:

- **`project`**: A project.el project
- **`file`**: A single non-project file

The workspace is determined by `macher-workspace-functions`. All file operations are restricted to
the current workspace.

To add custom workspace types, extend `macher-workspace-types-alist` and
`macher-workspace-functions`.

### Request lifecycle

When you send a macher request with tools:

1. When a tool is first invoked, a `macher-context` struct is created - an ephemeral file-editing
   environment.
1. The LLM uses tools to read/search/edit files in this environment.
1. All changes are captured in memory (never written to disk).
1. When the request completes, `macher-process-request-function` generates a unified diff.
1. The patch is displayed in the workspace's patch buffer.

The `macher-context` is created lazily (only when tools are actually used) and maintains two
versions of each accessed file:

- **Original content**: Read-only snapshot from first access
- **Modified content**: Editable copy where the LLM makes changes

### Tools

Tools are defined in `macher-tools` and structured similarly to the [MCP filesystem
server](https://github.com/modelcontextprotocol/servers).

Available tools:

- `read_file_in_workspace`: Read file contents
- `search_in_workspace`: Regex search (grep-like)
- `list_directory_in_workspace`: List directory contents
- `edit_file_in_workspace`: Make exact string replacements
- `multi_edit_file_in_workspace`: Multiple edits to one file
- `write_file_in_workspace`: Create or overwrite files
- `move_file_in_workspace`: Move/rename files
- `delete_file_in_workspace`: Delete files

When `macher-install` is called, these tools are registered with gptel but not activated. The
`@macher` and `@macher-ro` presets activate the appropriate subsets.

You can also enable/disable them directly from the gptel menu, load them in restored gptel sessions,
etc. Just make sure you call `(macher-enable)` or activate `@macher-base` - this adds a prompt
transform that provides tools with a shared `macher-context` for outgoing requests.

To customize the available tools and presets, modify `macher-tools` or `macher-presets-alist`.

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

| Variable                             | Description                                                    |
| ------------------------------------ | -------------------------------------------------------------- |
| `macher-workspace-functions`         | Functions to determine the workspace for a buffer              |
| `macher-workspace-types-alist`       | Defines workspace types (project, file, etc.)                  |
| `macher-context-string-function`     | Generates workspace context information for requests           |
| `macher-context-string-placeholder`  | System prompt placeholder string for dynamic context injection |
| `macher-context-string-marker-start` | Start marker for context in system prompt                      |
| `macher-context-string-marker-end`   | End marker for context in system prompt                        |
| `macher-context-string-max-files`    | Max files to list in workspace context                         |

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
