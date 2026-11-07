# macher

A project-aware LLM editing toolset for Emacs, built on [gptel](https://github.com/karthink/gptel).

https://github.com/user-attachments/assets/af8bed3d-913b-4365-9f71-85d969810d22

## What is macher?

**macher** allows your LLM to read/search files in the current project, and propose edits in the
form of patches, potentially involving multiple files. You can apply the patches directly to your
project, or send them back for revision.

macher is a lightweight Emacs-native take on LLM file editing. In a world filled with excellent
agentic development tools, macher is focused on the simple joys of reading and editing text. I reach
for it when I'm researching a codebase, or when I'm actively coding/writing and want to drive stick.
Use it on its own or alongside your other tools, to stay nimble when a heavier agentic stack is
unavailable or undesired.

macher's tools are designed to run safely without per-call confirmation. [By default](#workspaces),
file access is restricted to the current `project.el` project, or to a single projectless file.
Edits happen in-memory and get collected into a single patch, with nothing written to disk until you
apply it.

macher takes inspiration from gptel's flexibility and visibility. You can use the presets and tools
in your own workflows, or use the built-in action commands for a quick and easy editing setup. It
doesn't touch your gptel globals (unless you tell it to), and works with any gptel backend that
supports tool use.

## Installation and configuration

macher is available on [MELPA](https://melpa.org/#/macher). Example
configuration with `use-package`:

```elisp
(use-package macher
  :custom
  ;; The org UI has structured conversations and nice content folding.
  (macher-action-buffer-ui 'org)

  :hook
  ;; Set up action buffer behavior to your liking.  Alternately, do
  ;; this more generally in your `gptel-mode-hook'.
  (macher-action-buffer-setup
   . (lambda ()
      ;; Auto-scroll responses.
      (setq-local window-point-insertion-type t)
      ;; Wrap lines.
      (visual-line-mode 1))

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

### Presets/Tools

After calling `(macher-install)`, you can use macher presets and tools in any gptel request or
buffer.

https://github.com/user-attachments/assets/d6a5c8ee-559e-42ab-a240-381f7187cb06

Built-in presets:

- **`@macher`**: Full editing capabilities. Adds tools to read, search, and edit files. Changes are
  captured in memory and displayed as a patch. Also inherits `@macher-system`.

- **`@macher-ro`**: Read-only access. Adds tools to read and search files,
  but no editing tools. Also inherits `@macher-system`.

- **`@macher-tools`**: Tools only. Enables all macher tools without modifying the system prompt.

- **`@macher-system`**: Ensure the system prompt includes macher's content (by default, the
  workspace root). See `macher-system-prompt-placeholder` and `macher-system-prompt-sections`.

- **`@macher-system-commit`**: Commits the current workspace context into the system prompt,
  replacing the dynamic placeholder with a static snapshot. Useful to avoid cache churn in long
  conversations.

- **`@macher-base`**: Utility preset that enables macher tool infrastructure. This gets applied
  automatically when using other macher presets, but you may want to apply it globally via
  `macher-enable`, if you want to use macher tools and/or dynamic context in more generic contexts.
  This adds transforms that run on every outgoing request, which:
    - provide macher tools with a shared editing environment
    - replace instances of the `macher-system-prompt-placeholder` in the system prompt

    This preset shouldn't have any effect on requests that don't use macher tools or include the
    placeholder.

All built-in presets can safely be repeatedly applied.

You can also manage macher tools directly from the `gptel-menu`.

### Actions

Actions are convenience commands that use macher presets with a specific workflow:

- A description of your current location in the code is generated (see `macher-focus-string`).
- A prompt is captured based on the current selection or manual input.
- The prompt is sent in a dedicated actions buffer, with a preset applied.

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
- **`macher-focus-string`**: Get a description of your current code location. When called
  interactively, yanks the description to the kill ring.

The actions buffer UI can be customized with `macher-action-buffer-ui` (see
[Customization](#customization)).

When using the `org-mode` UI (via `macher-action-buffer-ui` and/or `gptel-default-mode`), macher
will set a `GPTEL_TOPIC` for outgoing actions, so you can continue conversations directly in the
action buffer using `gptel-send`.

You can define custom actions in `macher-actions-alist`.

By default, actions run in a single ephemeral buffer per workspace. To send them somewhere else,
e.g. a file, set `macher-action-buffer-function`.

### System prompt

macher can add content to your system prompt, built from `macher-system-prompt-sections`. By
default this is the root of the current workspace.

If you've called `(macher-enable)` and/or activated `@macher-base`, any instances of the
`macher-system-prompt-placeholder` (default: `"[macher_placeholder]"`) in your system prompt will
be replaced with that content, rendered in the request buffer.

The `@macher`, `@macher-ro`, and `@macher-system` presets will append the placeholder automatically
to your system prompt (unless it's already there).

You can also use the `macher-system-prompt-placeholder` in your own directives to control placement
or add info about the current project to any gptel request.

Each element of `macher-system-prompt-sections` is a string, or a function that takes the current
workspace and returns one. Add your own, or reuse the built-in ones (e.g.
`macher-system-prompt-workspace`) in your own directives.

## Advanced usage

### Workspaces

The **workspace** is the set of files macher can read and edit.

The workspace is determined from the current buffer by `macher-workspace-functions`. All file
operations are restricted to the current workspace. By default, macher supports:

- **`project`**: A project.el project
- **`file`**: A single non-project file

For `project` workspaces, only the `project-files` are exposed to the LLM. By default, this means
that things like `.gitignore`'d files won't be visible to the LLM. If you've customized
`project.el`, you might want to verify that your file listings exclude anything sensitive.

A **`directory`** workspace type is also available for working in arbitrary directories (not managed
by project.el). It is not enabled by default to avoid accidentally exposing broad directories to the
LLM. To enable it, you can do something like:

```elisp
(setopt macher-workspace-functions '(macher-workspace-project macher-workspace-directory))
```

To add custom workspace types, customize `macher-workspace-types-alist` and
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
| `macher-focus-string`             | Function to generate description of current code location           |
| `macher-action-buffer-function`   | Chooses the buffer each action runs in                              |
| `macher-action-buffer-ui`         | UI style for action buffers: `'default`, `'org`, `'basic`, or `nil` |
| `macher-action-buffer-setup-hook` | Hook run when setting up action buffers                             |
| `macher-action-dispatch-hook`     | Hook run when invoking an action                                    |
| `macher-before-action-functions`  | Functions run before sending the request                            |
| `macher-after-action-functions`   | Functions run after request completes                               |

### Workspace

| Variable                            | Description                                                    |
| ----------------------------------- | -------------------------------------------------------------- |
| `macher-workspace-functions`        | Functions to determine the workspace for a buffer              |
| `macher-workspace-types-alist`      | Defines workspace types (project, file, etc.)                  |
| `macher-system-prompt-sections`     | Sections of macher's content in the system prompt              |
| `macher-system-prompt-placeholder`  | System prompt placeholder string for dynamic context injection |
| `macher-system-prompt-marker-start` | Start marker for context in system prompt                      |
| `macher-system-prompt-marker-end`   | End marker for context in system prompt                        |

### Processing

| Variable                          | Description                                                           |
| --------------------------------- | --------------------------------------------------------------------- |
| `macher-process-request-function` | What to do when a request completes (shows a patch buffer by default) |
| `macher-patch-prepare-functions`  | Generate patch content (diff, metadata, etc.)                         |
| `macher-patch-buffer-ui`          | Patch buffer UI: `'diff` or `nil`                                     |
| `macher-patch-buffer-setup-hook`  | Hook run when creating patch buffers                                  |
| `macher-patch-ready-hook`         | Hook run when patch is ready to display                               |

### Tools

| Variable                        | Description                                                        |
| ------------------------------- | ------------------------------------------------------------------ |
| `macher-tools`                  | Tool definitions for reading/editing files                         |
| `macher-presets-alist`          | Preset definitions (macher, macher-ro, etc.)                       |
| `macher-tool-category`          | Category for macher tools in gptel registry                        |
| `macher-match-max-columns`      | Max line length for search results                                 |
| `macher-tool-output-max-length` | Max characters any macher tool may return (errors out if exceeded) |

## FAQ

### Why does `diff-apply-buffer` misbehave when the patch contains new or deleted files?

`diff-mode` in Emacs 30.x and below doesn't handle file creations/deletions well. The issue has been
fixed upstream, and the fix will likely be included in the next major Emacs release. In the
meantime, if this case comes up often for you, see
[#45](https://github.com/kmontag/macher/issues/45) for possible workarounds.
