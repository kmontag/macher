# AGENTS.md

## Project overview

macher is an Emacs Lisp package providing project-aware LLM editing capabilities built on gptel.

## Development workflow

**Before committing any changes:**

1. Run `make format` to auto-format code
2. Run `make lint` - all checks must pass
3. Run `make compile` - must complete without errors
4. Run `make test` - all tests must pass

See `Makefile` for available commands and implementation details.

## Code style

- Follow existing Emacs Lisp conventions in the codebase
- Respect `.editorconfig` and `.dir-locals` settings
- All code must pass `elisp-autofmt` and Prettier formatting checks

## Testing

- Use `make test.<pattern>` to run specific test subsets
- Tests use Buttercup framework

## File organization

- Main source: `macher.el`
- Tests: `tests/test-*.el`
- Build/test config: `Makefile`, `Eask`
