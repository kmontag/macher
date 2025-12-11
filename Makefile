NPM := npm
EASK := node_modules/.bin/eask
PRETTIER := node_modules/.bin/prettier

# CLI tools for generating demo videos. Not needed for other package-related operations.
AGG := agg
ASCIINEMA := asciinema
FFMPEG := ffmpeg

# Note this also installs eask dependencies via the package's postinstall script.
$(EASK) $(PRETTIER) .eask: package.json package-lock.json Eask
	"$(NPM)" install
	[ -f "$(EASK)" ] && touch "$(EASK)" && touch "$(PRETTIER)" && touch .eask/

# Analyze the Eask file itself for inconsistencies, and exit unsuccessfully if any are found.
.PHONY: analyze
analyze: $(EASK) .eask
# This always seems to exit with status 0, and it's unclear whether output will be expected on
# stdout or stderr, see https://github.com/emacs-eask/cli/issues/276. But in the successful case,
# the output will be exactly "(Checked 1 file)" (possibly with blank lines and color encodings around
# it, even when --no-color is passed - we strip these using sed and tr).
	@OUTPUT=$$($(EASK) --strict --no-color analyze --json 2>&1); \
	echo "$$OUTPUT"; \
	CLEANED_OUTPUT=$$(echo "$$OUTPUT" | sed 's/\x1b\[[0-9;]*m//g' | tr -d '\n' ); \
	if [ "$$CLEANED_OUTPUT" != "(Checked 1 file)" ]; then \
		echo "Error: eask analyze output was '$$CLEANED_OUTPUT', expected '(Checked 1 file)'" >&2; \
		exit 1; \
	fi

# Generic target for lint commands that take a list of files.
.PHONY: lint.%
lint.%: $(EASK) .eask
# Note we exclude files beginning with a non-alphanumeric character in the tests directory, since
# the elisp-autofmt defs file shouldn't be checked.
	$(EASK) --strict lint $* *.el demo/*.el tests/[a-z]*.el

# The keywords linter doesn't take file arguments, so we define it separately.
.PHONY: lint.keywords
lint.keywords: $(EASK) .eask
	$(EASK) --strict lint keywords

.PHONY: lint
lint: analyze lint.checkdoc lint.declare lint.keywords lint.package lint.regexps

# Compile is only used to check for byte-compilation errors/warnings. We remove the .elc files
# afterward to avoid polluting the source directory and causing "source file newer than
# byte-compiled file" warnings during e.g. tests.
.PHONY: compile
compile: $(EASK) .eask
	$(EASK) --strict compile; status=$$?; rm -f *.elc; exit $$status

.PHONY: format
format: format.elisp format.prettier

.PHONY: format.check
format.check: format.elisp.check format.prettier.check

# Use absolute path for Python to work with Nix-isolated Emacs in CI.
PYTHON3 := $(shell which python3)

# Common setup for elisp-autofmt commands. Notes:
# - We use editorconfig-apply to respect .editorconfig settings (e.g. fill-column).
# - elisp-autofmt--workaround-make-proc forces use of call-process instead of make-process,
#   which works around subprocess communication issues in Nix-isolated CI environments.
# - .eask/*.el provides macro definitions so test files are formatted correctly.
define ELISP_AUTOFMT_SETUP
--eval "(require 'editorconfig)" \
--eval "(require 'elisp-autofmt)" \
--eval "(setq elisp-autofmt--workaround-make-proc t)" \
--eval "(setq elisp-autofmt-python-bin \"$(PYTHON3)\")" \
-l .eask/buttercup.el \
-l .eask/gptel.el \
-l .eask/gptel-ollama.el
endef

.PHONY: format.elisp
format.elisp: $(EASK) .eask
	$(EASK) emacs --batch \
		$(ELISP_AUTOFMT_SETUP) \
		--eval "(dolist (file (cdr command-line-args-left)) \
			(find-file file) \
			(editorconfig-apply) \
			(elisp-autofmt-buffer) \
			(save-buffer))" \
		-- $$(git ls-files '*.el' | xargs realpath)

.PHONY: format.elisp.check
format.elisp.check: $(EASK) .eask
	$(EASK) emacs --batch \
		$(ELISP_AUTOFMT_SETUP) \
		--eval "(let ((failed nil)) \
			(dolist (file (cdr command-line-args-left)) \
			  (find-file file) \
			  (editorconfig-apply) \
			  (let ((original (buffer-string)) \
			        (result (elisp-autofmt-buffer))) \
			    (unless result \
			      (message \"elisp-autofmt-buffer failed for: %s\" file) \
			      (setq failed t)) \
			    (if (string= original (buffer-string)) \
			        (message \"OK: %s\" file) \
			      (message \"File needs formatting: %s\" file) \
			      (let ((temp-file (make-temp-file \"autofmt-check-\"))) \
			        (write-region (point-min) (point-max) temp-file) \
			        (message \"Diff:\\n%s\" \
			          (shell-command-to-string (format \"diff -u %s %s || true\" file temp-file))) \
			        (delete-file temp-file)) \
			      (setq failed t)))) \
			(when failed (kill-emacs 1)))" \
		-- $$(git ls-files '*.el' | xargs realpath)

.PHONY: format.prettier
format.prettier: $(PRETTIER)
	git ls-files --cached --others --exclude-standard | xargs $(PRETTIER) --write --ignore-unknown

.PHONY: format.prettier.check
format.prettier.check: $(PRETTIER)
	git ls-files --cached --others --exclude-standard | xargs $(PRETTIER) --check --ignore-unknown

# Convenience targets for running tests that match a pattern, e.g. `make test.unit`.
.PHONY: test.%
test.%: $(EASK) .eask
	$(EASK) exec buttercup -L . --no-skip -p "$*" tests/

.PHONY: test
test: $(EASK) .eask
	$(NPM) test

.PHONY: demos
demos: $(patsubst demo/demo-%.el,demo/output/%.mp4,$(wildcard demo/demo-*.el))

demo/output/%.cast: demo/demo-%.el macher.el demo/setup.el $(EASK) .eask
	mkdir -p "$(@D)"
	$(ASCIINEMA) rec --idle-time-limit 2 --command "$(EASK) emacs -nw -l demo/setup.el -l $<" --idle-time-limit=1 --cols=180 --rows=50 --overwrite "$@"

demo/output/%.gif: demo/output/%.cast
	$(AGG) --theme=github-dark "$<" "$@"

# Start by rendering to mkv. We do this instead of rendering directly to mp4 because the
# direct-to-mp4 conversion sometimes leave some blank frames at the beginning of the video.
demo/output/%.mkv: demo/output/%.gif
# Trim blank/loading screens that show up at the start and end of recordings. These values depend
# somewhat on how fast the system responds to commands, but hopefully these work fine.
	@START_TRIM=2; \
	END_TRIM=5; \
	DURATION=$$($(FFMPEG) -y -i "$<" 2>&1 | grep "Duration" | cut -d ' ' -f 4 | sed s/,// | awk -F: '{ print $$1*3600 + $$2*60 + $$3 }'); \
	TRIMMED_DURATION=$$(echo "$$DURATION - $$START_TRIM - $$END_TRIM" | bc); \
	$(FFMPEG) -y -i "$<" -movflags +faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" -ss $$START_TRIM -t $$TRIMMED_DURATION "$@"

# After the mkv is rendered, we can get an mp4 in a more straightforward way that doesn't have the
# blank-at-beginning issue. mp4's can be included inline in READMEs on GitHub.
demo/output/%.mp4: demo/output/%.mkv
	$(FFMPEG) -y -i "$<" "$@"
