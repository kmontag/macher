NPM := npm
EASK := node_modules/.bin/eask

# CLI tools for generating demo videos. Not needed for other package-related operations.
AGG := agg
ASCIINEMA := asciinema
FFMPEG := ffmpeg

# Note this also installs eask dependencies via the package's postinstall script.
$(EASK) .eask: package.json package-lock.json Eask
	"$(NPM)" install
	[ -f "$(EASK)" ] && touch "$(EASK)" && touch .eask/

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

.PHONY: lint
lint: analyze lint.declare lint.package lint.regexps

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

demo/output/%.mp4: demo/output/%.gif
# Trim blank/loading screens that show up at the start and end of recordings. These values depend
# somewhat on how fast the system responds to commands, but hopefully these work fine.
	@START_TRIM=0.5; \
	END_TRIM=4; \
	DURATION=$$($(FFMPEG) -y -i "$<" 2>&1 | grep "Duration" | cut -d ' ' -f 4 | sed s/,// | awk -F: '{ print $$1*3600 + $$2*60 + $$3 }'); \
	TRIMMED_DURATION=$$(echo "$$DURATION - $$START_TRIM - $$END_TRIM" | bc); \
	$(FFMPEG) -y -i "$<" -movflags +faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" -ss $$START_TRIM -t $$TRIMMED_DURATION "$@"
