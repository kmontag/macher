;;; test-setup.el --- Global setup for macher tests -*- lexical-binding: t -*-

;;; Code:

(defvar macher-test-require-all-specs
  ;; An empty value means unset.  `getenv' returns "" for `FOO=' in the
  ;; environment, and that would otherwise read as non-nil and turn this on.
  (let ((value (getenv "MACHER_TEST_REQUIRE_ALL_SPECS")))
    (and value (not (string-empty-p value)) value))
  "When non-nil, treat a skipped spec as a failure.

Buttercup's exit status reflects only failed specs, so a spec that
never runs - because it skipped itself via `assume', because it's an
`xit', or because a `--pattern' filter didn't match it - is
indistinguishable from one that passed.  Set this where every spec is
expected to run, so that a test which quietly stops running is as loud
as one that breaks.

Note this is incompatible with the `--pattern' filter behind the
`test.<pattern>' make targets, which marks everything it doesn't select
as pending.")

(defun macher-test--fail-on-skipped-specs (event suites)
  "Exit non-zero once the run is over if any spec in SUITES was skipped.

EVENT and SUITES are as described for `buttercup-reporter'; this runs
after the reporter has printed its summary, so the skipped specs are
visible above the error."
  (when (and macher-test-require-all-specs (eq event 'buttercup-done))
    (let ((pending (buttercup-suites-total-specs-pending suites)))
      (unless (zerop pending)
        (message "%d spec(s) did not run, and MACHER_TEST_REQUIRE_ALL_SPECS is set" pending)
        (kill-emacs 1)))))

(add-function :after (var buttercup-reporter) #'macher-test--fail-on-skipped-specs)

(buttercup-define-matcher :to-appear-once-in (pattern content)
  (let ((pattern (funcall pattern))
        (content (funcall content)))
    (unless (stringp pattern)
      (error (format "Expected string pattern, got %s" pattern)))
    (unless (stringp content)
      (error (format "Expected string content, got %s" content)))
    (let ((matches 0)
          (start 0))
      (while (string-match pattern content start)
        (setq matches (1+ matches))
        (setq start (match-end 0)))
      (cond
       ((= matches 0)
        `(nil . ,(format "Pattern '%s' not found in content '%s'" pattern content)))
       ((= matches 1)
        `(t . ,(format "Pattern '%s' appears exactly once in content '%s'" pattern content)))
       (t
        `(nil
          .
          ,(format "Pattern '%s' found %d times (expected exactly 1) in '%s'"
                   pattern
                   matches
                   content)))))))

(provide 'test-setup)
;;; test-setup.el ends here
