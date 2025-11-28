;;; test-setup.el --- Global setup for macher tests -*- lexical-binding: t -*-

;;; Code:

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
