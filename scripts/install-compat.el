;;; install-compat.el --- Install compat from GNU ELPA ahead of eask -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `eask emacs --batch -l scripts/install-compat.el' before `eask install-deps'. eask sets
;; up `package-user-dir' and the archives from the Eask file, so this installs where eask does.
;;
;; MELPA's transient requires compat 31.1. Emacs 30's package.el counts the built-in compat
;; (30.1.9999) as satisfying that, so neither package.el nor eask installs compat 31.1, and the new
;; transient then fails to load (`static-when' is void), which breaks byte-compiling gptel. Asking
;; for compat by name doesn't help either, since it's considered already installed; passing the
;; package descriptor does.
;;
;; Does nothing when the built-in compat is new enough, or compat 31.1 is already installed.

;;; Code:

(require 'package)

(defconst install-compat-version '(31 1)
  "Minimum compat version required by MELPA's transient.")

(when (version-list-<
       (or (alist-get 'compat package--builtin-versions) '(0)) install-compat-version)
  (package-initialize)
  (let ((installed (cadr (assq 'compat package-alist))))
    (unless (and installed
                 (not (version-list-< (package-desc-version installed) install-compat-version)))
      (package-refresh-contents)
      (package-install (cadr (assq 'compat package-archive-contents))))))

;;; install-compat.el ends here
