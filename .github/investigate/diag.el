;;; diag.el --- Diagnose package installation in CI -*- lexical-binding: t; -*-

;;; Commentary:

;; Temporary. Investigates "transient--init-suffix-key is already defined as something else than
;; a generic function" when installing gptel under Emacs 30.1.
;;
;; Driven by environment variables rather than arguments, so nothing is left on the command line
;; for Emacs to visit:
;;
;; - DIAG_MODE: "env" (print state only), "install" (install DIAG_PACKAGES), or "load" (load gptel
;;   from DIAG_ELPA in a fresh process and check the result).
;; - DIAG_ELPA: `package-user-dir'.
;; - DIAG_PACKAGES: space-separated package names to install.

;;; Code:

(require 'package)
(require 'cl-lib)
(require 'seq)

(setq package-user-dir (expand-file-name (or (getenv "DIAG_ELPA") "/tmp/diag-elpa")))
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/") ("melpa" . "https://melpa.org/packages/")))

(defun diag (fmt &rest args)
  "Print FMT with ARGS on a line of its own, prefixed so it can be grepped."
  (princ (concat "DIAG: " (apply #'format fmt args) "\n")))

(defun diag-state (label)
  "Print the state of transient, compat and gptel, under LABEL."
  (diag "----- %s" label)
  (diag "emacs-version: %s" emacs-version)
  (diag "builtin versions: transient=%S compat=%S"
        (alist-get 'transient package--builtin-versions)
        (alist-get 'compat package--builtin-versions))
  (diag "featurep: transient=%s compat=%s gptel=%s gptel-transient=%s"
        (featurep 'transient)
        (featurep 'compat)
        (featurep 'gptel)
        (featurep 'gptel-transient))
  (diag "transient-version: %S" (bound-and-true-p transient-version))
  (diag "locate-library: transient=%S compat=%S"
        (locate-library "transient")
        (locate-library "compat"))
  (diag "fboundp static-when: %s" (fboundp 'static-when))
  (let ((sym 'transient--init-suffix-key))
    (if (not (fboundp sym))
        (diag "%s: not fbound" sym)
      (let ((f (symbol-function sym)))
        (diag "%s: type=%S autoload=%S generic=%S symbol-file=%S"
              sym
              (type-of f)
              (and (autoloadp f) f)
              (and (get sym 'cl--generic) t)
              (symbol-file sym 'defun)))))
  (dolist (entry load-history)
    (when (and (stringp (car entry)) (string-match-p "transient\\|compat" (car entry)))
      (diag "load-history: %s" (car entry)))))

;; Report the state at the moment the error is signaled, then re-signal it.
(advice-add
 'cl-generic-ensure-function
 :around
 (lambda (orig name &rest args)
   (condition-case err
       (apply orig name args)
     (error
      (diag-state (format "ERROR for %s: %s" name (error-message-string err)))
      (signal (car err) (cdr err))))))

;; Report the first `require' of transient or compat, with who asked for it.  `require' calls
;; `load' from C, so advising `load' alone wouldn't see these.
(advice-add
 'require
 :before
 (lambda (feature &rest _)
   (when (and (memq feature '(transient compat)) (not (featurep feature)))
     (diag "first require of %s, would load %S" feature (locate-library (symbol-name feature))))))

;; Report every explicit load of a transient or compat file.
(advice-add
 'load
 :before
 (lambda (file &rest _)
   (when (and (stringp file) (string-match-p "transient\\|compat" (file-name-nondirectory file)))
     (diag "load %s (featurep transient: %s)" file (featurep 'transient)))))

(pcase (getenv "DIAG_MODE")
  ("env"
   (package-initialize)
   (diag-state "env"))
  ("install"
   (package-initialize)
   (package-refresh-contents)
   (diag-state "before install")
   ;; Which compat and transient the archives offer, and what package.el plans to install.
   (dolist (pkg '(compat transient))
     (diag "archive versions of %s: %S" pkg
           (mapcar (lambda (d) (cons (package-desc-archive d) (package-version-join (package-desc-version d))))
                   (alist-get pkg package-archive-contents))))
   (dolist (name (split-string (or (getenv "DIAG_PACKAGES") "") " " t))
     (diag "plan for %s: %S" name
           (condition-case err
               (mapcar #'package-desc-full-name
                       (package-compute-transaction nil (list (list (intern name)))))
             (error err))))
   (dolist (name (split-string (or (getenv "DIAG_PACKAGES") "") " " t))
     (diag "installing %s" name)
     (package-install (intern name)))
   (diag-state "after install")
   (dolist (dir (directory-files package-user-dir t "\\`[^.]"))
     (when (file-directory-p dir)
       (diag "installed: %s" (file-name-nondirectory dir)))))
  ("load"
   (package-initialize)
   (dolist (feature '(gptel gptel-transient gptel-rewrite))
     (condition-case err
         (progn
           (require feature)
           (diag "require %s: ok" feature))
       (error (diag "require %s: %S" feature err))))
   (diag-state "after load")
   (when-let* ((gptel (locate-library "gptel")))
     (dolist (el (directory-files (file-name-directory gptel) t "\\.el\\'"))
       (unless (or (string-suffix-p "-pkg.el" el)
                   (string-suffix-p "-autoloads.el" el)
                   (file-exists-p (concat el "c")))
         (diag "not byte-compiled: %s" (file-name-nondirectory el))))))
  (mode (error "Unknown DIAG_MODE: %S" mode)))

;;; diag.el ends here
