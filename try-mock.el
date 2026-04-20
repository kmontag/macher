;;; -*- lexical-binding: t -*-
(require 'tramp)
(setq tramp-verbose 0)
(setq inhibit-message t)

;; Register a "mock" method that uses a local /bin/sh.  This is the
;; same trick TRAMP's own test suite uses to exercise TRAMP code paths
;; without needing a real network connection.
(add-to-list
 'tramp-methods
 `("mock"
   (tramp-login-program        ,(executable-find "sh"))
   (tramp-login-args           (("-i")))
   (tramp-remote-shell         "/bin/sh")
   (tramp-remote-shell-args    ("-c"))
   (tramp-connection-timeout   10)))

(let ((dir "/mock::/tmp/"))
  (princ (format "file-remote-p:         %S\n" (file-remote-p dir)))
  (princ (format "file-remote-p local:   %S\n" (file-remote-p dir 'localname)))
  (princ (format "exists:                %S\n" (file-exists-p dir)))
  (princ (format "dir-p:                 %S\n" (file-directory-p dir))))
