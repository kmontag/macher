;;; macher-externals.el --- Aliases to macher symbols from other namespaces -*- lexical-binding: t -*-

;;; Commentary:
;; This file contains macher-related aliases that live in external namespaces, in particular the
;; 'gptel-' namespace.
;;
;; Such aliases violate package namespacing conventions, but are useful to enable users to control
;; macher variables using gptel presets.
;;
;; Note this file breaks the package-lint linter, and so is explicitly excluded from package-lint
;; linting.

;;; Code:

(defvaralias 'gptel--macher-process-request-function 'macher-process-request-function
             "This is a gptel-prefixed alias for the 'macher-process-request-function'.

This alias enables ':macher-process-request-function' to be used as a
key in gptel presets.")

(provide 'macher-externals)
;;; macher-externals.el ends here
