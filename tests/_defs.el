;;; _defs --- Helper definitions for elisp-autofmt.

;;; Commentary:
;; This file simply contains stub definitions for buttercup, to be used by elisp-autofmt when
;; formatting test files. Don't import it. The declarations are copied directly from buttercup.

;;; Code:

(defmacro it (description &rest body)
  (declare (indent 1) (debug (&define sexp def-body))))

(defmacro xit (description &rest body)
  (declare (indent 1)))

(defmacro describe (description &rest body)
  (declare (indent 1)))

(defmacro xdescribe (description &rest body)
  (declare (indent 1) (debug (&define sexp def-body))))

(defmacro before-each (&rest body)
  (declare (indent 0) (debug (&define def-body))))

(defmacro before-all (&rest body)
  (declare (indent 0) (debug (&define def-body))))

(defmacro after-each (&rest body)
  (declare (indent 0) (debug (&define def-body))))

(defmacro after-all (&rest body)
  (declare (indent 0) (debug (&define def-body))))

(defmacro buttercup-define-matcher (matcher args &rest body)
  (declare (indent defun)))

(provide '_defs)
;;; _defs.el ends here
