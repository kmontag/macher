;;; demo-macher --- Demo of main editing functionality -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'macher)

;; Create a temporary directory and populate it with project files manually.
(let* ((temp-parent-dir (make-temp-file "macher-demo-parent-" t))
       (temp-dir (expand-file-name "macher-demo" temp-parent-dir))
       (temp-main-file (expand-file-name "main.py" temp-dir))
       (temp-lib-file (expand-file-name "lib.py" temp-dir))
       (temp-project-file (expand-file-name ".project" temp-dir)))
  ;; Create the project subdirectory.
  (make-directory temp-dir)
  ;; Create .project file to mark it as a project
  (with-temp-file temp-project-file
    (insert ""))

  ;; Create lib.py file
  (with-temp-file temp-lib-file
    (insert "# Library functions for the macher demo project.\n"))

  ;; Create main.py file with the TODO
  (with-temp-file temp-main-file
    (insert
     "# Main file for the macher demo project.\n\n"
     "if __name__ == \"__main__\":\n"
     "    # TODO: Print the 10th fibonacci number using a helper function in lib.\n"
     "    pass\n"))

  ;; Open the main file and the lib file side by side.
  (find-file temp-main-file)
  (split-window-below)
  (other-window 1)
  (find-file temp-lib-file)
  (other-window 1))

(macher--demo-enter-key-sequence
 (concat
  ;; Add a bit of buffer at the beginning, since we'll be trimming the video.
  "<pause> <pause> "
  ;; Go to the TODO item.
  "<down> <down> <down> "
  ;; Select the line.
  "C-a C-SPC C-e "
  ;; Wait.
  "<pause> "
  ;; Run `macher-implement'.
  "M-x m a c h e r - i TAB <pause> RET"))

;; Catch the next patch-ready event. Note the initial key sequence above hasn't been entered at this
;; point, so the request hasn't actually been fired off yet.
(let ((hook-idx 0))
  (add-hook 'macher-patch-ready-hook
            (lambda ()
              (cond
               ((eq hook-idx 0)

                ;; Catch the first patch-ready event.
                (macher--demo-enter-key-sequence
                 (concat
                  ;; Scroll around.
                  "<pause> <down> <down> <down> <down> C-n <pause> C-n <pause> "
                  ;; Apply changes.
                  "M-x d i f f - a p p l y - b TAB <pause> RET <pause> "
                  ;; Close the diff window.
                  "C-x 0 <pause> "
                  ;; Run `macher-revise'.
                  "M-x m a c h e r - r TAB <pause> RET <pause> "
                  ;; Enter revision request.
                  (macher--demo-text-to-key-sequence "get n as a cli arg") " <pause> RET ")))
               ((eq hook-idx 1)

                ;; Catch the next and final patch-ready event.
                (macher--demo-enter-key-sequence
                 (concat
                  ;; Scroll around.
                  "<pause> <down> <down> <down> <down> <down> <down> <down> <down> <pause> "
                  ;; Apply changes
                  "M-x d i f f - a p p l y - b TAB <pause> RET "
                  ;; Scroll around.
                  "<pause> <pause> C-x o <pause> <pause> "
                  ;; Exit the demo.
                  "C-x C-c"))))
              (setq hook-idx (1+ hook-idx)))
            1))

(provide 'demo-macher)
;;; demo-macher.el ends here
