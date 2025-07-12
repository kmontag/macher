;;; demo-macher --- Demo of main editing functionality -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'macher)

;; Create a temporary directory and populate it with project files manually.
(let* ((temp-parent-dir (make-temp-file "macher-demo-parent-" t))
       (temp-dir (expand-file-name "macher-commands" temp-parent-dir))
       (temp-main-file (expand-file-name "__main__.py" temp-dir))
       (temp-lib-file (expand-file-name "wave_lib.py" temp-dir))
       (temp-project-file (expand-file-name ".project" temp-dir)))
  ;; Create the project subdirectory.
  (make-directory temp-dir)
  ;; Create .project file to mark it as a project
  (with-temp-file temp-project-file
    (insert ""))

  ;; Create lib.py file
  (with-temp-file temp-lib-file
    (insert "# Library functions for wave.\n\n# TODO\n"))

  ;; Create main.py file with the TODO
  (with-temp-file temp-main-file
    (insert
     "# Wave.\n\n"
     "if __name__ == \"__main__\":\n"
     "    # TODO: show a gnarly infinite wave animation. ctrl-c to quit.\n"
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
  "<pause3> <down> <pause> <down> <pause>"
  ;; Go to the TODO item.
  "<down> "
  ;; Select the line.
  "C-a C-SPC C-e "
  ;; Wait.
  "<pause> "
  ;; Run `macher-implement'.
  "M-x m a c h e r - i TAB <pause> RET"))

;; Catch the next patch-ready event. Note the initial key sequence above hasn't been entered at this
;; point, so the request hasn't actually been fired off yet.
(let ((hook-idx 0))
  (add-hook
   'macher-patch-ready-hook
   (lambda ()
     (cond
      ((eq hook-idx 0)

       ;; Catch the first patch-ready event.
       (macher--demo-enter-key-sequence
        (concat
         ;; Scroll around.
         "<pause> M-n <pause> <down> <down> <down> <pause> M-n <pause> <pause> M-p <pause> "
         ;; Apply changes.
         "M-x d i f f - a p p l y - b TAB <pause> RET <pause> "
         ;; Close the diff window.
         "C-x 0 <pause> "
         ;; Open a terminal.
         "M-x t e r m <pause> RET <pause> RET <pause> "
         ;; Run the animation.
         (macher--demo-text-to-key-sequence "python __m")
         " TAB <pause> RET "
         ;; Switch to another buffer so the cursor doesn't look weird.
         "C-c o "
         ;; Wait, switch back, and quit animation.
         " <pause5> C-u - 1 C-x o C-c C-c <pause> "
         ;; Switch to the code buffer.
         "C-c o <pause> "
         ;; Run `macher-revise'.
         "M-x m a c h e r - r TAB <pause> RET <pause> "
         ;; Enter revision request.
         (macher--demo-text-to-key-sequence "add a surfer and make the wave blue")
         " <pause> RET ")))
      ((eq hook-idx 1)
       ;; Catch the next and final patch-ready event.
       (macher--demo-enter-key-sequence
        (concat
         ;; Scroll around.
         "<down> <pause> <down> <down> <down> <pause> M-n <pause> M-n <pause> "
         ;; Apply changes
         "M-x d i f f - a p p l y - b TAB <pause> RET <pause> "
         ;; Close the diff buffer.
         "C-x 0 <pause> "
         ;; We're now in the code buffer. Cycle to the other buffer.
         "C-x <left> <pause> "
         ;; Switch to the terminal.
         "C-u - 1 C-x o <pause> "
         ;; Start the animation.
         (macher--demo-text-to-key-sequence "python __m") " TAB <pause> RET "
         ;; Switch to another buffer so the cursor doesn't look weird.
         "C-c o "
         ;; Wait, switch back, and quit animation.
         " <pause6> C-u - 1 C-x o C-c C-c <pause> "
         ;; Exit the demo. The rest should happen fast enough that it gets cut off in the
         ;; video.
         "e x i t RET <pause> " "C-x C-c"))))
     (setq hook-idx (1+ hook-idx)))
   1))

(provide 'demo-macher)
;;; demo-macher.el ends here
