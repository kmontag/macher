;;; demo-macher-gptel-presets --- Demo of using macher presets with gptel directly -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'gptel)
(require 'macher)

(add-hook
 'gptel-mode-hook
 (lambda ()
   ;; Wrap lines.
   (visual-line-mode 1)))
(add-hook 'gptel-post-stream-hook (lambda () (goto-char (point-max))))
(add-hook 'gptel-post-response-functions (lambda (&rest _) (goto-char (point-max))))

;; Create a temporary directory and populate it with project files manually.
(let* ((temp-parent-dir (make-temp-file "macher-parent-" t))
       (temp-dir (expand-file-name "presets-demo" temp-parent-dir))
       (temp-main-file (expand-file-name "__main__.py" temp-dir))
       (temp-state-file (expand-file-name "game_state.py" temp-dir))
       (temp-project-file (expand-file-name ".project" temp-dir)))
  ;; Create the project subdirectory.
  (make-directory temp-dir)
  ;; Create .project file to mark it as a project
  (with-temp-file temp-project-file
    (insert ""))

  ;; Create utils.py file
  (with-temp-file temp-state-file
    (insert "# Game state.\n\nclass GameState:\n    pass\n"))

  ;; Create calculator.py file with the TODO
  (with-temp-file temp-main-file
    (insert
     "# Game animation (gptel presets demo).\n\n" "if __name__ == \"__main__\":\n" "    pass\n"))

  ;; Open the main file and the utils file side by side.
  (find-file temp-main-file)
  (split-window-below)
  (other-window 1)
  (find-file temp-state-file)
  (other-window 1))

(macher--demo-enter-key-sequence
 (concat
  ;; Add a bit of buffer at the beginning, since we'll be trimming the video.
  "<pause3> <down> <pause> <pause> "
  ;; Open gptel.
  "M-x g p t e l <pause> RET <pause> RET <pause> "
  ;; Type the first request using @macher preset.
  (macher--demo-text-to-key-sequence "what was the first commercially successful video game?")
  " <pause> "
  ;; Send the request.
  "C-c RET"))

;; Catch the next response completion. We'll use a hook to detect when the response is done.
(let ((hook-idx 0))
  (add-hook 'gptel-post-response-functions
            (lambda (_start _end)
              (cond
               ((eq hook-idx 0)
                (macher--demo-enter-key-sequence
                 (concat
                  ;; Short pause.
                  "<pause> <pause> "
                  ;; Go to end of buffer.
                  "M-> <pause> "
                  (macher--demo-text-to-key-sequence
                   (concat
                    "cool. @macher write a self-playing demo with randomized paddles, "
                    "15 lines x 25 columns. use colors."))
                  ;; Send the request.
                  " <pause> C-c RET ")))
               ((eq hook-idx 1)
                ;; After second response, show the results.
                (macher--demo-enter-key-sequence
                 (concat
                  ;; Wait for response to complete.
                  "<down> <pause> "
                  ;; Scroll around a bit.
                  "<down> <down> <down> M-n <pause> M-n <pause> <pause> "
                  ;; Apply changes.
                  "M-x d i f f - a p p l y - b TAB <pause> RET <pause> "
                  ;; Close the patch buffer.
                  "C-x 0 <pause> "
                  ;; Open a terminal.
                  "M-x t e r m <pause> RET <pause> RET <pause> "
                  ;; Run the animation.
                  (macher--demo-text-to-key-sequence "python __m") " TAB RET "
                  ;; Switch to another buffer so the cursor doesn't look weird.
                  " C-c o "
                  ;; Wait, switch back, and quit animation.
                  " <pause5> <pause5> C-u - 1 C-x o C-c C-c <pause> <pause> "
                  ;; The rest should happen fast enough that it gets cut off in the video.
                  (macher--demo-text-to-key-sequence "exit") " RET "
                  ;; Exit the demo.
                  "C-x C-c"))))
              (setq hook-idx (1+ hook-idx)))
            1))

(provide 'demo-macher-gptel-presets)
;;; demo-macher-gptel-presets.el ends here
