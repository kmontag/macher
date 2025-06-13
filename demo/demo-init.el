;;; demo-init.el --- Minimal init file for macher demos. -*- lexical-binding: t -*-

;;; Commentary:
;; This file contains setup for automated demos of macher functionality. The demos in this directory
;; are intended to be run and recorded to generate screengrabs for the package README.

;;; Code:

;;; UI Setup

;; Clear the "For more information..." startup message.
(message "")

;; Hide the menu bar.
(menu-bar-mode -1)

;; Tweaks to make the gptel-mode header look better.
(set-face-attribute 'header-line nil
                    :background (face-background 'mode-line-inactive)
                    :foreground (face-foreground 'mode-line-inactive))

;; Configure macher buffer placement.
(let* ((window-width 0.5)
       (output-window-height 0.3)
       ;; (pattern slot height)
       (buffer-configs
        `(("\\*macher:.*\\*" 0 ,output-window-height)
          ("\\*macher-patch:.*\\*" 1 ,(- 1 output-window-height)))))
  (dolist (config buffer-configs)
    (let ((pattern (nth 0 config))
          (slot (nth 1 config))
          (height (nth 2 config)))
      (add-to-list
       'display-buffer-alist
       `(,pattern
         (display-buffer-in-side-window)
         (side . right)
         (slot . ,slot)
         (window-height . ,height)
         (window-width . ,window-width))))))

;; Don't show additional coloring for line-level edits.
(setopt diff-refine nil)

;; Recognize demo projects with a marker file.
(setopt project-vc-extra-root-markers '(".project"))

;; Avoid warnings about the python indent level.
(setopt python-indent-guess-indent-offset-verbose nil)

;;; LLM Setup

(require 'gptel)
(setopt gptel-log-level 'info)
(setopt gptel-model 'claude-sonnet-4-20250514)
(setq gptel-backend
      (gptel-make-anthropic "Anthropic" :key (getenv "ANTHROPIC_API_KEY") :stream nil))

;; (setopt gptel-model 'devstral)
;; (setq gptel-backend
;;       (gptel-make-ollama "Ollama" :host "localhost:11434" :stream t :models `(,gptel-model)))

(require 'macher)

;; Focus the diff buffer on display for easier automation.
(add-hook 'macher-patch-ready-hook (lambda () (select-window (get-buffer-window))))

;;; Helper Functions for Screen Recording

(defun macher--demo-enter-key-sequence (key-sequence &optional callback)
  "Enter KEY-SEQUENCE as if it were actually being entered by the user.

Once the key sequence has been received as input, call CALLBACK.

The input is performed sequentially and asynchronously. Note that a
synchronous approach using `sleep-for' or similar would prevent any
input from being processed until the current execution context exits.

KEY-SEQUENCE should be a string like \"C-x a b\" with space-separated
keys. Use \"<pause>\" to insert a delay."
  (let ((keys (split-string key-sequence)))
    (if (null keys)
        ;; All keys processed, call the callback.
        (when callback
          (funcall callback))
      ;; Process the first key and schedule the rest.
      (let* ((key (car keys))
             (remaining-keys (cdr keys))
             (remaining-sequence (mapconcat #'identity remaining-keys " "))
             (continue-fn (lambda () (demo-enter-key-sequence remaining-sequence callback))))
        (if (string= key "<pause>")
            ;; Handle pause by waiting and then continuing.
            (run-at-time 0.8 nil continue-fn)
          ;; Handle normal key input.
          (progn
            (setq unread-command-events
                  (nconc unread-command-events (listify-key-sequence (kbd key))))
            ;; Schedule processing of the remaining keys with a very brief wait between keystrokes.
            (run-at-time 0.06 nil continue-fn)))))))

(defun macher--demo-text-to-key-sequence (text)
  "Convert TEXT to a key sequence format for demo input.

Converts text like \"foo bar\" to the form
\"f o o SPC b a r \"."
  (let ((chars (string-to-list text)))
    (concat
     (mapconcat (lambda (char)
                  (if (eq char ?\s)
                      "SPC"
                    (string char)))
                chars
                " ")
     " ")))

(provide 'demo-init)
;;; demo-init.el ends here
