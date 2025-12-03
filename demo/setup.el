;;; demo-init.el --- Minimal init file for macher demos. -*- lexical-binding: t -*-

;;; Commentary:
;; This file contains setup for automated demos of macher functionality.  The demos in this directory
;; are intended to be run and recorded to generate screengrabs for the package README.

;;; Code:

;;; UI Setup

;; Clear the "For more information..." startup message.
(message "")

;; Pre-load org mode.
(require 'org)

;; Hide the menu bar.
(menu-bar-mode -1)

;; Tweaks to make the gptel-mode header look better.
(set-face-attribute 'header-line nil
                    :background (face-background 'mode-line-inactive)
                    :foreground (face-foreground 'mode-line-inactive))

;; Set the shell for `term'.
(setopt explicit-shell-file-name "bash --noprofile --norc")

;; Avoid the "default is now zsh" warning on MacOS.
(setenv "BASH_SILENCE_DEPRECATION_WARNING" "1")

;; Configure terminal for smooth animations.
(setq
 term-scroll-to-bottom-on-output t
 term-scroll-show-maximum-output t
 term-buffer-maximum-size 2048) ; Prevent excessive scrollback

;; Terminal setup for smooth animation rendering.
(add-hook
 'tty-setup-hook
 (lambda ()
   "Configure terminal for smooth, flicker-free animations."
   ;; Disable line wrapping to prevent jumpy text.
   (setq truncate-lines t)
   ;; Disable automatic scrolling to reduce flickering.
   (setq scroll-conservatively 10000)
   ;; Set up clean terminal environment.
   (term-send-string (get-buffer-process (current-buffer)) "export PS1='$ '\n")
   (term-send-string (get-buffer-process (current-buffer)) "export TERM=xterm-256color\n")
   ;; Disable terminal bell to avoid visual flashing.
   (term-send-string (get-buffer-process (current-buffer)) "set bell-style none\n")
   ;; Set up Python to use unbuffered output for smoother animation.
   (term-send-string (get-buffer-process (current-buffer)) "export PYTHONUNBUFFERED=1\n")
   (term-send-string (get-buffer-process (current-buffer)) "clear\n")))

;; Don't show additional coloring for line-level edits.
(setopt diff-refine nil)

;; Recognize demo projects with a marker file.
(setopt project-vc-extra-root-markers '(".project"))

;; Avoid warnings about the python indent level.
(setopt python-indent-guess-indent-offset-verbose nil)

;; Never grow the minibuffer, it looks weird.
(setopt resize-mini-windows nil)

;;; LLM Setup

(require 'gptel)
(require 'gptel-org)
(setopt gptel-default-mode 'org-mode)
(setopt gptel-include-tool-results t)
(setopt gptel-log-level 'info)
(setq
 gptel--system-message
 (concat
  "You are my coding assistant living in Emacs. "
  "I'm using you to record a demo video of your abilities. "
  "My interaction with you is fully automated, so you should keep answers simple, and  "
  "if you implement any commands, "
  "they must require no input. "
  "If I ask you to make changes to something, you should "
  "read existing content, avoid creating new files, and edit at least 2 files. "
  "The .project file is not relevant.\n\n"
  "Commands will be run in a small `term-mode` bash terminal, with no special fonts installed, "
  "so don't go overboard. Use only standard ASCII characters.\n\n"
  "**** Make VERY sure all commands exit cleanly with NO residual output. Clear the terminal "
  "before starting animations, and reset it to a blank uncolored prompt when exiting. "
  "Animations should update in-place. **** \n"
  "For animations, use techniques that minimize flickering: "
  "1. Use time.sleep() with small delays (0.1-0.3 seconds) between frames "
  "2. Use \\r to overwrite lines instead of printing new ones when possible "
  "3. Use sys.stdout.flush() after each frame for smooth output "
  "4. Avoid rapid screen clearing or excessive output\n\n"
  "Unless otherwise instructed, animations should be around 8 lines high "
  "(give or take) and fairly dense. "
  "Only implement what I tell you to - keep it simple.\n\n"
  "Pay attention to using correct JSON formatting when calling tools - "
  "MAKE SURE your arrays are properly closed."))

(setopt gptel-model 'claude-sonnet-4-20250514)
(setq gptel-backend (gptel-make-anthropic "Claude" :key (getenv "ANTHROPIC_API_KEY") :stream nil))
;; (setopt gptel-model 'llama3.3:70b)
;; (setq gptel-backend
;;       (gptel-make-ollama "Ollama" :host "localhost:11434" :stream t :models `(,gptel-model)))

(require 'macher)
(setopt macher-action-buffer-ui 'org)
(macher-install)

;; Focus the diff buffer on display for easier automation.
(add-hook 'macher-patch-ready-hook (lambda () (select-window (get-buffer-window))) 1)

;; Configure macher buffer placement.
(let* ((window-width 0.5)
       (request-window-height 0.3)
       ;; (pattern slot height)
       (buffer-configs
        `(("\\*macher:.*\\*" 0 ,request-window-height)
          ("\\*macher-patch:.*\\*" 1 ,(- 1 request-window-height))
          (,(concat "\\*" (gptel-backend-name gptel-backend) "\\*") 0 ,request-window-height))))
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

;;; Helper Functions for Screen Recording

(defun macher--demo-enter-key-sequence (key-sequence &optional callback)
  "Enter KEY-SEQUENCE as if it were actually being entered by the user.

Once the key sequence has been received as input, call CALLBACK.

The input is performed sequentially and asynchronously.  Note that a
synchronous approach using `sleep-for' or similar would prevent any
input from being processed until the current execution context exits.

KEY-SEQUENCE should be a string of space-separated keys in standard
Emacs key notation.  Use \"<pause>\" to insert a delay, or
\"<pause2>\", \"<pause5>\", etc. to pause for a number of seconds."
  (let ((keys (split-string key-sequence)))
    (if (null keys)
        ;; All keys processed, call the callback.
        (when callback
          (funcall callback))
      ;; Process the first key and schedule the rest.
      (let* ((key (car keys))
             (remaining-keys (cdr keys))
             (remaining-sequence (mapconcat #'identity remaining-keys " "))
             (continue-fn
              (lambda () (macher--demo-enter-key-sequence remaining-sequence callback))))
        (cond
         ;; Handle pause with optional duration specification
         ((string-match "^<pause\\([0-9]\\)?>$" key)
          (let ((duration
                 (if (match-string 1 key)
                     (string-to-number (match-string 1 key))
                   0.8)))
            (run-at-time duration nil continue-fn)))
         ;; Handle normal key input
         (t
          (setq unread-command-events
                (nconc unread-command-events (listify-key-sequence (kbd key))))
          ;; Schedule processing of the remaining keys with a very brief wait between keystrokes.
          (run-at-time 0.03 nil continue-fn)))))))

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

(provide 'setup)
;;; setup.el ends here
