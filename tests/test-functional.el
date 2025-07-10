;;; test-functional.el --- Functional tests for macher.el -*- lexical-binding: t -*-

;;; Commentary:
;; Functional tests for macher.el that interact with a real LLM on a local ollama server.
;; Uses very simple prompts to ensure that even weak LLMs can handle them correctly.
;;
;; These are not intended to be exhaustive tests of tool functionality. They're intended
;; to catch issues in the general end-to-end implementation workflow.

;;; Code:

(require 'buttercup)
(require 'macher)
(require 'gptel)
(require 'gptel-curl)
(require 'gptel-ollama)
(require 'project)

;; Uncomment for debugging output.
;;
;; (advice-add
;;  #'gptel--handle-post-insert
;;  :before
;;  (lambda (fsm)
;;    (when-let* ((info (gptel-fsm-info fsm))
;;                (start-marker (plist-get info :position))
;;                (tracking-marker (plist-get info :tracking-marker))
;;                (output-buffer (marker-buffer start-marker)))
;;      (with-current-buffer output-buffer
;;        (let ((newly-inserted-text
;;               (buffer-substring-no-properties
;;                (marker-position start-marker) (marker-position tracking-marker))))
;;          (message newly-inserted-text))))))

;; Uncomment for finer-grained debugging output.
;;
(advice-add
 #'gptel--insert-response
 :before
 (lambda (response &rest _)
   (when (stringp response)
     (message response))))

;; Logs will be printed by `with-macher-test-gptel' if enabled.
;;
;; (setopt gptel-log-level 'info)

(describe "functional tests"
  :var*
  (
   ;; The ollama model to use in gptel requests. Must be installed locally.
   (ollama-model "llama3.2:3b")

   ;; Add a transform based on the specific ollama model, e.g. to turn off thinking.
   (ollama-prompt-transform #'identity)

   ;; Add a specific seed value to try and get consistent responses. This still doesn't make things
   ;; perfectly replicable, as the output also depends on the OS and system tools, but it should
   ;; make responses consistent within a particular environment.
   ;;
   ;; You might need to play around with this when changing the model.
   (ollama-seed 5678)

   ;; The host where the ollama server is running.
   (ollama-host (or (getenv "MACHER_TEST_OLLAMA_HOST") "localhost:11434"))

   ;; Timeout in seconds for macher functional tests. Needs to be permissive as tests run in a
   ;; fairly constrained environment on GitHub Actions.
   (test-timeout 300)

   ;; Store global vars for restoration at the end of the suite.
   (original-project-vc-extra-root-markers project-vc-extra-root-markers)
   (original-debug-on-error debug-on-error)
   (original-gptel-model gptel-model)
   (original-gptel-directives gptel-directives)
   (original-gptel-backend gptel-backend)

   ;; Trackers for temp directories/files that need cleanup in after-each.
   (temp-files-created nil)
   (temp-dirs-created nil)

   ;; Contents of main.txt in both the single-file and project case.
   (main-file-contents "Hello from main file.\nNumber: 123")

   (system-message
    "You are editing the contents of a repository. Edit files directly using the tools provided. Do not ask for clarification or permission.")

   ;; Helper functions as lambdas
   (create-temp-file
    (lambda (content &optional dir)
      "Create a temporary file with CONTENT in DIR or a temp directory.
Always creates a file named 'main.txt'."
      (let* ((temp-root (or dir (make-temp-file "macher-test-dir" t)))
             ;; The actual project files live in a subdirectory with a predictable name, to ensure
             ;; predictable input to the LLM.
             (temp-dir (expand-file-name "macher-demo" temp-root))
             (temp-file (expand-file-name "main.txt" temp-dir)))
        ;; Track temp directories for cleanup
        (push temp-root temp-dirs-created)
        (push temp-dir temp-dirs-created)
        ;; Track temp file for cleanup
        (push temp-file temp-files-created)
        ;; Create the macher-demo subdirectory
        (make-directory temp-dir t)
        ;; Create the file with content
        (with-temp-file temp-file
          (insert content))
        ;; Return the created filename
        temp-file)))

   (create-temp-project
    (lambda ()
      "Create a temporary directory with test files for project cases.
Returns a plist with :dir, :files, and :cleanup function."
      (let* ((temp-root (make-temp-file "macher-test-proj" t))
             (temp-dir (expand-file-name "macher-demo" temp-root))
             (main-file (expand-file-name "main.txt" temp-dir))
             (extra-file (expand-file-name "extra.txt" temp-dir))
             (project-file (expand-file-name ".project" temp-dir))
             (files (list main-file extra-file project-file)))

        ;; Track temp directories for cleanup
        (push temp-root temp-dirs-created)
        (push temp-dir temp-dirs-created)
        ;; Track temp files for cleanup
        (dolist (file files)
          (push file temp-files-created))

        ;; Create the macher-demo subdirectory
        (make-directory temp-dir t)
        ;; Create the test files.
        (with-temp-file main-file
          (insert main-file-contents))
        (with-temp-file extra-file
          (insert "This is an extra file."))
        ;; Create .project file to ensure the directory is detected as a project.
        (with-temp-file project-file
          (insert ""))

        ;; Return information about the project.
        (list :dir temp-dir :files files))))

   (test-file-operation
    (lambda (prompt file-mode callback-test)
      "Test a LLM operation with PROMPT in FILE-MODE with CALLBACK-TEST.
PROMPT is the instruction to send to the LLM.
FILE-MODE is either 'single-file or 'project.
CALLBACK-TEST is a function that verifies the result."
      (let*
          (
           ;; gptel uses `with-demoted-errors' to swallow errors at some points during the request
           ;; lifecycle. Set 'debug-on-error' globally to cause swallowed errors to actually trigger
           ;; the debugger, which will be interpreted as a proper test error.
           (debug-on-error t)
           (is-project (eq file-mode 'project))
           (test-vars
            (if is-project
                (funcall create-temp-project)
              (let* ((temp-file (funcall create-temp-file main-file-contents))
                     (temp-dir (file-name-directory temp-file)))
                (list :files (list temp-file) :dir temp-dir))))
           (test-dir (plist-get test-vars :dir))
           (test-files (plist-get test-vars :files))
           (main-file (car test-files))
           (main-file-buffer)
           (patch-buffer nil)
           (test-complete nil)
           (test-result nil)
           ;; Allow project.el to detect projects with .project marker file in the root.
           (project-vc-extra-root-markers '(".project")))
        (unwind-protect
            ;; Set up the test environment.
            (let ((test-timer nil))

              ;; Visit the first file.
              (find-file main-file)
              (setq main-file-buffer (current-buffer))

              ;; Use a callback to check the results.
              (macher-implement
               (funcall ollama-prompt-transform prompt)
               (lambda (error _execution _fsm)
                 (setq test-complete t)
                 (when test-timer
                   (cancel-timer test-timer)
                   (setq test-timer nil))

                 (if error
                     (setq test-result (cons 'error error))
                   ;; Test was successful.
                   (setq patch-buffer (macher-patch-buffer))
                   (setq test-result
                         (if (and patch-buffer (buffer-live-p patch-buffer))
                             (progn
                               ;; The patch buffer should always include the prompt.
                               (expect
                                (with-current-buffer patch-buffer
                                  (buffer-string))
                                :to-match (regexp-quote prompt))
                               ;; Run the specific test for this operation.
                               (funcall callback-test patch-buffer test-files))
                           (cons 'error "No patch buffer created"))))))

              ;; Set a timer to handle the timeout case.
              (setq test-timer
                    (run-with-timer
                     test-timeout nil
                     (lambda ()
                       (unless test-complete
                         (setq test-complete t)
                         (setq test-result (cons 'error "Test timed out"))))))

              ;; Wait for the test to complete by checking the flag.
              (with-timeout (test-timeout nil)
                (while (not test-complete)
                  (sit-for 0.1)))


              ;; Check result.
              (expect test-complete :to-be-truthy)
              (expect test-result :to-be t))
          ;; Cleanup: abort any ongoing requests and kill buffers
          ;; (temp files will be cleaned up by after-each).
          (when-let ((action-buffer (macher-action-buffer)))
            (when (buffer-live-p action-buffer)
              ;; If there are any running requests - assume that they would be happening in the
              ;; action buffer, since the additional after-each expectation will catch issues if
              ;; that's not the case.
              (when gptel--request-alist
                ;; This is expected to be reached if a test error occurs, but print a warning so
                ;; it's fairly obvious if it somehow starts getting reached in other cases as well.
                (warn "Aborting running gptel request in %s" action-buffer)
                (gptel-abort action-buffer))))
          (when (buffer-live-p main-file-buffer)
            (kill-buffer main-file-buffer))
          (when (and patch-buffer (buffer-live-p patch-buffer))
            (kill-buffer patch-buffer)))))))

  (before-all
    ;; Allow project.el to detect projects with .project marker file in the root.
    (setq project-vc-extra-root-markers '(".project"))

    ;; gptel uses `with-demoted-errors' to swallow errors at some points during the request
    ;; lifecycle. Set 'debug-on-error' globally to cause swallowed errors to actually trigger
    ;; the debugger, which will be interpreted as a proper test error.
    (setq debug-on-error t))

  (before-each
    ;; Sanity check that trackers are cleaned up between tests.
    (expect temp-files-created :to-be nil)
    (expect temp-dirs-created :to-be nil)

    ;; Set up gptel configuration for tests
    (setq gptel-model ollama-model)
    (setq gptel-directives `(default . ,system-message))
    (setq gptel-backend
          (gptel-make-ollama
           "Test ollama"
           :host ollama-host
           :models `(,gptel-model)
           ;; Use temperature 0 and a fixed seed to ensure that responses are identical
           ;; across runs. When changing the model (which is expected to be rather weak),
           ;; you might need to play around with different seeds to find one that works.
           ;;
           ;; See https://github.com/ollama/ollama/issues/1749.
           :request-params `(:options (:temperature 0 :seed ,ollama-seed)))))

  (after-each
    ;; Verify that all gptel requests have been aborted or terminated.
    (expect gptel--request-alist :to-be nil)

    ;; Clean up temp files and directories
    (dolist (file temp-files-created)
      (when (and file (file-exists-p file))
        (delete-file file)))
    (dolist (dir temp-dirs-created)
      (when (and dir (file-exists-p dir))
        (delete-directory dir t)))

    ;; Reset trackers
    (setq temp-files-created nil)
    (setq temp-dirs-created nil)

    ;; Restore original gptel settings
    (setq gptel-model original-gptel-model)
    (setq gptel-directives original-gptel-directives)
    (setq gptel-backend original-gptel-backend)

    ;; If debugging output was enabled (otherwise the buffer won't exist), print it.
    (when (get-buffer "*gptel-log*")
      (with-current-buffer "*gptel-log*"
        (message "gptel log output:\n\n%s" (buffer-string)))))

  (after-all
    ;; Restore original global settings.
    (setq project-vc-extra-root-markers original-project-vc-extra-root-markers)
    (setq debug-on-error original-debug-on-error))

  (describe "replace-file operation"
    :var*
    ((replace-file-prompt
      (concat "Replace the entire contents of main.txt with exactly this text: \"Hi there.\""))
     (verify-replace-file
      (lambda (patch-buffer test-files)
        "Verify that PATCH-BUFFER displays a proper replacement diff for TEST-FILES."
        (with-current-buffer patch-buffer
          (let ((content (buffer-substring-no-properties (point-min) (point-max))))
            (if (and (string-match-p "diff --git" content)
                     (string-match-p "Hi there" content)
                     (string-match-p "-Hello" content))
                t
              (cons 'error (format "Patch doesn't contain expected content: %s" content))))))))

    (it "works in single-file mode"
      (funcall test-file-operation replace-file-prompt 'single-file verify-replace-file))

    (it "works in project mode"
      (funcall test-file-operation replace-file-prompt 'project verify-replace-file)))

  (describe "delete-file operation"
    :var*
    ((delete-file-prompt "Delete the file named main.txt.")
     (verify-delete-file
      (lambda (patch-buffer test-files)
        "Verify that PATCH-BUFFER contains a proper deletion diff for TEST-FILES."
        (with-current-buffer patch-buffer
          (let ((content (buffer-substring-no-properties (point-min) (point-max))))
            (if (and (string-match-p "diff --git" content) (string-match-p "/dev/null" content))
                t
              (cons 'error (format "Patch doesn't contain proper deletion: %s" content))))))))

    (it "works in single-file mode"
      (funcall test-file-operation delete-file-prompt 'single-file verify-delete-file))

    (it "works in project mode"
      (funcall test-file-operation delete-file-prompt 'project verify-delete-file)))

  (describe "edit-file operation"
    :var*
    ((edit-file-prompt (concat "Replace the number 123 with the number 456 in main.txt."))
     (verify-edit-file
      (lambda (patch-buffer test-files)
        "Verify that PATCH-BUFFER contains a proper edit diff for TEST-FILES."
        (with-current-buffer patch-buffer
          (let ((content (buffer-substring-no-properties (point-min) (point-max))))
            (if (and (string-match-p "diff --git" content)
                     (string-match-p "-Number: 123" content)
                     (string-match-p "\\+Number: 456" content))
                t
              (cons 'error (format "Patch doesn't contain expected edit: %s" content))))))))

    (it "works in single-file mode"
      (funcall test-file-operation edit-file-prompt 'single-file verify-edit-file))

    (it "works in project mode"
      (funcall test-file-operation edit-file-prompt 'project verify-edit-file)))

  (describe "inline presets"
    :var*
    ((original-gptel--known-presets gptel--known-presets)
     (original-gptel-post-response-functions gptel-post-response-functions)

     (verify-delete-file
      (lambda (patch-buffer test-files)
        "Verify that PATCH-BUFFER contains a proper deletion diff for TEST-FILES."
        (with-current-buffer patch-buffer
          (let ((content (buffer-substring-no-properties (point-min) (point-max))))
            (if (and (string-match-p "diff --git" content) (string-match-p "/dev/null" content))
                t
              (cons 'error (format "Patch doesn't contain proper deletion: %s" content))))))))
    (before-each
      (macher-install))
    (after-each
      (setq gptel--known-presets original-gptel--known-presets)
      (setq gptel-post-response-functions original-gptel-post-response-functions))

    (it "allows editing files using the inline macher preset"
      (let ((post-response nil))
        (with-temp-buffer
          (text-mode)
          (insert "@macher Delete the file named main.txt.")
          (add-hook 'gptel-post-response-functions (lambda (&rest _) (setq post-response t)))
          (gptel-send)

          ;; Wait for the async response.
          (let ((timeout 0))
            (while (and (not post-response) (< timeout test-timeout))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))))))

  (describe "abort functionality"
    (it "properly aborts requests and calls the callback"
      (let* ((temp-file (funcall create-temp-file "Hello, world!"))
             (abort-called nil)
             (test-complete nil)
             (test-result nil)
             (macher-workspace-functions
              (list (lambda () (cons 'file temp-file))))) ;; Use single-file mode.

        ;; Visit the test file.
        (find-file temp-file)

        ;; Set up a test timer to ensure the test completes.
        (let ((test-timer
               (run-with-timer
                3 nil
                (lambda ()
                  (unless test-complete
                    (setq test-complete t)
                    (setq test-result (cons 'error "Test timed out"))))))

              ;; Helper function to check if a request is active for the action buffer.
              (request-is-active-in-action-buffer
               (lambda ()
                 (when-let ((action-buffer (macher-action-buffer)))
                   (cl-some
                    (lambda (entry)
                      ;; Each entry has the form (PROC . (FSM ABORT-FN)). Check if the FSM's :buffer
                      ;; matches our action buffer.
                      (eq
                       (thread-first
                        (cadr entry) ; FSM
                        (gptel-fsm-info) (plist-get :buffer))
                       action-buffer))
                    gptel--request-alist)))))

          ;; Start the implementation with a callback.
          (macher-implement
           "A simple test prompt"
           (lambda (error _execution _fsm)
             ;; The abort signal should be passed to the callback.
             (when (eq error 'abort)
               (setq abort-called t))
             (setq test-complete t)
             (when test-timer
               (cancel-timer test-timer)
               (setq test-timer nil))))

          ;; Wait a moment for the request to start.
          (sit-for 0.1)

          ;; Verify that the request is active before attempting to abort it. This checks
          ;; 'gptel--request-alist' for an entry where the FSM's :buffer matches our
          ;; action buffer, indicating an active request. See `gptel-abort` implementation for
          ;; reference.
          (expect (funcall request-is-active-in-action-buffer) :to-be-truthy)

          ;; Abort the request.
          (macher-abort)

          ;; Wait for the callback to be fired.
          (with-timeout (5 nil)
            (while (not test-complete)
              (sit-for 0.1)))

          ;; Test that the action buffer is visible.
          (let ((action-buffer (macher-action-buffer)))
            (expect (and action-buffer (get-buffer-window action-buffer)) :to-be-truthy))

          ;; Verify that the request in the action buffer was killed. After aborting, there should
          ;; be no active request for this action buffer.
          (expect (funcall request-is-active-in-action-buffer) :to-be nil)

          ;; Cleanup: ensure all requests are terminated and kill buffers
          ;; (temp files will be cleaned up by after-each).
          (when-let ((action-buffer (macher-action-buffer)))
            (when (buffer-live-p action-buffer)
              (gptel-abort action-buffer)))
          (when (buffer-live-p (get-file-buffer temp-file))
            (kill-buffer (get-file-buffer temp-file)))

          ;; Verify that the abort callback was called.
          (expect test-complete :to-be-truthy)
          (expect abort-called :to-be-truthy))))))

;; Local variables:
;; elisp-autofmt-load-packages-local: ("./_defs.el")
;; end:

(provide 'test-functional)
;;; test-functional.el ends here
