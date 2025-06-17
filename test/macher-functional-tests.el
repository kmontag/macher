;;; macher-functional-tests.el --- Functional tests for macher.el -*- lexical-binding: t -*-

;;; Commentary:
;; Functional tests for macher.el that interact with a real LLM on a local ollama server. Uses very
;; simple prompts to ensure that even weak LLMs can handle them correctly.
;;
;; These are not intended to be exhaustive tests of tool functionality. They're intended to catch
;; issues in the general end-to-end implementation workflow.

;;; Code:

(require 'ert)
(require 'macher)

;; Uncomment for debugging output.
;;
(advice-add
 #'gptel--handle-post-insert
 :before
 (lambda (fsm)
   (when-let* ((info (gptel-fsm-info fsm))
               (start-marker (plist-get info :position))
               (tracking-marker (plist-get info :tracking-marker))
               (output-buffer (marker-buffer start-marker)))
     (with-current-buffer output-buffer
       (let ((newly-inserted-text
              (buffer-substring-no-properties
               (marker-position start-marker) (marker-position tracking-marker))))
         (message newly-inserted-text))))))
;; ;; Logs will be printed by `with-macher-test-gptel' if enabled.
;; (setopt gptel-log-level 'info)

;; Helper function to create a temporary file with content.
(defun macher-test-create-temp-file (content &optional dir)
  "Create a temporary file with CONTENT in DIR or 'default-directory'.
Always creates a file named 'main.txt'."
  (let* ((temp-root (or dir (make-temp-file "macher-test-dir" t)))
         ;; The actual project files live in a subdirectory with a predictable name, to ensure
         ;; predictable input to the LLM.
         (temp-dir (expand-file-name "macher-demo" temp-root))
         (temp-file (expand-file-name "main.txt" temp-dir)))
    ;; Create the macher-demo subdirectory
    (make-directory temp-dir t)
    ;; Create the file with content
    (with-temp-file temp-file
      (insert content))
    ;; Return the created filename
    temp-file))

(defconst macher-test--main-file-contents "Hello from main file.\nNumber: 123"
  "The contents of main.txt, for test cases involving both projects and single-file edits.")

(defconst macher-test--gptel-model "qwen2.5-coder:1.5b"
  "The ollama model to use in gptel requests. Must be installed locally.")
(add-hook
 'gptel-prompt-transform-functions
 (lambda ()
   ;; Prompt modifications to help the current model.
   (save-excursion
     (goto-char (point-max))
     ;; (insert "\n/no_think")
     (insert "\nDO NOT wrap your response in a ```json block."))))

;; Macro to set up gptel configuration for tests and execute body within the context.
(defmacro with-macher-test-gptel (&rest body)
  "Configure gptel with a local LLM and execute BODY within this context.
Sets up appropriate let-bindings for 'gptel-model' and 'gptel-backend'."
  `(let* ( ;; Use the simplest possible model that still gives correct responses.
          (gptel-model macher-test--gptel-model)
          ;; Set up the gptel backend for testing.
          (gptel-backend
           (gptel-make-ollama
            "Test ollama"
            :host (or (getenv "MACHER_TEST_OLLAMA_HOST") "localhost:11434")
            :models `(,gptel-model)
            ;; Use temperature 0 and a fixed seed to ensure that responses are identical
            ;; across runs. When changing the model (which is expected to be rather weak),
            ;; you might need to play around with different seeds to find one that works.
            ;;
            ;; See https://github.com/ollama/ollama/issues/1749.
            :request-params '(:options (:temperature 0 :seed 7890)))))
     ,@body

     ;; If debugging output was enabled (otherwise the buffer won't exist), print it.
     (when (get-buffer "*gptel-log*")
       (with-current-buffer "*gptel-log*"
         (message "gptel log output:\n\n%s" (buffer-string))))))

;; Helper function to create a temporary project directory with files.
(defun macher-test-create-temp-project ()
  "Create a temporary directory with test files for project cases.
Returns a plist with :dir, :files, and :cleanup function."
  (let* ((temp-root (make-temp-file "macher-test-proj" t))
         (temp-dir (expand-file-name "macher-demo" temp-root))
         (main-file (expand-file-name "main.txt" temp-dir))
         (extra-file (expand-file-name "extra.txt" temp-dir))
         (project-file (expand-file-name ".project" temp-dir))
         (files (list main-file extra-file project-file)))

    ;; Create the macher-demo subdirectory
    (make-directory temp-dir t)
    ;; Create the test files.
    (with-temp-file main-file
      (insert macher-test--main-file-contents))
    (with-temp-file extra-file
      (insert "This is an extra file."))
    ;; Create .project file to ensure the directory is detected as a project.
    (with-temp-file project-file
      (insert ""))

    ;; Return information about the project.
    (list
     :dir temp-dir
     :files files
     :cleanup
     (lambda ()
       (dolist (file files)
         (when (file-exists-p file)
           (delete-file file)))
       (when (file-exists-p temp-dir)
         (delete-directory temp-dir t))
       (when (file-exists-p temp-root)
         (delete-directory temp-root t))))))

;; Base test function for all file operations.
(defun macher-test-file-operation (prompt file-mode callback-test)
  "Test a LLM operation with PROMPT in FILE-MODE with CALLBACK-TEST.
PROMPT is the instruction to send to the LLM.
FILE-MODE is either 'single-file or 'project.
CALLBACK-TEST is a function that verifies the result."
  (let* ((is-project (eq file-mode 'project))
         (test-vars
          (if is-project
              (macher-test-create-temp-project)
            (let* ((temp-file (macher-test-create-temp-file macher-test--main-file-contents))
                   (temp-dir (file-name-directory temp-file))
                   (temp-root (file-name-directory (directory-file-name temp-dir))))
              (list
               :files (list temp-file)
               :dir temp-dir
               :cleanup
               (lambda ()
                 (delete-file temp-file)
                 (when (file-exists-p temp-dir)
                   (delete-directory temp-dir t))
                 (when (file-exists-p temp-root)
                   (delete-directory temp-root t)))))))
         (test-dir (plist-get test-vars :dir))
         (test-files (plist-get test-vars :files))
         (cleanup-fn (plist-get test-vars :cleanup))
         (main-file (car test-files))
         (patch-buffer nil)
         (test-complete nil)
         (test-result nil)
         ;; Allow project.el to detect projects with .project marker file in the root.
         (project-vc-extra-root-markers '(".project")))

    (unwind-protect
        (progn
          ;; Visit the first file.
          (find-file main-file)

          ;; Set up the test environment.
          (let ((test-timer nil)
                ;; Timeout needs to be pretty permissive, as this needs to run on GH actions.
                (test-timeout 300))

            ;; Temporarily override gptel settings to use local LLM.
            (with-macher-test-gptel
             ;; Use a callback to check the results.
             (macher-implement
              prompt
              (lambda (error _context)
                (setq test-complete t)
                (when test-timer
                  (cancel-timer test-timer)
                  (setq test-timer nil))

                (if error
                    (setq test-result (cons 'error error))
                  ;; Test was successful.
                  (setq patch-buffer (macher--patch-buffer))
                  (setq test-result
                        (if (and patch-buffer (buffer-live-p patch-buffer))
                            ;; Run the specific test for this operation.
                            (funcall callback-test patch-buffer test-files)
                          (cons 'error "No patch buffer created")))))))

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
                (sit-for 0.1)))))

      ;; Cleanup: kill any buffers and delete temp files.
      (when (buffer-live-p (get-file-buffer main-file))
        (kill-buffer (get-file-buffer main-file)))
      (when (and patch-buffer (buffer-live-p patch-buffer))
        (kill-buffer patch-buffer))
      (funcall cleanup-fn))

    ;; Check result.
    (should test-complete)
    (should-not (consp test-result))))


(let ((replace-file-prompt
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

  ;; Functional tests for replace-file operation.
  (ert-deftest macher-functional-test-replace-file-single ()
    "Test the replace-file functionality in single-file mode."
    (macher-test-file-operation replace-file-prompt 'single-file verify-replace-file))

  (ert-deftest macher-functional-test-replace-file-project ()
    "Test the replace-file functionality in project mode."
    (macher-test-file-operation replace-file-prompt 'project verify-replace-file)))

;; Create a let block for delete-file operation with its verification function.
(let ((delete-file-prompt "Delete the file named main.txt.")
      (verify-delete-file
       (lambda (patch-buffer test-files)
         "Verify that PATCH-BUFFER contains a proper deletion diff for TEST-FILES."
         (with-current-buffer patch-buffer
           (let ((content (buffer-substring-no-properties (point-min) (point-max))))
             (if (and (string-match-p "diff --git" content) (string-match-p "/dev/null" content))
                 t
               (cons 'error (format "Patch doesn't contain proper deletion: %s" content))))))))

  ;; Functional tests for delete-file operation.
  (ert-deftest macher-functional-test-delete-file-single ()
    "Test the delete-file functionality in single-file mode."
    (macher-test-file-operation delete-file-prompt 'single-file verify-delete-file))

  (ert-deftest macher-functional-test-delete-file-project ()
    "Test the delete-file functionality in project mode."
    (macher-test-file-operation delete-file-prompt 'project verify-delete-file)))

(let ((edit-file-prompt (concat "Replace the number 123 with the number 456 in main.txt."))
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

  (ert-deftest macher-functional-test-edit-file-single ()
    "Test the edit-file functionality in single-file mode."
    (macher-test-file-operation edit-file-prompt 'single-file verify-edit-file))
  (ert-deftest macher-functional-test-edit-file-project ()
    "Test the edit-file functionality in project mode."
    (macher-test-file-operation edit-file-prompt 'project verify-edit-file)))

(ert-deftest macher-functional-test-abort ()
  "Test that `macher-abort` properly aborts requests and calls the callback."
  (let* ((temp-file (macher-test-create-temp-file "Hello, world!"))
         (abort-called nil)
         (abort-written-to-output-buffer nil)
         (test-complete nil)
         (test-result nil)
         (macher-workspace-hook
          (list (lambda () (cons 'file temp-file))))) ;; Use single-file mode.

    (unwind-protect
        (progn
          ;; Visit the test file.
          (find-file temp-file)

          ;; Set up a test timer to ensure the test completes.
          (let ((test-timer
                 (run-with-timer
                  3 nil
                  (lambda ()
                    (unless test-complete
                      (setq test-complete t)
                      (setq test-result (cons 'error "Test timed out")))))))

            ;; Override gptel settings.
            (with-macher-test-gptel

             ;; Start the implementation with a callback.
             (macher-implement
              "A simple test prompt"
              (lambda (error context)
                ;; The abort signal should be passed to the callback.
                (when (eq error 'abort)
                  (setq abort-called t))
                (setq test-complete t)
                (when test-timer
                  (cancel-timer test-timer)
                  (setq test-timer nil))))

             ;; Wait a moment for the request to start.
             (sit-for 0.1)

             ;; Abort the request.
             (macher-abort)

             ;; Wait for the test to complete.
             (with-timeout (5 nil)
               (while (not test-complete)
                 (sit-for 0.1)))))

          ;; Check if "Aborted" appears in the output buffer.
          (with-current-buffer (macher--output-buffer)
            (setq abort-written-to-output-buffer (string-match-p "Aborted" (buffer-string))))

          ;; Cleanup: kill buffer and delete temp file.
          (when (buffer-live-p (get-file-buffer temp-file))
            (kill-buffer (get-file-buffer temp-file)))
          (let* ((temp-dir (file-name-directory temp-file))
                 (temp-root (file-name-directory (directory-file-name temp-dir))))
            (delete-file temp-file)
            (when (file-exists-p temp-dir)
              (delete-directory temp-dir t))
            (when (file-exists-p temp-root)
              (delete-directory temp-root t))))

      ;; Verify that the abort callback was called.
      (should test-complete)
      (should abort-called)
      (should abort-written-to-output-buffer))))

(provide 'macher-functional-tests)
;;; macher-functional-tests.el ends here
