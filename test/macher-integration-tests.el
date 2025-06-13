;;; macher-integration-tests.el --- Integration tests for macher.el -*- lexical-binding: t -*-

;;; Commentary:
;; Integration tests for macher.el functionality that test the full pipeline
;; including context generation and formatting.

;;; Code:

(require 'ert)
(require 'macher)
(require 'cl-lib)
(require 'gptel)

;; (setq ert-batch-backtrace-right-margin nil)
;; (setq gptel-log-level 'debug)
;; (advice-add #'gptel--log :before (lambda (data &optional type no-json) (message "%s %s" data type)))

;;; Stubbed gptel backend implementation:

(cl-defstruct
 (macher-test-backend
  (:constructor macher-test--make-backend) (:copier nil) (:include gptel-backend))
 "A stubbed gptel backend for testing."
 (responses nil) ; List of responses to provide
 (current-index 0) ; Current response index
 (received-requests nil)) ; List of requests received by the backend

(defun macher-test--create-backend (backend-id responses)
  "Create a stubbed backend with BACKEND-ID that provides RESPONSES.

BACKEND-ID should be an integer used to generate unique hostnames and
names. This will be used to identify the backend associated with mocked
HTTP calls.

RESPONSES should be a list where each element is either:
- A string (successful text response)
- A plist with :error key (error response)
- A plist with :tool-calls key (tool call response)"
  (let* ((hostname (format "test-backend-%d.test" backend-id))
         (name (format "Test Backend %d" backend-id))
         (backend
          (macher-test--make-backend
           :name name
           :host hostname
           :protocol "http"
           :endpoint "/test"
           :models '(test-model)
           :stream nil
           :url (format "http://%s/test" hostname)
           :responses responses
           :current-index 0
           :received-requests nil)))
    ;; Register the backend in gptel's known backends.
    (setf (alist-get name gptel--known-backends nil nil #'equal) backend)
    backend))

(cl-defmethod gptel--parse-response ((backend macher-test-backend) response info)
  "Parse RESPONSE for the test BACKEND."
  (setq inhibit-message nil)
  (let ((message (plist-get response :message))
        (error-data (plist-get response :error)))
    (cond
     (message
      ;; Normal response with content.
      (plist-get message :content))
     (error-data
      ;; Error response - store error info in INFO plist and return nil.
      (plist-put info :error error-data)
      nil)
     (t
      ;; Neither message nor error - something's wrong.
      nil))))

(cl-defmethod gptel--parse-buffer ((backend macher-test-backend) max-entries)
  "Parse current buffer backwards from point and return a list of prompt.

BACKEND is the LLM backend in use.

MAX-ENTRIES is the number of queries/responses to include for
context."
  (let ((content (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (list (list :role "user" :content content))))

(cl-defmethod gptel--request-data ((backend macher-test-backend) prompts)
  "Generate request data for the test BACKEND.

PROMPTS is the plist of previous user queries and LLM responses."
  ;; The system prompt needs to be added explicitly; see e.g. the implementation in 'gptel-ollama'
  ;; for an example.
  (when gptel--system-message
    (push (list :role "system" :content gptel--system-message) prompts))
  (let* ((prompts-plist
          (gptel--merge-plists
           `(:model "test-model" :messages [,@prompts])
           (gptel-backend-request-params gptel-backend)
           (gptel--model-request-params gptel-model))))

    (when (and gptel-use-tools gptel-tools)
      (plist-put prompts-plist :tools (gptel--parse-tools backend gptel-tools)))

    prompts-plist))

(cl-defmethod gptel--parse-tool-results ((_backend macher-test-backend) tool-use)
  "Return a prompt containing tool call results in TOOL-USE."
  (mapcar (lambda (tool-call) (list :role "tool" :content (plist-get tool-call :result))) tool-use))

(defun macher-test--get-next-response (backend)
  "Get the next response from BACKEND's response list."
  (let* ((responses (macher-test-backend-responses backend))
         (index (macher-test-backend-current-index backend))
         (response (nth index responses)))
    ;; Increment index for next call, cycling back to 0 if at end.
    (setf (macher-test-backend-current-index backend) (mod (1+ index) (max 1 (length responses))))
    response))

(defun macher-test--get-received-requests (backend)
  "Get the list of requests received by BACKEND."
  (reverse (macher-test-backend-received-requests backend)))

(defun macher-test--mock-url-retrieve (url callback &optional cbargs silent inhibit-cookies)
  "Mock implementation of `url-retrieve' for testing.
Creates a fake HTTP response buffer and calls CALLBACK.

URL is the request URL used to identify the backend.
CALLBACK is the function to call when response is ready.
CBARGS are additional arguments passed to CALLBACK.
SILENT and INHIBIT-COOKIES are ignored in this mock implementation."
  (let* (
         ;; gptel normally inhibits messages at this point; re-enable them to avoid confusion when
         ;; adding debugging output.
         (inhibit-message nil)
         (parsed-url (url-generic-parse-url url))
         (hostname (url-host parsed-url))
         ;; Extract backend name from hostname pattern test-backend-N.test.
         (backend-name
          (when (string-match "^test-backend-\\([0-9]+\\)\\.test$" hostname)
            (format "Test Backend %s" (match-string 1 hostname))))
         (backend
          (when backend-name
            (gptel-get-backend backend-name)))
         (response-buffer (generate-new-buffer " *test-response*")))
    (unless backend
      (error
       "No macher-test-backend found for hostname: %s (backend-name: %s)" hostname backend-name))

    ;; Store the request data on the correct backend instance.
    (when url-request-data
      (let* ((request-data (json-parse-string url-request-data :object-type 'plist))
             (messages (plist-get request-data :messages))
             (tools (plist-get request-data :tools))
             (stored-request
              (list
               :model (plist-get request-data :model)
               :messages
               (if (vectorp messages)
                   (append messages nil)
                 messages))))
        (when tools
          (plist-put stored-request :tools tools))
        (setf (macher-test-backend-received-requests backend)
              (cons stored-request (macher-test-backend-received-requests backend)))))

    (with-current-buffer response-buffer
      (let* ((response (macher-test--get-next-response backend))
             (is-error (and (plistp response) (plist-get response :error))))
        (cond
         (is-error
          ;; Send actual HTTP error status for error responses.
          (insert "HTTP/1.1 400 Bad Request\r\n")
          (insert "Content-Type: application/json\r\n")
          (insert "\r\n")
          (setq url-http-end-of-headers (point))
          (insert (json-encode `(:error ,is-error)))
          (setq url-http-response-status 400))
         (t
          ;; Normal successful response.
          (insert "HTTP/1.1 200 OK\r\n")
          (insert "Content-Type: application/json\r\n")
          (insert "\r\n")
          (setq url-http-end-of-headers (point))
          (cond
           ((stringp response)
            ;; Simple text response.
            (insert (json-encode `(:message (:content ,response)))))
           ((plist-get response :tool-calls)
            ;; Tool call response.
            (insert (json-encode `(:message (:tool_calls ,(plist-get response :tool-calls))))))
           (t
            (user-error (format "Unrecognized response format: %s" response))))
          (setq url-http-response-status 200)))
        (goto-char (point-min))))

    ;; Call the callback asynchronously to simulate network behavior.
    (run-at-time 0.01 nil
                 (lambda (_)
                   (with-current-buffer response-buffer
                     (apply callback
                            ;; `url-retrieve' would normally pass a STATUS plist containing a list
                            ;; of events during the request, but it's not currently used by gptel.
                            '() cbargs)))
                 nil)

    response-buffer))

(defvar macher-test--backend-counter 0
  "Counter for generating unique backend names in tests.")

(defmacro macher-test-with-stubbed-backend (responses &rest body)
  "Execute BODY with a stubbed gptel backend that provides RESPONSES.

RESPONSES should be a list of response specifications.
The macro sets up the backend, disables curl, and restores the
original state after BODY completes."
  (declare (indent 1))
  `(let* ((gptel-use-curl nil) ; Disable curl to force use of url-retrieve.
          (backend-id (cl-incf macher-test--backend-counter))
          (backend-name (format "Test Backend %d" backend-id))
          (gptel-backend (macher-test--create-backend backend-id ,responses))
          (gptel-model 'test-model)
          ;; Allow project.el to detect projects with this marker file in the root.
          (project-vc-extra-root-markers '(".project")))
     (unwind-protect
         (progn
           ;; Replace url-retrieve with our mock using advice.
           (advice-add 'url-retrieve :override #'macher-test--mock-url-retrieve)
           ,@body)
       ;; Clean up: remove the advice.
       (advice-remove 'url-retrieve #'macher-test--mock-url-retrieve))))

;;; Sanity-check tests:

(ert-deftest macher-test-stubbed-backend-basic ()
  "Test that the stubbed backend can receive and respond to basic requests."
  (let ((response-received nil)
        (callback-called nil)
        (temp-file (make-temp-file "macher-test-basic-"))
        (test-buffer nil))
    (unwind-protect
        (macher-test-with-stubbed-backend '("Hello, World!")
          (setq test-buffer (find-file-noselect temp-file))
          (with-current-buffer test-buffer
            (macher-send
             "Test prompt"
             (lambda (error context)
               (setq callback-called t)
               (setq response-received (not error))))

            ;; Wait for the async response.
            (let ((timeout 0))
              (while (and (not callback-called) (< timeout 100))
                (sleep-for 0.1)
                (setq timeout (1+ timeout))))

            (should callback-called)
            (should response-received)

            ;; Validate that the request contains the actual prompt.
            (let ((requests (macher-test--get-received-requests gptel-backend)))
              (should (> (length requests) 0))
              (let* ((request (car requests))
                     (messages (plist-get request :messages)))
                (should messages)
                (should (> (length messages) 0))
                ;; Check that at least one message contains our test prompt.
                (should
                 (cl-some
                  (lambda (msg)
                    (and (string= (plist-get msg :role) "user")
                         (string-match-p "Test prompt" (plist-get msg :content))))
                  messages))))))
      ;; Clean up.
      (when test-buffer
        (kill-buffer test-buffer))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest macher-test-stubbed-backend-error-response ()
  "Test that error responses get propagated to the callback."
  (let ((error-received nil)
        (callback-called nil)
        (context-received nil)
        (temp-file (make-temp-file "macher-test-error-"))
        (test-buffer nil))
    (unwind-protect

        (macher-test-with-stubbed-backend '((:error
                                             (:type "test_error" :message "Test error message")))
          (setq test-buffer (find-file-noselect temp-file))
          (with-current-buffer test-buffer
            (macher-send
             "Test prompt"
             (lambda (error context)
               (setq callback-called t)
               (setq error-received error)
               (setq context-received context)))

            ;; Wait for the async response.
            (let ((timeout 0))
              (while (and (not callback-called) (< timeout 100))
                (sleep-for 0.1)
                (setq timeout (1+ timeout))))

            (should callback-called)
            (should error-received)
            (should context-received)
            ;; Verify the context is a macher-context struct.
            (should (macher-context-p context-received))))
      ;; Clean up.
      (when test-buffer
        (kill-buffer test-buffer))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest macher-test-stubbed-backend-multiple-responses ()
  "Test that the stubbed backend cycles through multiple responses."
  (let ((responses-received '())
        (callbacks-completed 0)
        (temp-file-1 (make-temp-file "macher-test-1-"))
        (temp-file-2 (make-temp-file "macher-test-2-"))
        (test-buffer-1 nil)
        (test-buffer-2 nil))
    (unwind-protect
        (macher-test-with-stubbed-backend '("First response" "Second response")
          ;; Make first request.
          (setq test-buffer-1 (find-file-noselect temp-file-1))
          (with-current-buffer test-buffer-1
            (macher-send
             "First prompt"
             (lambda (error context)
               (push (if error
                         "ERROR"
                       "SUCCESS")
                     responses-received)
               (setq callbacks-completed (1+ callbacks-completed))))

            ;; Wait for first response.
            (let ((timeout 0))
              (while (and (< callbacks-completed 1) (< timeout 100))
                (sleep-for 0.1)
                (setq timeout (1+ timeout)))))

          ;; Make second request.
          (setq test-buffer-2 (find-file-noselect temp-file-2))
          (with-current-buffer test-buffer-2
            (macher-send
             "Second prompt"
             (lambda (error context)
               (push (if error
                         "ERROR"
                       "SUCCESS")
                     responses-received)
               (setq callbacks-completed (1+ callbacks-completed))))

            ;; Wait for second response.
            (let ((timeout 0))
              (while (and (< callbacks-completed 2) (< timeout 100))
                (sleep-for 0.1)
                (setq timeout (1+ timeout)))))

          (should (= callbacks-completed 2))
          (should (equal responses-received '("SUCCESS" "SUCCESS"))))
      ;; Clean up.
      (when test-buffer-1
        (kill-buffer test-buffer-1))
      (when test-buffer-2
        (kill-buffer test-buffer-2))
      (when (file-exists-p temp-file-1)
        (delete-file temp-file-1))
      (when (file-exists-p temp-file-2)
        (delete-file temp-file-2)))))

;;; Tests of macher functionality:

(ert-deftest macher-test-prepare-context-files ()
  "Test that context files in workspace get implementation buffers created."
  (let ((response-received nil)
        (callback-called nil)
        (temp-project-dir (make-temp-file "macher-test-context-" t))
        (test-buffer nil)
        (main-file nil)
        (context-file nil))
    (unwind-protect
        (macher-test-with-stubbed-backend '("Test response")
          ;; Create a project structure.
          (let ((src-dir (expand-file-name "src" temp-project-dir)))
            ;; Create directories.
            (make-directory src-dir)

            ;; Create project files.
            (setq main-file (expand-file-name "main.el" src-dir))
            (setq context-file (expand-file-name "context.el" src-dir))
            (with-temp-file main-file
              (insert ";;; main.el --- Main file\n\n")
              (insert "Main file content\n")
              (insert "\n(provide 'main)\n;;; main.el ends here\n"))
            (with-temp-file context-file
              (insert ";;; context.el --- Context file\n\n")
              (insert "Context file content\n")
              (insert "\n(provide 'context)\n;;; context.el ends here\n"))

            ;; Create .project file to ensure the directory is detected as a project.
            (with-temp-file (expand-file-name ".project" temp-project-dir)
              (insert ""))

            ;; Open the main file for testing.
            (setq test-buffer (find-file-noselect main-file))
            (with-current-buffer test-buffer
              ;; Add the context file to gptel context (but not the main file).
              (gptel-add-file context-file)

              ;; Mock the context buffers creation to verify it's called.
              (let ((buffers-created '()))
                (advice-add
                 'macher-context-buffers-for-file
                 :before (lambda (path context) (push path buffers-created)))

                (macher-send
                 "Test prompt"
                 (lambda (error context)
                   (setq callback-called t)
                   (setq response-received (not error))))

                ;; Wait for the async response.
                (let ((timeout 0))
                  (while (and (not callback-called) (< timeout 100))
                    (sleep-for 0.1)
                    (setq timeout (1+ timeout))))

                (should callback-called)
                (should response-received)

                ;; Verify that implementation buffers were created for the context file.
                (should (member context-file buffers-created))
                ;; Verify that main file was not processed (not in context).
                (should-not (member main-file buffers-created))

                (advice-remove 'macher-context-buffers-for-file :before)))))
      ;; Clean up.
      (when test-buffer
        (kill-buffer test-buffer))
      (when (and temp-project-dir (file-exists-p temp-project-dir))
        (delete-directory temp-project-dir t))
      (gptel-context-remove-all))))

(ert-deftest macher-test-context-string-includes-project-info ()
  "Test that requests include workspace description with file listings and context indicators."
  (let ((response-received nil)
        (callback-called nil)
        (temp-project-dir (make-temp-file "macher-test-project-" t))
        (test-buffer nil)
        (request-content nil)
        ;; Explicitly make sure the context shows up in a system message.
        (gptel-use-context 'system)
        (main-file nil))
    (unwind-protect
        (macher-test-with-stubbed-backend '("Test response")
          ;; Create a realistic project structure.
          (let ((readme-file (expand-file-name "README.md" temp-project-dir))
                (src-dir (expand-file-name "src" temp-project-dir))
                (test-dir (expand-file-name "test" temp-project-dir)))
            ;; Create directories.
            (make-directory src-dir)
            (make-directory test-dir)

            ;; Create project files.
            (setq main-file (expand-file-name "main.el" src-dir))
            (with-temp-file readme-file
              (insert "# Test Project\n\nThis is a test project for macher integration tests.\n"))
            (with-temp-file main-file
              (insert ";;; main.el --- Main file for test project\n\n")
              (insert "Test file content for context test\n")
              (insert "\n(provide 'main)\n;;; main.el ends here\n"))
            (with-temp-file (expand-file-name "test-file.el" test-dir)
              (insert ";;; test-file.el --- Test file\n\n")
              (insert "(require 'main)\n\n")
              (insert "(provide 'test-file)\n;;; test-file.el ends here\n"))

            ;; Create .project file to ensure the directory is detected as a project.
            (with-temp-file (expand-file-name ".project" temp-project-dir)
              (insert ""))

            ;; Open the main file for testing.
            (setq test-buffer (find-file-noselect main-file))
            (with-current-buffer test-buffer
              ;; Add the current file to gptel context.
              (gptel-add-file main-file)

              (macher-send
               "Test prompt with context"
               (lambda (error context)
                 (setq callback-called t)
                 (setq response-received (not error))))

              ;; Wait for the async response.
              (let ((timeout 0))
                (while (and (not callback-called) (< timeout 100))
                  (sleep-for 0.1)
                  (setq timeout (1+ timeout))))

              (should callback-called)
              (should response-received)

              ;; Validate that the request contains enhanced context information.
              (let ((requests (macher-test--get-received-requests gptel-backend)))
                (should (> (length requests) 0))
                (let* ((request (car requests))
                       (messages (plist-get request :messages))
                       (system-messages
                        (cl-remove-if-not
                         (lambda (msg) (string= (plist-get msg :role) "system")) messages))
                       (user-messages
                        (cl-remove-if-not
                         (lambda (msg) (string= (plist-get msg :role) "user")) messages)))
                  (should messages)
                  (should (> (length messages) 0))
                  (should (> (length system-messages) 0))
                  (should (> (length user-messages) 0))

                  ;; Verify that context appears in system message.
                  (let ((system-content
                         (mapconcat (lambda (msg) (plist-get msg :content)) system-messages " ")))
                    ;; Check for workspace description.
                    (should (string-match-p "WORKSPACE CONTEXT" system-content))

                    ;; Check that the workspace contains all project files with full relative paths,
                    ;; including context indicator(s) where appropriate.
                    (should (string-match-p "  README.md" system-content))
                    (should (string-match-p "\\[\\*\\] src/main.el" system-content))
                    (should (string-match-p "  test/test-file.el" system-content))

                    ;; Check that it mentions the project name in the description.
                    (should
                     (string-match-p
                      (format "In-memory editing environment for '%s'"
                              (file-name-nondirectory temp-project-dir))
                      system-content)))

                  ;; Verify that user prompt appears in user message.
                  (let ((user-content
                         (mapconcat (lambda (msg) (plist-get msg :content)) user-messages " ")))
                    (should (string-match-p "Test prompt with context" user-content))))))))
      ;; Clean up.
      (when test-buffer
        (kill-buffer test-buffer))
      (when (and temp-project-dir (file-exists-p temp-project-dir))
        (delete-directory temp-project-dir t))
      (gptel-context-remove-all))))

;;; Context string generation stress tests:

;;; Helper functions for context tests:

(defun macher-test--create-project-structure (temp-dir files)
  "Create a project structure in TEMP-DIR with FILES.
FILES is an alist of (relative-path . content) pairs."
  ;; Create .project file to ensure the directory is detected as a project.
  (with-temp-file (expand-file-name ".project" temp-dir)
    (insert ""))

  ;; Create each file and its directory structure.
  (dolist (file-spec files)
    (let* ((rel-path (car file-spec))
           (content (cdr file-spec))
           (full-path (expand-file-name rel-path temp-dir))
           (dir (file-name-directory full-path)))
      ;; Ensure directory exists.
      (when dir
        (make-directory dir t))
      ;; Create file with content.
      (with-temp-file full-path
        (insert content)))))

(defun macher-test--setup-context-test (test-name file-specs)
  "Set up a context test with TEST-NAME and FILE-SPECS.
Returns a plist with :temp-dir, :main-file, and :test-buffer."
  (let*
      ((temp-project-dir (make-temp-file (format "macher-test-%s-" test-name) t))
       (main-file (expand-file-name "src/main.el" temp-project-dir))
       (default-files
        `(("src/main.el"
           .
           ";;; main.el --- Main file\n\nMain file content\n\n(provide 'main)\n;;; main.el ends here\n")
          ("src/other.el"
           .
           ";;; other.el --- Other file\n\nOther file content\n\n(provide 'other)\n;;; other.el ends here\n")))
       (all-files (append file-specs default-files))
       (test-buffer nil))

    ;; Create project structure.
    (macher-test--create-project-structure temp-project-dir all-files)

    ;; Open the main file for testing.
    (setq test-buffer (find-file-noselect main-file))
    (list :temp-dir temp-project-dir :main-file main-file :test-buffer test-buffer)))

(defun macher-test--cleanup-context-test (setup-info &optional extra-buffers)
  "Clean up after a context test using SETUP-INFO.
EXTRA-BUFFERS is a list of additional buffers to clean up."
  (let ((test-buffer (plist-get setup-info :test-buffer))
        (temp-dir (plist-get setup-info :temp-dir)))
    ;; Clean up buffers.
    (when test-buffer
      (kill-buffer test-buffer))
    (dolist (buffer extra-buffers)
      (when buffer
        (kill-buffer buffer)))
    ;; Clean up temp directory.
    (when (and temp-dir (file-exists-p temp-dir))
      (delete-directory temp-dir t))
    ;; Clean up gptel context.
    (gptel-context-remove-all)))

(defun macher-test--run-context-test (setup-info prompt expected-checks &optional context-setup-fn)
  "Run a context test with SETUP-INFO, PROMPT, and EXPECTED-CHECKS.
CONTEXT-SETUP-FN is an optional function to set up gptel context.
EXPECTED-CHECKS is a list of check functions that take system-content as argument.
Returns t if all checks pass."
  (let ((response-received nil)
        (callback-called nil)
        (test-buffer (plist-get setup-info :test-buffer))
        ;; Explicitly make sure the context shows up in a system message.
        (gptel-use-context 'system))

    (with-current-buffer test-buffer
      ;; Set up context if function provided.
      (when context-setup-fn
        (funcall context-setup-fn setup-info))

      (macher-send
       prompt
       (lambda (error context)
         (setq callback-called t)
         (setq response-received (not error))))

      ;; Wait for the async response.
      (let ((timeout 0))
        (while (and (not callback-called) (< timeout 100))
          (sleep-for 0.1)
          (setq timeout (1+ timeout))))

      (should callback-called)
      (should response-received)

      ;; Validate that the request contains expected content.
      (let ((requests (macher-test--get-received-requests gptel-backend)))
        (should (> (length requests) 0))
        (let* ((request (car requests))
               (messages (plist-get request :messages))
               (system-messages
                (cl-remove-if-not
                 (lambda (msg) (string= (plist-get msg :role) "system")) messages)))
          (should messages)
          (should (> (length system-messages) 0))

          ;; Run all expected checks.
          (let ((system-content
                 (mapconcat (lambda (msg) (plist-get msg :content)) system-messages " ")))
            (dolist (check expected-checks)
              (funcall check system-content))))))))

(ert-deftest macher-test-context-string-no-context ()
  "Test that workspace info is included when nothing is in the context."
  (let ((setup-info nil))
    (unwind-protect
        (macher-test-with-stubbed-backend '("Test response")
          (setq setup-info (macher-test--setup-context-test "no-context" nil))

          (macher-test--run-context-test
           setup-info
           "Test prompt with no context"
           (list
            ;; Check for workspace description.
            (lambda (content) (should (string-match-p "WORKSPACE CONTEXT" content)))
            ;; Check that the workspace contains all project files.
            (lambda (content) (should (string-match-p "src/main.el" content)))
            (lambda (content) (should (string-match-p "src/other.el" content)))
            ;; Check that no files are marked with [*] since nothing is in context.
            (lambda (content) (should-not (string-match-p "\\[\\*\\]" content)))
            ;; Check that it mentions the project name in the description.
            (lambda (content)
              (should
               (string-match-p
                (format "In-memory editing environment for '%s'"
                        (file-name-nondirectory (plist-get setup-info :temp-dir)))
                content))))
           ;; Context setup function - ensure no context.
           (lambda (setup-info) (gptel-context-remove-all))))
      ;; Clean up.
      (when setup-info
        (macher-test--cleanup-context-test setup-info)))))

(ert-deftest macher-test-context-string-buffers-only ()
  "Test that workspace info is included when only buffers are in the context."
  (let ((setup-info nil)
        (other-buffer nil))
    (unwind-protect
        (macher-test-with-stubbed-backend '("Test response")
          (setq
           setup-info
           (macher-test--setup-context-test
            "buffers"
            '(("src/third.el"
               .
               ";;; third.el --- Third file\n\nThird file content\n\n(provide 'third)\n;;; third.el ends here\n"))))

          ;; Open another buffer for context.
          (setq other-buffer
                (find-file-noselect
                 (expand-file-name "src/other.el" (plist-get setup-info :temp-dir))))

          (macher-test--run-context-test
           setup-info
           "Test prompt with buffer context"
           (list
            ;; Check for workspace description.
            (lambda (content) (should (string-match-p "WORKSPACE CONTEXT" content)))
            ;; Check that all files are present but not marked as context (including other.el, which
            ;; is present in context but only as a buffer).
            (lambda (content) (should (string-match-p "    src/main.el" content)))
            (lambda (content) (should (string-match-p "    src/third.el" content)))
            (lambda (content) (should (string-match-p "    src/other.el" content)))
            ;; Check that it mentions the project name in the description.
            (lambda (content)
              (should
               (string-match-p
                (format "In-memory editing environment for '%s'"
                        (file-name-nondirectory (plist-get setup-info :temp-dir)))
                content))))
           ;; Context setup function - add only buffers to context.
           (lambda (setup-info)
             (gptel-context-remove-all)
             (with-current-buffer other-buffer
               (gptel-add)))))
      ;; Clean up.
      (when setup-info
        (macher-test--cleanup-context-test setup-info (list other-buffer))))))

(ert-deftest macher-test-context-string-mixed-context ()
  "Test that workspace info is included when both buffers and files are in the context."
  (let ((setup-info nil)
        (buffer-buffer nil))
    (unwind-protect
        (macher-test-with-stubbed-backend '("Test response")
          (setq
           setup-info
           (macher-test--setup-context-test
            "mixed"
            '(("src/context.el"
               .
               ";;; context.el --- Context file\n\nContext file content\n\n(provide 'context)\n;;; context.el ends here\n")
              ("src/buffer.el"
               .
               ";;; buffer.el --- Buffer file\n\nBuffer file content\n\n(provide 'buffer)\n;;; buffer.el ends here\n")
              ("docs/README.md" . "# Test Project\n\nThis is a test project.\n"))))

          ;; Open buffer for context.
          (setq buffer-buffer
                (find-file-noselect
                 (expand-file-name "src/buffer.el" (plist-get setup-info :temp-dir))))

          (macher-test--run-context-test
           setup-info
           "Test prompt with mixed context"
           (list
            ;; Check for workspace description.
            (lambda (content) (should (string-match-p "WORKSPACE CONTEXT" content)))
            ;; Check that the context file is marked with [*].
            (lambda (content) (should (string-match-p "\\[\\*\\] src/context.el" content)))
            ;; Check that other files are not marked, including the one that was loaded as a buffer.
            (lambda (content) (should (string-match-p "    src/buffer.el" content)))
            (lambda (content) (should (string-match-p "    docs/README.md" content)))
            (lambda (content) (should (string-match-p "    src/main.el" content)))
            (lambda (content) (should (string-match-p "    src/other.el" content)))
            ;; Check that it mentions the project name in the description.
            (lambda (content)
              (should
               (string-match-p
                (format "In-memory editing environment for '%s'"
                        (file-name-nondirectory (plist-get setup-info :temp-dir)))
                content))))
           ;; Context setup function - add both files and buffers to context.
           (lambda (setup-info)
             (gptel-context-remove-all)
             (gptel-add-file (expand-file-name "src/context.el" (plist-get setup-info :temp-dir)))
             (with-current-buffer buffer-buffer
               (gptel-add)))))
      ;; Clean up.
      (when setup-info
        (macher-test--cleanup-context-test setup-info (list buffer-buffer))))))

;;; Tool cleanup tests:

(ert-deftest macher-test-tool-lifecycle ()
  "Test that tools are properly created and cleaned up during requests.

For now this needs to peek into 'gptel--known-tools'."
  (let ((callback-called nil)
        (response-received nil)
        (temp-dir (make-temp-file "macher-test-" t))
        (test-buffer nil))
    (unwind-protect
        (macher-test-with-stubbed-backend '("Test response")
          ;; Create a test.txt file in the temp directory (no .project file).
          (let ((test-file (expand-file-name "test.txt" temp-dir)))
            (with-temp-file test-file
              (insert "Test content\n"))

            (setq test-buffer (find-file-noselect test-file))
            (with-current-buffer test-buffer
              ;; Verify initial state has no tools.
              (should (= (length gptel--known-tools) 0))
              (should (= (length gptel-tools) 0))

              ;; Start the request.
              (macher-send
               "Test prompt"
               (lambda (error context)
                 (setq callback-called t)
                 (setq response-received (not error))))

              ;; Verify exactly one workspace category was added during the request.
              (let ((workspace-categories-during (mapcar #'car gptel--known-tools)))
                (should workspace-categories-during)
                (should (= (length workspace-categories-during) 1))
                (should (string-prefix-p "macher-workspace-" (car workspace-categories-during))))

              ;; Wait for the async response and cleanup.
              (let ((timeout 0))
                (while (and (not callback-called) (< timeout 100))
                  (sit-for 0.1)
                  (setq timeout (1+ timeout))))

              ;; Wait a bit more for cleanup to complete.
              (sleep-for 0.2)

              ;; Verify callback was called and response received.
              (should callback-called)
              (should response-received)

              ;; Verify tools are cleaned up after callback - back to empty state.
              (should (= (length gptel--known-tools) 0))
              (should (= (length gptel-tools) 0)))))
      ;; Clean up.
      (when test-buffer
        (kill-buffer test-buffer))
      (when (and temp-dir (file-exists-p temp-dir))
        (delete-directory temp-dir t)))))

(provide 'macher-integration-tests)
;;; macher-integration-tests.el ends here
