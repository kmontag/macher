;;; test-integration.el --- Integration tests for macher.el -*- lexical-binding: t -*-

;;; Commentary:
;; Integration tests for macher.el functionality that test the full pipeline
;; including context generation and formatting using buttercup framework.

;;; Code:

(require 'buttercup)
(require 'macher)
(require 'cl-lib)
(require 'gptel)
(require 'gptel-ollama)
(require 'project)

;;; Stubbed gptel backend implementation:

(cl-defstruct
 (macher-test-backend
  (:constructor macher-test--make-backend) (:copier nil) (:include gptel-backend))
 "A stubbed gptel backend for testing."
 (responses nil) ; List of responses to provide
 (current-index 0) ; Current response index
 (received-requests nil)) ; List of requests received by the backend

(cl-defmethod macher--wrap-ollama-method (method &rest args)
  "Wrap a gptel backend METHOD by calling it with a dummy ollama backend.
ARGS will be forwarded."
  (let (
        ;; Avoid modifications to the global registry.
        (gptel--known-backends nil)
        (ollama (gptel-make-ollama "Ollama Empty")))
    (apply method ollama args)))

(cl-defmethod gptel--parse-buffer ((_backend macher-test-backend) &rest args)
  (apply #'macher--wrap-ollama-method #'gptel--parse-buffer args))

(cl-defmethod gptel--parse-tool-results ((_backend macher-test-backend) &rest args)
  (apply #'macher--wrap-ollama-method #'gptel--parse-tool-results args))

(cl-defmethod gptel--parse-response ((_backend macher-test-backend) response info)
  (funcall #'macher--wrap-ollama-method #'gptel--parse-response response info))

(cl-defmethod gptel--request-data ((_backend macher-test-backend) &rest args)
  (apply #'macher--wrap-ollama-method #'gptel--request-data args))

(defun macher-test--send (preset prompt callback)
  "Send PROMPT with to the LLM with a macher PRESET applied.

Invoke CALLBACK after the request completes. The response is handled in
the same way as with `gptel-send'."
  (macher--with-preset
   preset
   (lambda ()
     (macher--gptel-request
      callback
      prompt
      :stream gptel-stream
      :transforms gptel-prompt-transform-functions
      :fsm (gptel-make-fsm :handlers gptel-send--handlers)))))

(defun macher-test--make-once-only-callback (original-callback)
  "Wrap ORIGINAL-CALLBACK to ensure it's only called once.
This prevents test flakiness from duplicate callback invocations."
  (let ((called nil))
    (lambda (&rest args)
      (if called
          (error "Already called")
        (setq called t)
        (apply original-callback args)))))

(buttercup-define-matcher :to-appear-once-in (pattern content)
  (let ((pattern (funcall pattern))
        (content (funcall content)))
    (unless (stringp pattern)
      (error (format "Expected string pattern, got %s" pattern)))
    (unless (stringp content)
      (error (format "Expected string content, got %s" content)))
    (let ((matches 0)
          (start 0))
      (while (string-match pattern content start)
        (setq matches (1+ matches))
        (setq start (match-end 0)))
      (cond
       ((= matches 0)
        `(nil . ,(format "Pattern '%s' not found in content '%s'" pattern content)))
       ((= matches 1)
        `(t . ,(format "Pattern '%s' appears exactly once in content '%s'" pattern content)))
       (t
        `(nil
          .
          ,(format "Pattern '%s' found %d times (expected exactly 1) in '%s'"
                   pattern
                   matches
                   content)))))))

(describe "integration tests"
  :var*
  (
   ;; Store global vars for restoration at the end of the suite.
   (original-project-vc-extra-root-markers project-vc-extra-root-markers)
   (original-gptel-backend gptel-backend)
   (original-gptel--known-backends gptel--known-backends)
   (original-gptel-model gptel-model)
   (original-gptel-stream gptel-stream)
   (original-gptel-use-curl gptel-use-curl)
   (original-debug-on-error debug-on-error)
   (original-debug-on-quit debug-on-quit)
   ;; Trackers for temp directories/files, if created via setup functions.
   (project-dir)
   (project-file)


   ;; Internal helpers for dealing with stub backend objects.
   (create-backend
    (lambda (backend-id responses)
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
               :responses responses)))
        ;; Register the backend in gptel's known backends.
        (setf (alist-get name gptel--known-backends nil nil #'equal) backend)
        backend)))

   ;; Helper to set up a stubbed backend with known responses.
   (next-backend-id 0)
   (setup-backend
    (lambda (responses)
      (let ((backend (funcall create-backend next-backend-id responses)))
        (setq next-backend-id (1+ next-backend-id))
        (setq gptel-backend backend)
        (setq gptel-model 'test-model)
        backend)))
   ;; Helper to create a project structure in a generic directory.
   (create-project-structure
    (lambda (temp-dir files)
      "Create a project structure in TEMP-DIR with FILES.
FILES is an alist of (relative-path . content) pairs.

Returns the absolute path of the first file from FILES."
      ;; Create .project file to ensure the directory is detected as a project.
      (unless (and (file-name-absolute-p temp-dir) (file-directory-p temp-dir))
        (error "Expected an absolute directory path, got '%s'" temp-dir))
      (with-temp-file (expand-file-name ".project" temp-dir)
        (insert ""))
      ;; Create each file and its directory structure.
      (let ((result))
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
              (insert content))

            (unless result
              (setq result full-path))))
        result)))
   ;; Helper to set up a named project and set the project-dir/project-file variables.
   (setup-project
    (lambda (name &optional files)
      "Create a project named NAME on the filesystem.
FILES is an optional alist of (relative-path . content) pairs. If omitted, a default set of files will be used. 'project-file' will be set to the first file in the list."
      (when project-dir
        (error "Main project already created"))
      (setq project-dir (expand-file-name (make-temp-file (format "macher-test") t) name))
      (let*
          ((default-files
            `(("src/main.el"
               .
               ";;; main.el --- Main file\n\nMain file content\n\n(provide 'main)\n;;; main.el ends here\n")
              ("src/other.el"
               .
               ";;; other.el --- Other file\n\nOther file content\n\n(provide 'other)\n;;; other.el ends here\n"))))
        ;; Create project structure and store one of the files.
        (setq project-file
              (funcall create-project-structure project-dir (or files default-files))))))
   (received-requests
    (lambda ()
      "Get a list of received request, in the order they were received.

Each element is a plist containing :model, :messages, :tools."
      (reverse (macher-test-backend-received-requests gptel-backend))))

   (messages-of-type
    (lambda (requests type)
      "Get a list of the actual content of messages filtered by type.
Accepts a list of message plists, as returned by 'received-requests'.
Returns a list of content strings."
      (seq-map
       (lambda (ms)
         (seq-map
          (lambda (m) (plist-get m :content))
          (seq-filter (lambda (m) (string= (plist-get m :role) type)) ms)))
       (seq-map (lambda (r) (plist-get r :messages)) requests)))))

  (before-all
    ;; Allow project.el to detect projects with this marker file in the root.
    (setq project-vc-extra-root-markers '(".project"))

    ;; gptel uses `with-demoted-errors' to swallow errors at some points during the request
    ;; lifecycle. Set 'debug-on-error' globally to cause swallowed errors to actually trigger
    ;; the debugger, which will be interpreted as a proper test error.
    (setq debug-on-error t)

    ;; If we end up triggering a prompt to the user (e.g. through `save-some-buffers'), the tests
    ;; will try to force-quit. Catch this as an error with a stacktrace instead.
    (setq debug-on-quit t)

    (setq gptel-use-curl nil) ; Disable curl to force use of url-retrieve.
    (setq gptel-stream nil))

  (after-all
    ;; Restore original global settings.
    (setq project-vc-extra-root-markers original-project-vc-extra-root-markers)
    (setq gptel-backend original-gptel-backend)
    (setq gptel-model original-gptel-model)
    (setq gptel-stream original-gptel-stream)
    (setq gptel-use-curl original-gptel-use-curl)
    (setq debug-on-error original-debug-on-error)
    (setq debug-on-quit original-debugon-quit))

  (before-each
    ;; Sanity check that trackers are cleaned up between tests.
    (expect project-dir :to-be nil)
    (expect project-file :to-be nil)

    ;; Override url-retrieve to simulate HTTP requests to the backend.
    (spy-on
     'url-retrieve
     :and-call-fake
     (lambda (url callback &optional cbargs data silent inhibit-cookies)

       "Mock implementation of `url-retrieve' for testing.
Creates a fake HTTP response buffer and calls CALLBACK.

URL is the request URL used to identify the backend.
CALLBACK is the function to call when response is ready.
CBARGS are additional arguments passed to CALLBACK.
SILENT and INHIBIT-COOKIES are ignored in this mock implementation."
       (let*
           (
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
            "No macher-test-backend found for hostname: %s (backend-name: %s)"
            hostname
            backend-name))

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
           (let* ((response
                   ;; Get the next response from the backend's response list, and increment the
                   ;; counter.
                   (let* ((responses (macher-test-backend-responses gptel-backend))
                          (index (macher-test-backend-current-index gptel-backend))
                          (response (nth index responses)))
                     ;; Increment index for next call, cycling back to 0 if at end.
                     (setf (macher-test-backend-current-index gptel-backend)
                           (mod (1+ index) (max 1 (length responses))))
                     response))
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
                        ;; Aborting requests kills the response buffer.
                        (when (buffer-live-p response-buffer)
                          (with-current-buffer response-buffer
                            (apply
                             callback
                             ;; `url-retrieve' would normally pass a STATUS plist containing a list
                             ;; of events during the request, but it's not currently used by gptel.
                             '()
                             cbargs))))
                      nil)

         response-buffer))))

  (after-each
    ;; Verify that no file buffers were created, as this can mess with other tests (e.g. which use
    ;; `save-some-buffers'). Note these tests also open action/patch buffers; we might want to clean
    ;; those up at some point.
    (expect (seq-find (lambda (b) (buffer-file-name b)) (buffer-list)) :to-be nil)

    ;; Clean up project directory and files.
    (when project-dir
      (delete-directory project-dir t))

    ;; Clean up gptel context.
    (gptel-context-remove-all)
    ;; Remove any newly-created backends from the registry.
    (setq gptel--known-backends original-gptel--known-backends)
    (expect (length gptel--known-backends) :to-be 1)
    ;; Reset variables that may have changed.
    (setq project-dir nil)
    (setq project-file nil)
    (setq gptel-backend nil)
    (setq gptel-model nil))

  (describe "basic backend functionality"

    (it "sends and receives basic requests"
      (funcall setup-backend '("Hello, World!"))
      (let ((exit-code nil)
            (fsm nil)
            (callback-called nil)
            (temp-file (make-temp-file "macher-test-basic-")))
        (unwind-protect
            (with-temp-buffer
              (set-visited-file-name temp-file)
              (macher-test--send
               'macher-ro "Test prompt"
               (macher-test--make-once-only-callback
                (lambda (cb-exit-code cb-fsm)
                  (setq callback-called t)
                  (setq exit-code cb-exit-code)
                  (setq fsm cb-fsm))))

              ;; Wait for the async response.
              (let ((timeout 0))
                (while (and (not callback-called) (< timeout 100))
                  (sleep-for 0.1)
                  (setq timeout (1+ timeout))))

              (expect callback-called :to-be-truthy)
              (expect exit-code :to-be nil)
              (expect (gptel-fsm-state fsm) :to-be 'DONE)

              ;; Validate that the request contains the actual prompt.
              (let ((requests (funcall received-requests)))
                (expect (> (length requests) 0) :to-be-truthy)
                (let* ((request (car requests))
                       (messages (plist-get request :messages)))
                  (expect messages :to-be-truthy)
                  (expect (> (length messages) 0) :to-be-truthy)
                  ;; Check that at least one message contains our test prompt.
                  (expect
                   (cl-some
                    (lambda (msg)
                      (and (string= (plist-get msg :role) "user")
                           (string-match-p "Test prompt" (plist-get msg :content))))
                    messages)
                   :to-be-truthy))))

          ;; Clean up.
          (when (file-exists-p temp-file)
            (delete-file temp-file)))))

    (it "handles error responses"
      (funcall setup-backend
               '((:error
                  (:type
                   "test_error"
                   ;; The error still shows up in the logs, make sure
                   ;; it's obvious that it's nbd.
                   :message "This is an expected test error"))))
      (let ((exit-code nil)
            (fsm nil)
            (callback-called nil)
            (context-received nil)
            (temp-file (make-temp-file "macher-test-error-")))
        (unwind-protect
            (with-temp-buffer
              (set-visited-file-name temp-file)
              (macher-test--send
               'macher-ro "Test prompt"
               (macher-test--make-once-only-callback
                (lambda (cb-exit-code cb-fsm)
                  (setq callback-called t)
                  (setq exit-code cb-exit-code)
                  (setq fsm cb-fsm))))

              ;; Wait for the async response.
              (let ((timeout 0))
                (while (and (not callback-called) (< timeout 100))
                  (sleep-for 0.1)
                  (setq timeout (1+ timeout))))

              (expect callback-called :to-be-truthy)
              (expect exit-code :to-be nil)
              (expect (gptel-fsm-state fsm) :to-be 'ERRS))
          ;; Clean up.
          (when (file-exists-p temp-file)
            (delete-file temp-file)))))

    (it "cycles through multiple responses"
      (funcall setup-backend '("First response" "Second response"))
      (let ((responses-received '())
            (callbacks-completed 0)
            (fsm-1 nil)
            (fsm-2 nil)
            (temp-file-1 (make-temp-file "macher-test-1-"))
            (temp-file-2 (make-temp-file "macher-test-2-")))
        (unwind-protect
            (progn
              ;; Make first request.
              (with-temp-buffer
                (set-visited-file-name temp-file-1)
                (macher-test--send
                 'macher-ro "First prompt"
                 (macher-test--make-once-only-callback
                  (lambda (exit-code fsm)
                    (expect exit-code :to-be nil)
                    (setq fsm-1 fsm)
                    (setq callbacks-completed (1+ callbacks-completed)))))

                ;; Wait for first response.
                (let ((timeout 0))
                  (while (and (< callbacks-completed 1) (< timeout 100))
                    (sleep-for 0.1)
                    (setq timeout (1+ timeout)))))

              ;; Make second request.
              (with-temp-buffer
                (set-visited-file-name temp-file-2)
                (macher-test--send
                 'macher-ro "Second prompt"
                 (macher-test--make-once-only-callback
                  (lambda (exit-code fsm)
                    (expect exit-code :to-be nil)
                    (setq fsm-2 fsm)
                    (setq callbacks-completed (1+ callbacks-completed)))))

                ;; Wait for second response.
                (let ((timeout 0))
                  (while (and (< callbacks-completed 2) (< timeout 100))
                    (sleep-for 0.1)
                    (setq timeout (1+ timeout)))))

              (expect callbacks-completed :to-be 2)
              (expect (gptel-fsm-state fsm-1) :to-be 'DONE)
              (expect (gptel-fsm-state fsm-2) :to-be 'DONE)

              (let ((requests (funcall received-requests)))
                (expect (length requests) :to-be 2)
                (let* ((user-messages (funcall messages-of-type requests "user"))
                       (first-messages (elt user-messages 0))
                       (second-messages (elt user-messages 1)))
                  (expect first-messages :to-equal '("First prompt"))
                  (expect second-messages :to-equal '("Second prompt")))))
          ;; Clean up.
          (when (file-exists-p temp-file-1)
            (delete-file temp-file-1))
          (when (file-exists-p temp-file-2)
            (delete-file temp-file-2))))))

  (it "handles conversation flow when prompt is nil (using buffer contents)"
    (funcall setup-backend '("First response from LLM" "Second response from LLM"))
    (let ((response-received nil)
          (callback-called nil)
          (temp-file (make-temp-file "macher-test-conversation-"))
          (test-buffer nil))
      (unwind-protect
          (progn
            ;; Create a test file with initial content.
            (with-temp-file temp-file
              (insert "Initial buffer content for first message"))

            (setq test-buffer (find-file-noselect temp-file))
            (with-current-buffer test-buffer
              ;; Conversation history doesn't work in fundamental mode.
              (text-mode)
              (gptel-mode 1)
              ;; First request - send buffer contents (nil prompt means use buffer up to point).
              (goto-char (point-max))
              (macher-test--send
               'macher-ro
               nil ; This should use buffer contents up to point
               (macher-test--make-once-only-callback
                (lambda (exit-code fsm)
                  (setq callback-called t)
                  (setq response-received (not exit-code)))))

              ;; Wait for the first async response.
              (let ((timeout 0))
                (while (and (not callback-called) (< timeout 100))
                  (sleep-for 0.1)
                  (setq timeout (1+ timeout))))

              (expect callback-called :to-be-truthy)
              (expect response-received :to-be-truthy)

              ;; Reset for second request.
              (setq callback-called nil)
              (setq response-received nil)

              ;; Add more content and make a second request.
              (goto-char (point-max))
              (insert "\n\nAdditional content for second message")
              (macher-test--send
               'macher-ro
               nil ; Again use buffer contents (now including LLM response + new text)
               (macher-test--make-once-only-callback
                (lambda (exit-code fsm)
                  (setq callback-called t)
                  (setq response-received (not exit-code)))))

              ;; Wait for the second async response.
              (let ((timeout 0))
                (while (and (not callback-called) (< timeout 100))
                  (sleep-for 0.1)
                  (setq timeout (1+ timeout))))

              (expect callback-called :to-be-truthy)
              (expect response-received :to-be-truthy)

              ;; Verify conversation flow using messages-of-type helper.
              (let* ((requests (funcall received-requests))
                     (user-messages (funcall messages-of-type requests "user"))
                     (assistant-messages (funcall messages-of-type requests "assistant")))

                ;; Should have 2 requests.
                (expect (length requests) :to-be 2)
                (expect (length user-messages) :to-be 2)
                (expect (length assistant-messages) :to-be 2)

                ;; First request should contain only initial content.
                (let ((first-user-messages (elt user-messages 0)))
                  (expect (length first-user-messages) :to-be 1)
                  (expect "Initial buffer content" :to-appear-once-in (car first-user-messages))
                  ;; Should NOT contain the additional content from the second message.
                  (expect
                   (car first-user-messages)
                   :not
                   :to-match "Additional content for second message"))

                ;; Second request should contain conversation flow.
                (let ((second-user-messages (elt user-messages 1))
                      (second-assistant-messages (elt assistant-messages 1)))
                  ;; Should have multiple user messages (original + new content).
                  (expect (length second-user-messages) :to-be 2)
                  ;; Should have assistant response from first exchange.
                  (expect (length second-assistant-messages) :to-be 1)

                  ;; Check content of each message type specifically.
                  (let ((first-user-msg (nth 0 second-user-messages))
                        (assistant-msg (nth 0 second-assistant-messages))
                        (second-user-msg (nth 1 second-user-messages)))

                    ;; First user message should contain the initial buffer content.
                    (expect "Initial buffer content" :to-appear-once-in first-user-msg)
                    ;; First user message should NOT contain the additional content.
                    (expect first-user-msg :not :to-match "Additional content for second message")

                    ;; Assistant message should contain the LLM response.
                    (expect "First response from LLM" :to-appear-once-in assistant-msg)

                    ;; Second user message should contain the additional content.
                    (expect
                     "Additional content for second message"
                     :to-appear-once-in second-user-msg)
                    ;; Second user message should NOT contain the initial content.
                    (expect second-user-msg :not :to-match "Initial buffer content"))))))
        ;; Clean up.
        (when test-buffer
          (kill-buffer test-buffer))
        (when (file-exists-p temp-file)
          (delete-file temp-file)))))

  (it "prepares context files for workspace operations"
    (funcall setup-backend '("Test response"))
    (funcall setup-project
             "context"
             '(("src/main.el" . "main content") ("src/context.el" . "context content")))
    (let ((callback-called nil)
          (response-received nil)
          (context-file (expand-file-name "src/context.el" project-dir)))
      (with-temp-buffer
        (set-visited-file-name project-file)
        ;; Add the context file to gptel context (but not the main file).
        (gptel-add-file context-file)

        ;; Mock the context contents creation to verify it's called.
        (spy-on #'macher--load-gptel-context-files :and-call-through)

        (macher-test--send
         'macher-ro "Test prompt"
         (macher-test--make-once-only-callback
          (lambda (exit-code fsm)
            (setq callback-called t)
            (setq response-received (not exit-code)))))

        ;; Wait for the async response.
        (let ((timeout 0))
          (while (and (not callback-called) (< timeout 100))
            (sleep-for 0.1)
            (setq timeout (1+ timeout))))

        (expect callback-called :to-be-truthy)
        (expect response-received :to-be-truthy)

        ;; Verify that implementation contents were created for the context file.
        (expect #'macher--load-gptel-context-files :to-have-been-called-times 1)
        (let ((args (spy-calls-args-for #'macher--load-gptel-context-files 0)))
          (expect (length args) :to-be 2)
          (let ((contexts (car args))
                (macher-context (elt args 1)))
            ;; Verify that the gptel context was passed it, like with the standard
            ;; 'gptel-context-string-function'.
            (expect contexts :to-equal `((,context-file)))
            ;; Verify that the gptel context file was loaded into the macher context.
            (expect
             (macher-context-contents macher-context)
             :to-equal `((,context-file . ("context content" . "context content")))))))))

  (describe "tool lifecycle tests"
    (it "includes tools in request for macher presets"
      (funcall setup-backend '("Test response"))
      (funcall setup-project "tools")
      (let ((callback-called nil)
            (exit-code nil)
            (fsm nil)
            (temp-file (make-temp-file "macher-test-tools-")))
        (unwind-protect
            (with-temp-buffer
              (set-visited-file-name temp-file)
              (macher-test--send
               'macher "Test prompt with tools"
               (macher-test--make-once-only-callback
                (lambda (cb-exit-code cb-fsm)
                  (setq callback-called t)
                  (setq exit-code cb-exit-code)
                  (setq fsm cb-fsm))))

              ;; Wait for the async response.
              (let ((timeout 0))
                (while (and (not callback-called) (< timeout 100))
                  (sleep-for 0.1)
                  (setq timeout (1+ timeout))))

              (expect callback-called :to-be-truthy)
              (expect exit-code :to-be nil)
              (expect (gptel-fsm-state fsm) :to-be 'DONE)

              ;; Verify that tools were included in the request.
              (let ((requests (funcall received-requests)))
                (expect (> (length requests) 0) :to-be-truthy)
                (let* ((request (car requests))
                       (tools (plist-get request :tools)))
                  ;; Check that there are at least a few tools. The exact number might change in the future.
                  (expect (> (length tools) 2) :to-be-truthy)
                  ;; Check that :tools contains an expected tool.
                  (expect tools :to-be-truthy)
                  (expect
                   (seq-find
                    (lambda (t)
                      (string= (plist-get (plist-get t :function) :name) "write_file_in_workspace"))
                    tools)
                   :to-be-truthy))))
          ;; Clean up.
          (when (file-exists-p temp-file)
            (delete-file temp-file))))))

  (describe "context string generation"

    (it "includes workspace info when nothing is in context"
      (funcall setup-backend '("Test response"))
      (funcall setup-project
               "no-context"
               '(("README.md" . "# Test Project\n\nSimple test project.\n")
                 ("src/main.el" . "Main content")
                 ("src/util.el" . "Utility functions content")
                 ("test/test-main.el" . "Test file content")))
      (let ((response-received nil)
            (callback-called nil)
            ;; Explicitly make sure the context shows up in a system message.
            (gptel-use-context 'system))

        (gptel-context-remove-all)

        (with-temp-buffer
          ;; Open the main project file but don't add anything to context.
          (set-visited-file-name project-file)

          (macher-test--send
           'macher-notools "Test prompt with no context"
           (macher-test--make-once-only-callback
            (lambda (exit-code fsm)
              (setq callback-called t)
              (setq response-received (not exit-code)))))

          ;; Wait for the async response.
          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)
          (expect response-received :to-be-truthy)

          ;; Validate that the request contains expected content.
          (let ((requests (funcall received-requests)))
            (expect (> (length requests) 0) :to-be-truthy)
            (let* ((request (car requests))
                   (messages (plist-get request :messages))
                   (system-messages
                    (cl-remove-if-not
                     (lambda (msg) (string= (plist-get msg :role) "system")) messages)))
              (expect messages :to-be-truthy)
              (expect (> (length system-messages) 0) :to-be-truthy)

              ;; Verify that context appears in system message.
              (let ((system-content
                     (mapconcat (lambda (msg) (plist-get msg :content)) system-messages " ")))
                ;; Check for workspace description.
                (expect "WORKSPACE CONTEXT" :to-appear-once-in system-content)
                ;; Check that all files are present but not marked with [*] since nothing is in context.
                (expect "    README.md" :to-appear-once-in system-content)
                (expect "    src/main.el" :to-appear-once-in system-content)
                (expect "    src/util.el" :to-appear-once-in system-content)
                (expect "    test/test-main.el" :to-appear-once-in system-content)
                ;; Check that no files are marked with [*] since nothing is in context.
                (expect system-content :not :to-match "\\[\\*\\]")
                ;; Check that it mentions the project name in the description.
                (expect
                 (format "In-memory editing environment for '%s'"
                         (file-name-nondirectory project-dir))
                 :to-appear-once-in system-content)))))))

    (it "includes workspace info when both files and buffers are in context"
      (funcall setup-backend '("Test response"))
      (funcall setup-project
               "mixed"
               '(("src/main.el" . "main content")
                 ("src/context.el" . "context content")
                 ("src/buffer.el" . "Buffer file content")
                 ("docs/README.md" . "# Test Project\n\nThis is a test project.\n")))
      (let ((response-received nil)
            (callback-called nil)
            (buffer-buffer nil)
            ;; Explicitly make sure the context shows up in a system message.
            (gptel-use-context 'system))

        (gptel-context-remove-all)

        ;; Open buffer for context.
        (setq buffer-buffer (find-file-noselect (expand-file-name "src/buffer.el" project-dir)))

        (unwind-protect
            (progn
              (with-temp-buffer
                ;; Open the main project file.
                (set-visited-file-name project-file)
                ;; Set up context - add file to context.
                (gptel-add-file (expand-file-name "src/context.el" project-dir))
                ;; Add buffer to context.
                (with-current-buffer buffer-buffer
                  (gptel-add))

                (macher-test--send
                 'macher-notools "Test prompt with mixed context"
                 (macher-test--make-once-only-callback
                  (lambda (exit-code fsm)
                    (setq callback-called t)
                    (setq response-received (not exit-code)))))

                ;; Wait for the async response.
                (let ((timeout 0))
                  (while (and (not callback-called) (< timeout 100))
                    (sleep-for 0.1)
                    (setq timeout (1+ timeout))))

                (expect callback-called :to-be-truthy)
                (expect response-received :to-be-truthy)

                ;; Validate that the request contains expected content.
                (let ((requests (funcall received-requests)))
                  (expect (> (length requests) 0) :to-be-truthy)
                  (let* ((request (car requests))
                         (messages (plist-get request :messages))
                         (system-messages
                          (cl-remove-if-not
                           (lambda (msg) (string= (plist-get msg :role) "system")) messages)))
                    (expect messages :to-be-truthy)
                    (expect (> (length system-messages) 0) :to-be-truthy)

                    ;; Verify that context appears in system message.
                    (let ((system-content
                           (mapconcat (lambda (msg) (plist-get msg :content)) system-messages " ")))
                      ;; Check for workspace description.
                      (expect "WORKSPACE CONTEXT" :to-appear-once-in system-content)
                      ;; Check that the context file is marked with [*].
                      (expect "\\[\\*\\] src/context.el" :to-appear-once-in system-content)
                      ;; Check that other files are not marked, including the one that was loaded as a buffer.
                      (expect "    src/buffer.el" :to-appear-once-in system-content)
                      (expect "    docs/README.md" :to-appear-once-in system-content)
                      (expect "    src/main.el" :to-appear-once-in system-content)
                      ;; Check that buffer content appears in context (since buffer was added).
                      (expect "Buffer file content" :to-appear-once-in system-content)
                      ;; Check that it mentions the project name in the description.
                      (expect
                       (format "In-memory editing environment for '%s'"
                               (file-name-nondirectory project-dir))
                       :to-appear-once-in system-content))))))
          ;; Clean up the buffer.
          (when (buffer-live-p buffer-buffer)
            (kill-buffer buffer-buffer)))))


    (it "includes project info when context has files"
      (funcall setup-backend '("Test response"))
      (funcall setup-project
               "files"
               '(("README.md" . "# Test Project\n\nSimple test project.\n")
                 ("src/main.el" . "Main file content")
                 ("test/test-file.el" . "Test file content")))
      (let ((response-received nil)
            (callback-called nil)
            ;; Explicitly make sure the context shows up in a system message.
            (gptel-use-context 'system))

        (gptel-context-remove-all)

        (with-temp-buffer
          ;; Add the main file to gptel context.
          (set-visited-file-name (expand-file-name "src/main.el" project-dir))
          (gptel-add-file (expand-file-name "src/main.el" project-dir))
          (with-temp-buffer
            ;; Now open another project file for the test.
            (set-visited-file-name project-file)

            (macher-test--send
             'macher-ro "Test prompt with file context"
             (macher-test--make-once-only-callback
              (lambda (error context)
                (setq callback-called t)
                (setq response-received (not error)))))

            ;; Wait for the async response.
            (let ((timeout 0))
              (while (and (not callback-called) (< timeout 100))
                (sleep-for 0.1)
                (setq timeout (1+ timeout))))

            (expect callback-called :to-be-truthy)
            (expect response-received :to-be-truthy)

            ;; Validate that the request contains expected content.
            (let ((requests (funcall received-requests)))
              (expect (> (length requests) 0) :to-be-truthy)
              (let* ((request (car requests))
                     (messages (plist-get request :messages))
                     (system-messages
                      (cl-remove-if-not
                       (lambda (msg) (string= (plist-get msg :role) "system")) messages)))
                (expect messages :to-be-truthy)
                (expect (> (length system-messages) 0) :to-be-truthy)

                ;; Verify that context appears in system message.
                (let ((system-content
                       (mapconcat (lambda (msg) (plist-get msg :content)) system-messages " ")))
                  ;; Check for workspace description.
                  (expect "WORKSPACE CONTEXT" :to-appear-once-in system-content)
                  ;; Check that all files are present with proper context markers.
                  (expect "    README.md" :to-appear-once-in system-content)
                  (expect "\\[\\*\\] src/main.el" :to-appear-once-in system-content)
                  (expect "    test/test-file.el" :to-appear-once-in system-content)
                  ;; Check that file content appears in context.
                  (expect "Main file content" :to-appear-once-in system-content)
                  ;; Check that it mentions the project name in the description.
                  (expect
                   (format "In-memory editing environment for '%s'"
                           (file-name-nondirectory project-dir))
                   :to-appear-once-in system-content))))))))

    (it "includes workspace info when only buffers are in context"
      (funcall setup-backend '("Test response"))
      (funcall setup-project
               "buffers"
               '(("first.el" . "first content")
                 ("src/second.el" . "second content")
                 ("src/third.el" . "third content")))
      (let ((response-received nil)
            (callback-called nil)
            ;; Explicitly make sure the context shows up in a system message.
            (gptel-use-context 'system)
            (context-buffer nil))

        (gptel-context-remove-all)

        (unwind-protect
            (progn
              ;; Add context from another buffer.
              (setq context-buffer
                    (find-file-noselect (expand-file-name "src/second.el" project-dir)))
              (with-current-buffer context-buffer
                (gptel-add))

              (with-temp-buffer
                ;; Now open another project file.
                (set-visited-file-name project-file)
                (macher-test--send
                 'macher-notools "Test prompt with buffer context"
                 (macher-test--make-once-only-callback
                  (lambda (error context)
                    (setq callback-called t)
                    (setq response-received (not error)))))

                ;; Wait for the async response.
                (let ((timeout 0))
                  (while (and (not callback-called) (< timeout 100))
                    (sleep-for 0.1)
                    (setq timeout (1+ timeout))))

                (expect callback-called :to-be-truthy)
                (expect response-received :to-be-truthy)

                ;; Validate that the request contains expected content.
                (let ((requests (funcall received-requests)))
                  (expect (> (length requests) 0) :to-be-truthy)
                  (let* ((request (car requests))
                         (messages (plist-get request :messages))
                         (system-messages
                          (cl-remove-if-not
                           (lambda (msg) (string= (plist-get msg :role) "system")) messages)))
                    (expect messages :to-be-truthy)
                    (expect (> (length system-messages) 0) :to-be-truthy)

                    ;; Run all expected checks.
                    (let ((system-content
                           (mapconcat (lambda (msg) (plist-get msg :content)) system-messages " ")))
                      ;; Check for workspace description.
                      (expect "WORKSPACE CONTEXT" :to-appear-once-in system-content)
                      ;; Check that all files are present but not marked as context (including other.el, which
                      ;; is present in context but only as a buffer).
                      (expect "    first.el" :to-appear-once-in system-content)
                      (expect "    src/second.el" :to-appear-once-in system-content)
                      (expect "    src/third.el" :to-appear-once-in system-content)
                      (expect "second content" :to-appear-once-in system-content)
                      ;; Check that it mentions the project name in the description.
                      (expect
                       (format "In-memory editing environment for '%s'"
                               (file-name-nondirectory project-dir))
                       :to-appear-once-in system-content))))))
          (when context-buffer
            (kill-buffer context-buffer))))))

  (describe "default before- and after-action handlers"
    :var*
    (callback-called
     exit-code fsm callback project-file-buffer
     ;; Get the exact expected action buffer contents after a single macher action, with the default
     ;; settings. This will need to be updated if the default UI changes.
     (action-buffer-content
      (lambda (request response action)
        (format (concat
                 ;; Header.
                 "### `%s` %s\n"
                 ;; Full prompt.
                 "```\n%s\n```\n"
                 ;; Response.
                 "%s\n"
                 ;; Next prompt prefix.
                 "### ")
                action request request response)))
     ;; Get the exact expected action buffer contents for org-mode buffers after a single macher action.
     ;; This will need to be updated if the org UI changes.
     (action-buffer-org-content
      (lambda (request response action)
        (format (concat
                 ;; Org-mode header with action as tag.
                 "*** %s :%s:\n"
                 ;; Prompt block.
                 ":PROMPT:\n%s\n:END:\n"
                 ;; Response.
                 "%s\n"
                 ;; Next prompt prefix (empty for org).
                 "*** ")
                request action request response))))

    (before-each
      (setq callback-called nil)
      (setq exit-code nil)
      (setq fsm nil)
      (setq callback
            (macher-test--make-once-only-callback
             (lambda (cb-exit-code _cb-execution cb-fsm)
               (setq callback-called t)
               (setq exit-code cb-exit-code)
               (setq fsm cb-fsm))))

      (funcall setup-project "macher--after-action")

      ;; Load the project file in an actual file buffer, to avoid unsaved files appearing during
      ;; `save-some-buffers'.
      (find-file project-file)
      (setq project-file-buffer (current-buffer)))

    (after-each
      (kill-buffer project-file-buffer))

    (it "formats prompts/responses for successful requests"
      (funcall setup-backend '("Response content"))

      (with-current-buffer project-file-buffer
        (macher-discuss "Test successful request" callback)

        (let ((action-buffer (macher-action-buffer)))
          (expect action-buffer :to-be-truthy)
          ;; Wait for the async response
          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)
          (expect exit-code :to-be nil)
          (expect (gptel-fsm-state fsm) :to-be 'DONE)

          (with-current-buffer action-buffer
            (let ((buffer-content (buffer-substring-no-properties (point-min) (point-max))))
              (expect
               buffer-content
               :to-equal
               (funcall action-buffer-content
                        "Test successful request"
                        "\n\nResponse content\n"
                        'discuss)))))))

    (it "formats prompts/responses for successful requests with org UI"
      (funcall setup-backend '("Response content"))
      ;; Override the UI configuration for this test.
      (let ((macher-action-buffer-ui 'org))
        (with-current-buffer project-file-buffer
          (macher-discuss "Test successful request with org" callback)

          (let ((action-buffer (macher-action-buffer)))
            (expect action-buffer :to-be-truthy)
            ;; Wait for the async response.
            (let ((timeout 0))
              (while (and (not callback-called) (< timeout 100))
                (sleep-for 0.1)
                (setq timeout (1+ timeout))))

            (expect callback-called :to-be-truthy)
            (expect exit-code :to-be nil)
            (expect (gptel-fsm-state fsm) :to-be 'DONE)

            (with-current-buffer action-buffer
              ;; Check that the action buffer is in org-mode.
              (expect (derived-mode-p 'org-mode) :to-be-truthy)
              ;; Check that the content matches the expected org-mode format exactly.
              (let ((buffer-content (buffer-substring-no-properties (point-min) (point-max))))
                (expect
                 buffer-content
                 :to-equal
                 (funcall action-buffer-org-content
                          "Test successful request with org"
                          "\n\nResponse content\n"
                          'discuss))))))))

    (it "formats prompts/responses for error requests"
      (funcall setup-backend
               '((:error (:type "test_error" :message "This is an expected test error"))))
      (with-current-buffer project-file-buffer
        (macher-discuss "Test error request" callback)

        (let ((action-buffer (macher-action-buffer)))
          (expect action-buffer :to-be-truthy)
          ;; Wait for the async response.
          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)
          (expect exit-code :to-equal "This is an expected test error")
          (expect (gptel-fsm-state fsm) :to-be 'ERRS)

          (with-current-buffer action-buffer
            (let ((buffer-content (buffer-substring-no-properties (point-min) (point-max))))
              (expect
               buffer-content
               ;; No response text, but we should see the proper overall formatting, i.e. including
               ;; the prompt and the trailing prompt prefix.
               :to-equal (funcall action-buffer-content "Test error request" "" 'discuss)))))))

    (it "inserts abort status text after aborted requests"
      (funcall setup-backend '("This response should not be received"))
      (with-current-buffer project-file-buffer
        (macher-discuss "Test abort request" callback)

        (let ((action-buffer (macher-action-buffer)))
          (expect action-buffer :to-be-truthy)

          ;; Abort the request before it completes.
          (macher-abort)

          ;; The abort should be processed immediately.
          (expect callback-called :to-be-truthy)
          (expect exit-code :to-be 'abort)

          (with-current-buffer action-buffer
            (let ((buffer-content (buffer-substring-no-properties (point-min) (point-max))))
              (expect
               buffer-content
               ;; No response text, but we should see the proper overall formatting, i.e. including
               ;; the prompt and the trailing prompt prefix.
               :to-equal (funcall action-buffer-content "Test abort request" "" 'discuss))))))))

  (describe "patch generation tests"

    (it "does not generate a patch when no changes are made"
      (funcall setup-backend '("No changes needed."))
      (funcall setup-project "empty-patch")

      (let ((callback-called nil)
            (exit-code nil)
            (fsm nil))

        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher "Test prompt that should result in no changes"
           (macher-test--make-once-only-callback
            (lambda (cb-exit-code cb-fsm)
              (setq callback-called t)
              (setq exit-code cb-exit-code)
              (setq fsm cb-fsm))))

          ;; Wait for the async response.
          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)
          (expect exit-code :to-be nil)
          (expect (gptel-fsm-state fsm) :to-be 'DONE))))

    (it "displays a patch when changes are made"
      (funcall setup-backend
               `((:tool-calls
                  [(:function
                    (:name
                     "write_file_in_workspace"
                     :arguments (:content "hello test" :path "created.txt")))])
                 "Finished creating file"))
      (funcall setup-project "patch-with-changes")
      (let* ((callback-called nil)
             (exit-code nil)
             (fsm nil)
             (rendered-patch nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher "Test prompt to create a file."
           (macher-test--make-once-only-callback
            (lambda (cb-exit-code cb-fsm)
              (setq callback-called t)
              (setq exit-code cb-exit-code)
              (setq fsm cb-fsm))))

          ;; Wait for the async response.
          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)
          (expect exit-code :to-be nil)
          (expect (gptel-fsm-state fsm) :to-be 'DONE)

          (with-current-buffer (macher-patch-buffer)
            (let ((patch (buffer-string)))
              (expect
               (regexp-quote
                (concat
                 "diff --git a/created.txt b/created.txt\n"
                 "--- /dev/null\n"
                 "+++ b/created.txt\n"
                 "@@ -0,0 +1 @@\n"
                 "+hello test"))
               :to-appear-once-in patch)

              (expect "Test prompt to create a file" :to-appear-once-in patch))))))

    (it "displays patch buffer with no-changes warning when file is overwritten with same contents"
      (funcall setup-backend
               `((:tool-calls
                  [(:function
                    (:name
                     "write_file_in_workspace"
                     :arguments (:content "main content" :path "main.txt")))])
                 "Finished updating file with same content"))
      (funcall setup-project "no-changes-warning" '(("main.txt" . "main content")))
      (let* ((callback-called nil)
             (exit-code nil)
             (fsm nil))

        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher "Test prompt that overwrites file with same contents"
           (macher-test--make-once-only-callback
            (lambda (cb-exit-code cb-fsm)
              (setq callback-called t)
              (setq exit-code cb-exit-code)
              (setq fsm cb-fsm))))

          ;; Wait for the async response.
          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)
          (expect exit-code :to-be nil)
          (expect (gptel-fsm-state fsm) :to-be 'DONE)

          ;; Check the patch buffer contents
          (with-current-buffer (macher-patch-buffer)
            (let ((patch (buffer-string)))
              ;; Should contain the no-changes message
              (expect "No changes were made to any files" :to-appear-once-in patch)
              ;; Should contain the prompt for reference
              (expect
               "Test prompt that overwrites file with same contents"
               :to-appear-once-in patch)
              ;; Should NOT contain any diff content since no changes were made
              (expect patch :not :to-match "diff --git")
              (expect patch :not :to-match "@@")
              (expect patch :not :to-match "^[+-]"))))))))

;; Local variables:
;; elisp-autofmt-load-packages-local: ("./_defs.el")
;; end:

(provide 'test-integration)
;;; test-integration.el ends here
