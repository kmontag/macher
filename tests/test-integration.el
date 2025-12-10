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

(cl-defstruct (macher-test-backend
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
  "Parse buffer for macher-test-backend.  Delegates ARGS to Ollama."
  (apply #'macher--wrap-ollama-method #'gptel--parse-buffer args))

(cl-defmethod gptel--parse-tool-results ((_backend macher-test-backend) &rest args)
  "Parse tool results for macher-test-backend.  Delegates ARGS to Ollama."
  (apply #'macher--wrap-ollama-method #'gptel--parse-tool-results args))

(cl-defmethod gptel--parse-response ((_backend macher-test-backend) response info)
  "Parse RESPONSE with INFO for macher-test-backend.  Delegates to Ollama."
  (funcall #'macher--wrap-ollama-method #'gptel--parse-response response info))

(cl-defmethod gptel--request-data ((_backend macher-test-backend) &rest args)
  "Get request data for macher-test-backend.  Delegates ARGS to Ollama."
  (apply #'macher--wrap-ollama-method #'gptel--request-data args))

(defun macher-test--send (preset prompt callback)
  "Send PROMPT with to the LLM with a macher PRESET applied.

Invoke CALLBACK after the request completes.  The response is handled in
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
      "Get a list of the actual content of messages filtered by TYPE.

Accepts a list of request plists, as returned by 'received-requests'.

Returns a list of lists of content strings - that is, for each request,
a list of the contents of messages whose role is TYPE."
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
         (run-at-time
          0.01
          nil
          (lambda (_)
            ;; Aborting requests kills the response buffer.
            (when (buffer-live-p response-buffer)
              (with-current-buffer response-buffer
                (apply callback
                       ;; `url-retrieve' would normally pass a STATUS plist containing a list
                       ;; of events during the request, but it's not currently used by gptel.
                       '() cbargs))))
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
                  (expect (cl-some
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
                  (expect (car first-user-messages)
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
                    (expect "Additional content for second message"
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
            (let ((contents (macher-context-contents macher-context)))
              (expect (length contents) :to-be 1)
              (let* ((entry (car contents))
                     (stored-path (car entry))
                     (content-pair (cdr entry)))
                (expect (expand-file-name stored-path) :to-equal (expand-file-name context-file))
                (expect (car content-pair) :to-equal "context content")
                (expect (cdr content-pair) :to-equal "context content"))))))))

  (describe "read_file_in_workspace"
    (before-each
      (funcall setup-project "read-tool" '(("test-file.txt" . "line1\nline2\nline3\nline4"))))

    (it "returns full file content when called with no arguments"
      (funcall setup-backend
               '((:tool-calls
                  [(:function (:name "read_file_in_workspace" :arguments (:path "test-file.txt")))])
                 "I read the file"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher-ro "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)


          ;; Check that received-requests contains a tool response with the file contents.
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            ;; We expect one element per received request.
            (expect (length tool-messages) :to-be 2)
            ;; No tool response included in the first request.
            (expect (car tool-messages) :to-be nil)
            (expect (cadr tool-messages) :to-equal '("line1\nline2\nline3\nline4"))))))

    (it "supports the offset/limit parameters"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name
                     "read_file_in_workspace"
                     :arguments (:path "test-file.txt" :offset 2 :limit 2)))])
                 "I read specific lines from the file"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher-ro "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with the limited file contents.
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            ;; We expect one element per received request.
            (expect (length tool-messages) :to-be 2)
            ;; No tool response included in the first request.
            (expect (car tool-messages) :to-be nil)
            ;; Second request should contain lines 2-3 (offset 2, limit 2).
            (expect (cadr tool-messages) :to-equal '("line2\nline3"))))))

    (it "supports the numbered parameter for cat -n style output"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name
                     "read_file_in_workspace"
                     :arguments (:path "test-file.txt" :show_line_numbers t)))])
                 "I read the file with line numbers"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher-ro "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with numbered file contents.
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            ;; We expect one element per received request.
            (expect (length tool-messages) :to-be 2)
            ;; No tool response included in the first request.
            (expect (car tool-messages) :to-be nil)
            ;; Second request should contain numbered lines (cat -n style).
            (expect (cadr tool-messages) :to-equal '("1\tline1\n2\tline2\n3\tline3\n4\tline4"))))))

    (it "returns error when file content exceeds max read length"
      ;; Create a file with content that exceeds macher--max-read-length
      (let* ((large-content (make-string (1+ macher--max-read-length) ?x))
             (large-file-path (expand-file-name "large-file.txt" project-dir)))
        (unwind-protect
            (progn
              ;; Create the large file in the existing project directory
              (with-temp-file large-file-path
                (insert large-content))
              (funcall setup-backend
                       '((:tool-calls
                          [(:function
                            (:name "read_file_in_workspace" :arguments (:path "large-file.txt")))])
                         "I tried to read a large file"))
              (let ((callback-called nil)
                    (exit-code nil))
                (with-temp-buffer
                  (set-visited-file-name project-file)
                  (macher-test--send
                   'macher-ro "Test prompt"
                   (lambda (cb-exit-code cb-fsm)
                     (setq callback-called t)
                     (setq exit-code cb-exit-code)))

                  (let ((timeout 0))
                    (while (and (not callback-called) (< timeout 100))
                      (sleep-for 0.1)
                      (setq timeout (1+ timeout))))

                  (expect callback-called :to-be-truthy)

                  ;; Check that received-requests contains a tool response with error message.
                  (let* ((requests (funcall received-requests))
                         (tool-messages (funcall messages-of-type requests "tool")))
                    ;; We expect one element per received request.
                    (expect (length tool-messages) :to-be 2)
                    ;; No tool response included in the first request.
                    (expect (car tool-messages) :to-be nil)
                    ;; Second request should contain error message about file being too large.
                    (let ((error-message (cadr tool-messages)))
                      (expect (length error-message) :to-be 1)
                      (expect "File content too large" :to-appear-once-in (car error-message))
                      (expect "exceeds maximum read length"
                              :to-appear-once-in (car error-message)))))))
          ;; Clean up the large file
          (when (file-exists-p large-file-path)
            (delete-file large-file-path)))))

    (it "handles float values for offset and limit parameters"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name
                     "read_file_in_workspace"
                     :arguments (:path "test-file.txt" :offset 2.7 :limit 1.4)))])
                 "I read specific lines with float parameters"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher-ro "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with the correctly rounded parameters.
          ;; offset 2.7 should round to 3, limit 1.4 should round to 1
          ;; So we should get line3 (1 line starting from offset 3)
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            ;; We expect one element per received request.
            (expect (length tool-messages) :to-be 2)
            ;; No tool response included in the first request.
            (expect (car tool-messages) :to-be nil)
            ;; Second request should contain line3 (offset 2.7 -> 3, limit 1.4 -> 1).
            (expect (cadr tool-messages) :to-equal '("line3")))))))

  (describe "edit_file_in_workspace"
    (before-each
      (funcall setup-project
               "edit-tool"
               '(("test-file.txt" . "hello world\nhello universe\nhello again"))))

    (it "performs single edit without replace_all"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name
                     "edit_file_in_workspace"
                     :arguments
                     (:path "test-file.txt" :old_text "hello universe" :new_text "hi universe")))])
                 "I edited the file"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with null (success).
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            ;; We expect one element per received request.
            (expect (length tool-messages) :to-be 2)
            ;; No tool response included in the first request.
            (expect (car tool-messages) :to-be nil)
            ;; Second request should contain null (success).
            (expect (cadr tool-messages) :to-equal '("nil"))))))

    (it "performs replace_all edit"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name
                     "edit_file_in_workspace"
                     :arguments
                     (:path "test-file.txt" :old_text "hello" :new_text "hi" :replace_all t)))])
                 "I edited all occurrences in the file"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with null (success).
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            ;; We expect one element per received request.
            (expect (length tool-messages) :to-be 2)
            ;; No tool response included in the first request.
            (expect (car tool-messages) :to-be nil)
            ;; Second request should contain null (success).
            (expect (cadr tool-messages) :to-equal '("nil")))))))

  (describe "multi_edit_file_in_workspace"
    (before-each
      (funcall setup-project
               "multi-edit-tool"
               '(("test-file.txt" . "hello world\nhello universe\ngoodbye world"))))

    (it "performs multiple edits without replace_all"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name
                     "multi_edit_file_in_workspace"
                     :arguments
                     (:path
                      "test-file.txt"
                      :edits
                      [(:old_text "hello world" :new_text "hi world")
                       (:old_text "goodbye world" :new_text "farewell world")])))])
                 "I made multiple edits to the file"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with null (success).
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            ;; We expect one element per received request.
            (expect (length tool-messages) :to-be 2)
            ;; No tool response included in the first request.
            (expect (car tool-messages) :to-be nil)
            ;; Second request should contain null (success).
            (expect (cadr tool-messages) :to-equal '("nil"))))))

    (it "performs multiple edits with replace_all"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name
                     "multi_edit_file_in_workspace"
                     :arguments
                     (:path
                      "test-file.txt"
                      :edits
                      [(:old_text "hello" :new_text "hi" :replace_all t)
                       (:old_text "goodbye world" :new_text "farewell planet")])))])
                 "I made multiple edits with replace_all"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with null (success).
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            ;; We expect one element per received request.
            (expect (length tool-messages) :to-be 2)
            ;; No tool response included in the first request.
            (expect (car tool-messages) :to-be nil)
            ;; Second request should contain null (success).
            (expect (cadr tool-messages) :to-equal '("nil")))))))

  (describe "delete_file_in_workspace"
    (before-each
      (funcall setup-project
               "delete-tool"
               '(("test-file.txt" . "This file will be deleted")
                 ("keep-file.txt" . "This file should remain"))))

    (it "deletes an existing file successfully"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name "delete_file_in_workspace" :arguments (:path "test-file.txt")))])
                 "I deleted the file"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with null (success).
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            ;; We expect one element per received request.
            (expect (length tool-messages) :to-be 2)
            ;; No tool response included in the first request.
            (expect (car tool-messages) :to-be nil)
            ;; Second request should contain null (success).
            (expect (cadr tool-messages) :to-equal '("nil"))))))

    (it "returns error when trying to delete non-existent file"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name "delete_file_in_workspace" :arguments (:path "non-existent-file.txt")))])
                 "I tried to delete a file that doesn't exist"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with error message.
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            ;; We expect one element per received request.
            (expect (length tool-messages) :to-be 2)
            ;; No tool response included in the first request.
            (expect (car tool-messages) :to-be nil)
            ;; Second request should contain error message.
            (let ((error-message (cadr tool-messages)))
              (expect (length error-message) :to-be 1)
              (expect
               ;; When a tool raises an error, the quotes in the formatted string that gets returned
               ;; apper to get changed, e.g. ' -> `.
               "File .non-existent-file.txt. not found in workspace"
               :to-appear-once-in (car error-message)))))))

    (it "generates proper diff when file is deleted"
      (funcall setup-backend
               `((:tool-calls
                  [(:function
                    (:name "delete_file_in_workspace" :arguments (:path "test-file.txt")))])
                 "Finished deleting file"))
      (let* ((callback-called nil)
             (exit-code nil)
             (fsm nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher "Test prompt to delete a file."
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
              ;; Should contain diff header for file deletion
              (expect (regexp-quote
                       (concat
                        "diff --git a/test-file.txt b/test-file.txt\n"
                        "--- a/test-file.txt\n"
                        "+++ /dev/null\n"
                        "@@ -1 +0,0 @@\n"
                        "-This file will be deleted"))
                      :to-appear-once-in patch)

              ;; Should contain the prompt for reference
              (expect "Test prompt to delete a file" :to-appear-once-in patch)))))))

  (describe "write_file_in_workspace"
    (before-each
      (funcall setup-project "write-tool" '(("existing-file.txt" . "existing content"))))

    (it "creates a new file successfully"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name
                     "write_file_in_workspace"
                     :arguments (:path "new-file.txt" :content "new file content")))])
                 "I created a new file"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with null (success).
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            ;; We expect one element per received request.
            (expect (length tool-messages) :to-be 2)
            ;; No tool response included in the first request.
            (expect (car tool-messages) :to-be nil)
            ;; Second request should contain null (success).
            (expect (cadr tool-messages) :to-equal '("nil"))))))

    (it "overwrites existing file successfully"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name
                     "write_file_in_workspace"
                     :arguments (:path "existing-file.txt" :content "overwritten content")))])
                 "I overwrote the existing file"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with null (success).
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            ;; We expect one element per received request.
            (expect (length tool-messages) :to-be 2)
            ;; No tool response included in the first request.
            (expect (car tool-messages) :to-be nil)
            ;; Second request should contain null (success).
            (expect (cadr tool-messages) :to-equal '("nil"))))))

    (it "generates proper diff when file is created"
      (funcall setup-backend
               `((:tool-calls
                  [(:function
                    (:name
                     "write_file_in_workspace"
                     :arguments (:path "created-file.txt" :content "created content")))])
                 "Finished creating file"))
      (let* ((callback-called nil)
             (exit-code nil)
             (fsm nil))
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
              ;; Should contain diff header for file creation
              (expect (regexp-quote
                       (concat
                        "diff --git a/created-file.txt b/created-file.txt\n"
                        "--- /dev/null\n"
                        "+++ b/created-file.txt\n"
                        "@@ -0,0 +1 @@\n"
                        "+created content"))
                      :to-appear-once-in patch)

              ;; Should contain the prompt for reference
              (expect "Test prompt to create a file" :to-appear-once-in patch)))))))

  (describe "move_file_in_workspace"
    (before-each
      (funcall setup-project
               "move-tool"
               '(("source-file.txt" . "content to move") ("other-file.txt" . "other content"))))

    (it "moves a file successfully"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name
                     "move_file_in_workspace"
                     :arguments
                     (:source_path "source-file.txt" :destination_path "destination-file.txt")))])
                 "I moved the file"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with null (success).
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            ;; We expect one element per received request.
            (expect (length tool-messages) :to-be 2)
            ;; No tool response included in the first request.
            (expect (car tool-messages) :to-be nil)
            ;; Second request should contain null (success).
            (expect (cadr tool-messages) :to-equal '("nil"))))))

    (it "renames a file in the same directory successfully"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name
                     "move_file_in_workspace"
                     :arguments
                     (:source_path "source-file.txt" :destination_path "renamed-file.txt")))])
                 "I renamed the file"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with null (success).
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            ;; We expect one element per received request.
            (expect (length tool-messages) :to-be 2)
            ;; No tool response included in the first request.
            (expect (car tool-messages) :to-be nil)
            ;; Second request should contain null (success).
            (expect (cadr tool-messages) :to-equal '("nil"))))))

    (it "returns error when source file doesn't exist"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name
                     "move_file_in_workspace"
                     :arguments
                     (:source_path "non-existent.txt" :destination_path "destination.txt")))])
                 "I tried to move a non-existent file"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with error message.
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            ;; We expect one element per received request.
            (expect (length tool-messages) :to-be 2)
            ;; No tool response included in the first request.
            (expect (car tool-messages) :to-be nil)
            ;; Second request should contain error message.
            (let ((error-message (cadr tool-messages)))
              (expect (length error-message) :to-be 1)
              (expect "File .non-existent.txt. not found in workspace"
                      :to-appear-once-in (car error-message)))))))

    (it "returns error when destination already exists"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name
                     "move_file_in_workspace"
                     :arguments
                     (:source_path "source-file.txt" :destination_path "other-file.txt")))])
                 "I tried to move to an existing file"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with error message.
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            ;; We expect one element per received request.
            (expect (length tool-messages) :to-be 2)
            ;; No tool response included in the first request.
            (expect (car tool-messages) :to-be nil)
            ;; Second request should contain error message.
            (let ((error-message (cadr tool-messages)))
              (expect (length error-message) :to-be 1)
              (expect "Destination .other-file.txt. already exists"
                      :to-appear-once-in (car error-message)))))))

    (it "generates proper diff when file is moved"
      (funcall setup-backend
               `((:tool-calls
                  [(:function
                    (:name
                     "move_file_in_workspace"
                     :arguments
                     (:source_path "source-file.txt" :destination_path "moved-file.txt")))])
                 "Finished moving file"))
      (let* ((callback-called nil)
             (exit-code nil)
             (fsm nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher "Test prompt to move a file."
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
              ;; Should contain diff header for file creation (destination)
              (expect (regexp-quote
                       (concat
                        "diff --git a/moved-file.txt b/moved-file.txt\n"
                        "--- /dev/null\n"
                        "+++ b/moved-file.txt\n"
                        "@@ -0,0 +1 @@\n"
                        "+content to move"))
                      :to-appear-once-in patch)

              ;; Should contain diff header for file deletion (source)
              (expect (regexp-quote
                       (concat
                        "diff --git a/source-file.txt b/source-file.txt\n"
                        "--- a/source-file.txt\n"
                        "+++ /dev/null\n"
                        "@@ -1 +0,0 @@\n"
                        "-content to move"))
                      :to-appear-once-in patch)

              ;; Should contain the prompt for reference
              (expect "Test prompt to move a file" :to-appear-once-in patch)))))))

  (describe "tool specs"
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
                  (expect (seq-find
                           (lambda (t)
                             (string=
                              (plist-get (plist-get t :function) :name) "write_file_in_workspace"))
                           tools)
                          :to-be-truthy))))
          ;; Clean up.
          (when (file-exists-p temp-file)
            (delete-file temp-file)))))

    ;; Anthropic expects an input_schema parameter, which contains a valid JSON schema describing
    ;; the tool arguments. This parameter gets added in the custom `gptel--parse-tools'
    ;; implementation for Anthropic backends.
    ;;
    ;; Other backends are a bit more flexible with non-conforming tool schemas, so there are some
    ;; potential schema errors that wouldn't be picked up by other tests - for example, a
    ;; ':required' array being present somewhere that it shouldn't be, maybe one level too deep.
    ;; This test checks that we're able to generate a strictly correct JSON schema from our tool
    ;; definitions, in particular for the Anthropic API, though this also validates that we aren't
    ;; sending any unintentionally weird schemas to other backends.
    (it "generates a valid input schema for anthropic backends"
      (require 'gptel-anthropic)
      (let* ( ;; Avoid modifications to the global registry.
             (gptel--known-backends nil)
             (anthropic (gptel-make-anthropic "Test Anthropic")))
        (macher--with-preset
         'macher
         (lambda ()
           (let ((parsed-tools (gptel--parse-tools anthropic gptel-tools)))
             (expect (vectorp parsed-tools) :to-be-truthy)
             (let ((tools-list (append parsed-tools nil)))
               ;; The exact number of tools might change; just sanity check that there are at least
               ;; several of them.
               (expect (> (length tools-list) 4) :to-be-truthy)
               (dolist (tool tools-list)
                 (let ((input-schema (plist-get tool :input_schema)))
                   (expect input-schema :to-be-truthy)
                   (let*
                       (
                        ;; A schema that just inherits from the JSON schema "meta-schema" (i.e. the
                        ;; schema for JSON schemas) expected by Anthropic.
                        (meta-schema
                         (concat
                          "{\"$schema\": \"https://json-schema.org/draft/2020-12/schema\","
                          "\"$ref\": \"https://json-schema.org/draft/2020-12/schema\"}"))
                        (input-schema-json (gptel--json-encode input-schema))
                        (meta-schema-file (make-temp-file "meta-schema" nil ".json"))
                        (input-schema-file (make-temp-file "input-schema" nil ".json")))
                     (unwind-protect
                         (progn
                           ;; Write the meta-schema to a temporary file.
                           (with-temp-file meta-schema-file
                             (insert meta-schema))
                           ;; Write the input schema to a temporary file.
                           (with-temp-file input-schema-file
                             (insert input-schema-json))
                           (with-temp-buffer
                             ;; Validate using https://github.com/sourcemeta/jsonschema.
                             (let ((exit-code
                                    (call-process "npx"
                                                  nil
                                                  (current-buffer)
                                                  nil
                                                  "jsonschema"
                                                  "validate"
                                                  meta-schema-file
                                                  input-schema-file)))

                               ;; Add some additional context as warning output.
                               (unless (eq exit-code 0)
                                 (display-warning
                                  'buttercup
                                  (format "Error in schema for tool %s:\n\n%s"
                                          (plist-get tool :name)
                                          (buffer-string))))
                               (expect exit-code :to-be 0))))
                       ;; Clean up temporary files
                       (when (file-exists-p meta-schema-file)
                         (delete-file meta-schema-file))
                       (when (file-exists-p input-schema-file)
                         (delete-file input-schema-file)))))))))))))

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
                (expect "^WORKSPACE CONTEXT" :to-appear-once-in system-content)
                ;; Check that all files are listed as available for editing since nothing is in context.
                (expect "Files available for editing:" :to-appear-once-in system-content)
                ;; Check that files appear in the correct structural relationship to the header.
                (expect
                 system-content
                 :to-match "Files available for editing:\n\\(    [^\n]*\n\\)*    README\\.md")
                (expect
                 system-content
                 :to-match "Files available for editing:\n\\(    [^\n]*\n\\)*    src/main\\.el")
                (expect
                 system-content
                 :to-match "Files available for editing:\n\\(    [^\n]*\n\\)*    src/util\\.el")
                (expect
                 system-content
                 :to-match "Files available for editing:\n\\(    [^\n]*\n\\)*    test/test-main\\.el")
                ;; Should not have an "already provided" section since nothing is in context.
                (expect system-content :not :to-match "Files already provided above")
                ;; Check that it mentions the project name in the description.
                (expect (format "which is named `%s`" (file-name-nondirectory project-dir))
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
                      (expect "^WORKSPACE CONTEXT" :to-appear-once-in system-content)
                      ;; Check that files in context are in the "already provided" section.
                      (expect "Files already provided above" :to-appear-once-in system-content)
                      (expect system-content
                              :to-match "Files already provided above.*\n    src/context\\.el")
                      ;; Check that other files are in the "available for editing" section with proper structure.
                      (expect "Other files available for editing:"
                              :to-appear-once-in system-content)
                      (expect
                       system-content
                       :to-match "Other files available for editing:\n\\(    [^\n]*\n\\)*    src/buffer\\.el")
                      (expect
                       system-content
                       :to-match "Other files available for editing:\n\\(    [^\n]*\n\\)*    docs/README\\.md")
                      (expect
                       system-content
                       :to-match "Other files available for editing:\n\\(    [^\n]*\n\\)*    src/main\\.el")
                      ;; Check that buffer content appears in context (since buffer was added).
                      (expect "Buffer file content" :to-appear-once-in system-content)
                      ;; Check that it mentions the project name in the description.
                      (expect (format "which is named `%s`" (file-name-nondirectory project-dir))
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
                  (expect "^WORKSPACE CONTEXT" :to-appear-once-in system-content)
                  ;; Check that files are properly separated between context and available.
                  (expect "Files already provided above" :to-appear-once-in system-content)
                  (expect system-content
                          :to-match "Files already provided above.*\n    src/main\\.el")
                  (expect "Other files available for editing:" :to-appear-once-in system-content)
                  (expect
                   system-content
                   :to-match "Other files available for editing:\n\\(    [^\n]*\n\\)*    README\\.md")
                  (expect
                   system-content
                   :to-match "Other files available for editing:\n\\(    [^\n]*\n\\)*    test/test-file\\.el")
                  ;; Check that file content appears in context.
                  (expect "Main file content" :to-appear-once-in system-content)
                  ;; Check that it mentions the project name in the description.
                  (expect (format "which is named `%s`" (file-name-nondirectory project-dir))
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
                      (expect "^WORKSPACE CONTEXT" :to-appear-once-in system-content)
                      ;; Check that all files are listed (buffer context doesn't affect file listing).
                      (expect "Files available for editing:" :to-appear-once-in system-content)
                      (expect
                       system-content
                       :to-match "Files available for editing:\n\\(    [^\n]*\n\\)*    first\\.el")
                      (expect
                       system-content
                       :to-match "Files available for editing:\n\\(    [^\n]*\n\\)*    src/second\\.el")
                      (expect
                       system-content
                       :to-match "Files available for editing:\n\\(    [^\n]*\n\\)*    src/third\\.el")
                      (expect "second content" :to-appear-once-in system-content)
                      ;; Check that it mentions the project name in the description.
                      (expect (format "which is named `%s`" (file-name-nondirectory project-dir))
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
              (expect buffer-content
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
                (expect buffer-content
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

  (describe "search_in_workspace"
    (before-each
      (funcall setup-project
               "search-tool"
               '(("src/main.js"
                  .
                  "console.log('hello world');\nfunction test() {\n  return 'hello universe';\n}")
                 ("src/utils.js" . "export function hello() {\n  return 'hello javascript';\n}")
                 ("README.md" . "# Hello Project\n\nThis is a test project with hello examples.")
                 ("tests/test.js" . "test('hello test', () => {\n  expect(true).toBe(true);\n});")
                 ("config.yaml" . "name: hello-app\nversion: 1.0.0"))))

    (after-each
      ;; Clean up any file buffers that might have been created during search operations. The search
      ;; tool may open files to search through them, and we need to clean those up.
      (dolist (buffer (buffer-list))
        (when (buffer-file-name buffer)
          (let ((file-path (buffer-file-name buffer)))
            ;; Only kill buffers for files within our test project directory.
            (when (and project-dir (string-prefix-p project-dir file-path))
              (kill-buffer buffer))))))

    (it "searches with files mode (default)"
      (funcall setup-backend
               '((:tool-calls
                  [(:function (:name "search_in_workspace" :arguments (:pattern "hello")))])
                 "I found hello in multiple files"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher-ro "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with search results.
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            (expect (length tool-messages) :to-be 2)
            (expect (car tool-messages) :to-be nil)
            (let ((search-result (cadr tool-messages)))
              (expect (length search-result) :to-be 1)
              (expect (car search-result) :to-match "README.md (1 match)")
              (expect (car search-result) :to-match "config.yaml (1 match)")
              (expect (car search-result) :to-match "src/main.js (2 matches)")
              (expect (car search-result) :to-match "src/utils.js (2 matches)")
              (expect (car search-result) :to-match "tests/test.js (1 match)")
              (expect (car search-result) :to-match "Total: 7 matches in 5 files"))))))

    (it "searches with content mode and context lines"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name
                     "search_in_workspace"
                     :arguments
                     (:pattern
                      "hello"
                      :mode "content"
                      :lines_before 1
                      :lines_after 1
                      :show_line_numbers t)))])
                 "I found hello with context"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher-ro "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with content results.
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            (expect (length tool-messages) :to-be 2)
            (expect (car tool-messages) :to-be nil)
            (let ((search-result (cadr tool-messages)))
              (expect (length search-result) :to-be 1)
              (expect (car search-result)
                      :to-match "README.md:3:This is a test project with hello examples.")
              (expect (car search-result) :to-match "config.yaml:1:name: hello-app")
              (expect (car search-result) :to-match "src/main.js:1:console.log('hello world');")
              (expect (car search-result) :to-match "src/main.js:3:  return 'hello universe';")
              (expect (car search-result) :to-match "src/utils.js:1:export function hello() {")
              (expect (car search-result) :to-match "src/utils.js:2:  return 'hello javascript';")
              (expect (car search-result)
                      :to-match "tests/test.js:1:test('hello test', () => {"))))))

    (it "searches with regexp file filtering"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name
                     "search_in_workspace"
                     :arguments (:pattern "hello" :file_regexp "\\.js$")))])
                 "I found hello in JavaScript files"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher-ro "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that only JS files are included in results.
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            (expect (length tool-messages) :to-be 2)
            (expect (car tool-messages) :to-be nil)
            (let ((search-result (cadr tool-messages)))
              (expect (length search-result) :to-be 1)
              ;; Should only match .js files with file_regexp "\\.js$".
              (expect (car search-result) :to-match "src/main.js (2 matches)")
              (expect (car search-result) :to-match "src/utils.js (2 matches)")
              (expect (car search-result) :to-match "tests/test.js (1 match)")
              ;; These should NOT match with .js file_regexp.
              (expect (car search-result) :not :to-match "README.md")
              (expect (car search-result) :not :to-match "config.yaml")
              (expect (car search-result) :to-match "Total: 5 matches in 3 files"))))))

    (it "searches case-insensitively"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name
                     "search_in_workspace"
                     :arguments (:pattern "HElLO" :case_insensitive t)))])
                 "I found HELLO case-insensitively"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher-ro "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that case-insensitive search found matches.
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            (expect (length tool-messages) :to-be 2)
            (expect (car tool-messages) :to-be nil)
            (let ((search-result (cadr tool-messages)))
              (expect (length search-result) :to-be 1)
              ;; Should find matches despite case difference.
              (expect (car search-result) :to-match "README.md (2 matches)")
              (expect (car search-result) :to-match "config.yaml (1 match)")
              (expect (car search-result) :to-match "src/main.js (2 matches)")
              (expect (car search-result) :to-match "src/utils.js (2 matches)")
              (expect (car search-result) :to-match "tests/test.js (1 match)")
              (expect (car search-result) :to-match "Total: 8 matches in 5 files"))))))

    (it "limits results with head_limit parameter"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name "search_in_workspace" :arguments (:pattern "hello" :head_limit 2)))])
                 "I found hello with limited results"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher-ro "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that results are limited to 2 files.
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            (expect (length tool-messages) :to-be 2)
            (expect (car tool-messages) :to-be nil)
            (let ((search-result (cadr tool-messages)))
              (expect (length search-result) :to-be 1)
              ;; Should have results limited to 2 lines (head_limit=2), just like `head -2`.
              (let ((lines (split-string (car search-result) "\n")))
                (expect (length lines) :to-be 2)))))))

    (it "searches in specific directory path"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name "search_in_workspace" :arguments (:pattern "hello" :path "src")))])
                 "I found hello in src directory"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher-ro "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that only src directory files are included
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            (expect (length tool-messages) :to-be 2)
            (expect (car tool-messages) :to-be nil)
            (let ((search-result (cadr tool-messages)))
              (expect (length search-result) :to-be 1)
              (expect (car search-result) :to-match "src/main.js (2 matches)")
              (expect (car search-result) :to-match "src/utils.js (2 matches)")
              (expect (car search-result) :not :to-match "README.md")
              (expect (car search-result) :not :to-match "config.yaml")
              (expect (car search-result) :not :to-match "tests/test.js")
              (expect (car search-result) :to-match "Total: 4 matches in 2 files"))))))

    (it "handles workspace context with modified files"
      ;; Edit a file and then search to verify the changes show up.
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name
                     "edit_file_in_workspace"
                     :arguments
                     (:path
                      "src/main.js"
                      :old_text "'hello universe'"
                      :new_text "'hello UNIQUEWORD'")))
                   (:function
                    (:name "search_in_workspace" :arguments (:pattern "hello" :mode "content")))])
                 "I modified a file and searched for hello"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that search reflects the modified workspace context.
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            (expect (length tool-messages) :to-be 2)
            ;; First request is the prompt, and has no tool result content.
            (expect (car tool-messages) :to-be nil)
            ;; Second request is the response to the tool calls.
            (let ((search-result (cadr tool-messages)))
              ;; Both the edit call and the search call were included in a single response.
              (expect (length search-result) :to-be 2)
              (expect (car search-result) :to-equal "nil")
              (let ((search-content (cadr search-result))
                    (expected-content
                     (concat
                      "src/main.js:console.log('hello world');\n"
                      "src/main.js:  return 'hello UNIQUEWORD';\n"
                      "README.md:This is a test project with hello examples.\n"
                      "config.yaml:name: hello-app\n"
                      "src/utils.js:export function hello() {\n"
                      "src/utils.js:  return 'hello javascript';\n"
                      "tests/test.js:test('hello test', () => {\n")))
                ;; This proves the edit worked: UNIQUEWORD present, universe absent
                (expect search-content :to-equal expected-content))))))))

  (describe "list_directory_in_workspace"
    (before-each
      (funcall setup-project
               "list-dir-tool"
               '(("file1.txt" . "content1")
                 ("file2.el" . "content2")
                 ("subdir/file3.md" . "content3")
                 ("subdir/nested/file4.txt" . "content4"))))

    (it "lists directory contents without recursion or sizes"
      (funcall setup-backend
               '((:tool-calls
                  [(:function (:name "list_directory_in_workspace" :arguments (:path ".")))])
                 "I listed the directory"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher-ro "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with directory listing
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            (expect (length tool-messages) :to-be 2)
            (expect (car tool-messages) :to-be nil)
            (let ((listing (cadr tool-messages)))
              (expect (length listing) :to-be 1)
              (expect (car listing) :to-match "file: file1.txt")
              (expect (car listing) :to-match "file: file2.el")
              (expect (car listing) :to-match "dir: subdir")
              (expect (car listing) :not :to-match "file3.md") ; Not recursive
              (expect (car listing) :not :to-match "B)")))))) ; No sizes

    (it "lists directory contents with sizes"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name "list_directory_in_workspace" :arguments (:path "." :sizes t)))])
                 "I listed the directory with sizes"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher-ro "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with sizes
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            (expect (length tool-messages) :to-be 2)
            (expect (car tool-messages) :to-be nil)
            (let ((listing (cadr tool-messages)))
              (expect (length listing) :to-be 1)
              (expect (car listing) :to-match "file: file1.txt (8 B)")
              (expect (car listing) :to-match "file: file2.el (8 B)")
              (expect (car listing) :to-match "dir: subdir$")) ; No size for directory
            ))))

    (it "lists directory contents recursively"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name "list_directory_in_workspace" :arguments (:path "." :recursive t)))])
                 "I listed the directory recursively"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher-ro "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with recursive listing
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            (expect (length tool-messages) :to-be 2)
            (expect (car tool-messages) :to-be nil)
            (let ((listing (cadr tool-messages)))
              (expect (length listing) :to-be 1)
              (expect (car listing) :to-match "file: file1.txt")
              (expect (car listing) :to-match "dir: subdir")
              (expect (car listing) :to-match "  file: subdir/file3.md")
              (expect (car listing) :to-match "  dir: subdir/nested")
              (expect (car listing) :to-match "    file: subdir/nested/file4.txt"))))))

    (it "lists subdirectory contents"
      (funcall setup-backend
               '((:tool-calls
                  [(:function (:name "list_directory_in_workspace" :arguments (:path "subdir")))])
                 "I listed the subdirectory"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher-ro "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with subdirectory listing
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            (expect (length tool-messages) :to-be 2)
            (expect (car tool-messages) :to-be nil)
            (let ((listing (cadr tool-messages)))
              (expect (length listing) :to-be 1)
              (expect (car listing) :to-match "file: file3.md")
              (expect (car listing) :to-match "dir: nested")
              (expect (car listing) :not :to-match "file1.txt")
              (expect (car listing) :not :to-match "file2.el"))))))

    (it "returns error when directory doesn't exist"
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name "list_directory_in_workspace" :arguments (:path "nonexistent")))])
                 "I tried to list a nonexistent directory"))
      (let ((callback-called nil)
            (exit-code nil))
        (with-temp-buffer
          (set-visited-file-name project-file)
          (macher-test--send
           'macher-ro "Test prompt"
           (lambda (cb-exit-code cb-fsm)
             (setq callback-called t)
             (setq exit-code cb-exit-code)))

          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 100))
              (sleep-for 0.1)
              (setq timeout (1+ timeout))))

          (expect callback-called :to-be-truthy)

          ;; Check that received-requests contains a tool response with error message
          (let* ((requests (funcall received-requests))
                 (tool-messages (funcall messages-of-type requests "tool")))
            (expect (length tool-messages) :to-be 2)
            (expect (car tool-messages) :to-be nil)
            (let ((error-message (cadr tool-messages)))
              (expect (length error-message) :to-be 1)
              (expect "Directory .nonexistent. not found"
                      :to-appear-once-in (car error-message))))))))

  ;; Tests ensuring that non-project files are hidden for real git repositories.
  (describe "git file visibility"
    :var (git-temp-dir git-project-dir)

    (before-each
      (setq git-temp-dir (make-temp-file "macher-git-test" t))
      (setq git-project-dir (expand-file-name "test-project" git-temp-dir))
      (make-directory git-project-dir t)

      ;; Initialize git repo.
      (let ((default-directory git-project-dir))
        (call-process "git" nil nil nil "init")
        (call-process "git" nil nil nil "config" "user.email" "test@example.com")
        (call-process "git" nil nil nil "config" "user.name" "Test User"))

      ;; Create a .gitignore file.
      (write-region "*.secret\nsecrets/\n" nil (expand-file-name ".gitignore" git-project-dir))

      ;; Create a test file and gitignored files.
      (write-region "test content" nil (expand-file-name "test.txt" git-project-dir))
      (write-region "secret data" nil (expand-file-name "api.secret" git-project-dir))
      (make-directory (expand-file-name "secrets" git-project-dir))
      (write-region
       "password=123456" nil (expand-file-name "secrets/passwords.txt" git-project-dir))

      ;; Commit the tracked files.
      (let ((default-directory git-project-dir))
        (call-process "git" nil nil nil "add" ".gitignore")
        (call-process "git" nil nil nil "add" "test.txt")
        (call-process "git" nil nil nil "commit" "-m" "Initial commit")))

    (after-each
      (when (file-exists-p git-temp-dir)
        (delete-directory git-temp-dir t)))

    (it "prevents reading files in .git directory"
      (funcall setup-backend '("Test response"))
      (let ((git-file (expand-file-name ".git/config" git-project-dir)))
        ;; Verify the git config file exists.
        (expect (file-exists-p git-file) :to-be-truthy)

        (with-temp-buffer
          (set-visited-file-name (expand-file-name "test.txt" git-project-dir))
          (let ((callback-called nil)
                (exit-code nil))
            (macher-test--send
             'macher-ro "Test prompt"
             (macher-test--make-once-only-callback
              (lambda (cb-exit-code cb-fsm)
                (setq callback-called t)
                (setq exit-code cb-exit-code))))

            (let ((timeout 0))
              (while (and (not callback-called) (< timeout 100))
                (sleep-for 0.1)
                (setq timeout (1+ timeout))))

            (expect callback-called :to-be-truthy)
            (expect exit-code :to-be nil)

            ;; Verify that .git files are not in the workspace files list.
            (let* ((workspace `(project . ,git-project-dir))
                   (workspace-files (macher--workspace-files workspace)))
              (expect (cl-some (lambda (f) (string-match-p "\\.git/" f)) workspace-files)
                      :to-be nil))))))

    (describe "list_directory_in_workspace"
      (it "does not list .git directory"
        (funcall setup-backend
                 '((:tool-calls
                    [(:function (:name "list_directory_in_workspace" :arguments (:path ".")))])
                   "Listed the directory"))

        (with-temp-buffer
          (set-visited-file-name (expand-file-name "test.txt" git-project-dir))
          (let ((callback-called nil)
                (exit-code nil))
            (macher-test--send
             'macher-ro "Test prompt"
             (macher-test--make-once-only-callback
              (lambda (cb-exit-code cb-fsm)
                (setq callback-called t)
                (setq exit-code cb-exit-code))))

            (let ((timeout 0))
              (while (and (not callback-called) (< timeout 100))
                (sleep-for 0.1)
                (setq timeout (1+ timeout))))

            (expect callback-called :to-be-truthy)
            (expect exit-code :to-be nil)

            ;; Check that the tool response does not contain .git.
            (let* ((requests (funcall received-requests))
                   (tool-messages (funcall messages-of-type requests "tool")))
              (expect (length tool-messages) :to-be 2)
              (expect (car tool-messages) :to-be nil)
              (let ((listing (cadr tool-messages)))
                (expect (length listing) :to-be 1)
                (expect (car listing) :not :to-match "\\.git"))))))

      (it "does not list gitignored files"
        (funcall setup-backend
                 '((:tool-calls
                    [(:function (:name "list_directory_in_workspace" :arguments (:path ".")))])
                   "Listed the directory"))

        (with-temp-buffer
          (set-visited-file-name (expand-file-name "test.txt" git-project-dir))
          (let ((callback-called nil)
                (exit-code nil))
            (macher-test--send
             'macher-ro "Test prompt"
             (macher-test--make-once-only-callback
              (lambda (cb-exit-code cb-fsm)
                (setq callback-called t)
                (setq exit-code cb-exit-code))))

            (let ((timeout 0))
              (while (and (not callback-called) (< timeout 100))
                (sleep-for 0.1)
                (setq timeout (1+ timeout))))

            (expect callback-called :to-be-truthy)
            (expect exit-code :to-be nil)

            ;; Check that the tool response does not contain gitignored files.
            (let* ((requests (funcall received-requests))
                   (tool-messages (funcall messages-of-type requests "tool")))
              (expect (length tool-messages) :to-be 2)
              (expect (car tool-messages) :to-be nil)
              (let ((listing (cadr tool-messages)))
                (expect (length listing) :to-be 1)
                ;; Should contain tracked files.
                (expect (car listing) :to-match "test\\.txt")
                (expect (car listing) :to-match "\\.gitignore")
                ;; Should NOT contain gitignored files.
                (expect (car listing) :not :to-match "api\\.secret")
                (expect (car listing) :not :to-match "secrets")))))))

    (describe "read_file_in_workspace"
      (it "cannot read files in .git directory"
        (funcall setup-backend
                 '((:tool-calls
                    [(:function (:name "read_file_in_workspace" :arguments (:path ".git/config")))])
                   "Tried to read git config"))

        (with-temp-buffer
          (set-visited-file-name (expand-file-name "test.txt" git-project-dir))
          (let ((callback-called nil)
                (exit-code nil))
            (macher-test--send
             'macher-ro "Test prompt"
             (macher-test--make-once-only-callback
              (lambda (cb-exit-code cb-fsm)
                (setq callback-called t)
                (setq exit-code cb-exit-code))))

            (let ((timeout 0))
              (while (and (not callback-called) (< timeout 100))
                (sleep-for 0.1)
                (setq timeout (1+ timeout))))

            (expect callback-called :to-be-truthy)
            (expect exit-code :to-be nil)

            ;; Check that the tool response contains an error.
            (let* ((requests (funcall received-requests))
                   (tool-messages (funcall messages-of-type requests "tool")))
              (expect (length tool-messages) :to-be 2)
              (expect (car tool-messages) :to-be nil)
              (let ((error-message (cadr tool-messages)))
                (expect (length error-message) :to-be 1)
                (expect "not in the workspace files list"
                        :to-appear-once-in (car error-message)))))))

      (it "cannot read gitignored files"
        (funcall setup-backend
                 '((:tool-calls
                    [(:function (:name "read_file_in_workspace" :arguments (:path "api.secret")))])
                   "Tried to read secret file"))

        (with-temp-buffer
          (set-visited-file-name (expand-file-name "test.txt" git-project-dir))
          (let ((callback-called nil)
                (exit-code nil))
            (macher-test--send
             'macher-ro "Test prompt"
             (macher-test--make-once-only-callback
              (lambda (cb-exit-code cb-fsm)
                (setq callback-called t)
                (setq exit-code cb-exit-code))))

            (let ((timeout 0))
              (while (and (not callback-called) (< timeout 100))
                (sleep-for 0.1)
                (setq timeout (1+ timeout))))

            (expect callback-called :to-be-truthy)
            (expect exit-code :to-be nil)

            ;; Check that the tool response contains an error.
            (let* ((requests (funcall received-requests))
                   (tool-messages (funcall messages-of-type requests "tool")))
              (expect (length tool-messages) :to-be 2)
              (expect (car tool-messages) :to-be nil)
              (let ((error-message (cadr tool-messages)))
                (expect (length error-message) :to-be 1)
                (expect "not in the workspace files list"
                        :to-appear-once-in (car error-message)))))))

      (it "cannot read files in gitignored directories"
        (funcall setup-backend
                 '((:tool-calls
                    [(:function
                      (:name "read_file_in_workspace" :arguments (:path "secrets/passwords.txt")))])
                   "Tried to read file in gitignored directory"))

        (with-temp-buffer
          (set-visited-file-name (expand-file-name "test.txt" git-project-dir))
          (let ((callback-called nil)
                (exit-code nil))
            (macher-test--send
             'macher-ro "Test prompt"
             (macher-test--make-once-only-callback
              (lambda (cb-exit-code cb-fsm)
                (setq callback-called t)
                (setq exit-code cb-exit-code))))

            (let ((timeout 0))
              (while (and (not callback-called) (< timeout 100))
                (sleep-for 0.1)
                (setq timeout (1+ timeout))))

            (expect callback-called :to-be-truthy)
            (expect exit-code :to-be nil)

            ;; Check that the tool response contains an error.
            (let* ((requests (funcall received-requests))
                   (tool-messages (funcall messages-of-type requests "tool")))
              (expect (length tool-messages) :to-be 2)
              (expect (car tool-messages) :to-be nil)
              (let ((error-message (cadr tool-messages)))
                (expect (length error-message) :to-be 1)
                (expect "not in the workspace files list"
                        :to-appear-once-in (car error-message))))))))

    (describe "search_in_workspace"
      (it "does not search in .git directory"
        (funcall setup-backend
                 '((:tool-calls
                    [(:function (:name "search_in_workspace" :arguments (:pattern "test")))])
                   "Searched for test"))

        (with-temp-buffer
          (set-visited-file-name (expand-file-name "test.txt" git-project-dir))
          (let ((callback-called nil)
                (exit-code nil))
            (macher-test--send
             'macher-ro "Test prompt"
             (macher-test--make-once-only-callback
              (lambda (cb-exit-code cb-fsm)
                (setq callback-called t)
                (setq exit-code cb-exit-code))))

            (let ((timeout 0))
              (while (and (not callback-called) (< timeout 100))
                (sleep-for 0.1)
                (setq timeout (1+ timeout))))

            (expect callback-called :to-be-truthy)
            (expect exit-code :to-be nil)

            ;; Check that search results don't include .git files.
            (let* ((requests (funcall received-requests))
                   (tool-messages (funcall messages-of-type requests "tool")))
              (expect (length tool-messages) :to-be 2)
              (expect (car tool-messages) :to-be nil)
              (let ((search-result (cadr tool-messages)))
                (expect (length search-result) :to-be 1)
                ;; Should find test.txt.
                (expect (car search-result) :to-match "test\\.txt")
                ;; Should NOT find anything in .git.
                (expect (car search-result) :not :to-match "\\.git"))))))

      (it "does not search gitignored files"
        (funcall setup-backend
                 '((:tool-calls
                    [(:function (:name "search_in_workspace" :arguments (:pattern "password")))])
                   "Searched for password"))

        (with-temp-buffer
          (set-visited-file-name (expand-file-name "test.txt" git-project-dir))
          (let ((callback-called nil)
                (exit-code nil))
            (macher-test--send
             'macher-ro "Test prompt"
             (macher-test--make-once-only-callback
              (lambda (cb-exit-code cb-fsm)
                (setq callback-called t)
                (setq exit-code cb-exit-code))))

            (let ((timeout 0))
              (while (and (not callback-called) (< timeout 100))
                (sleep-for 0.1)
                (setq timeout (1+ timeout))))

            (expect callback-called :to-be-truthy)
            (expect exit-code :to-be nil)

            ;; Check that search results don't include gitignored files.
            (let* ((requests (funcall received-requests))
                   (tool-messages (funcall messages-of-type requests "tool")))
              (expect (length tool-messages) :to-be 2)
              (expect (car tool-messages) :to-be nil)
              (let ((search-result (cadr tool-messages)))
                (expect (length search-result) :to-be 1)
                ;; Should find "No matches found" since "password" only appears in gitignored files.
                (expect (car search-result) :to-equal "No matches found."))))))))

  (describe "patch generation"
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
              (expect (regexp-quote
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
              (expect "Test prompt that overwrites file with same contents"
                      :to-appear-once-in patch)
              ;; Should NOT contain any diff content since no changes were made
              (expect patch :not :to-match "diff --git")
              (expect patch :not :to-match "@@")
              (expect patch :not :to-match "^[+-]")))))))

  (describe "macher-process-request"
    :var
    (callback-called
     exit-code
     callback
     project-file-buffer
     original-process-fn
     process-fn
     process-fn-called
     process-fn-args
     original-known-presets)

    (before-each
      (setq callback-called nil)
      (setq exit-code nil)
      (setq process-fn-called nil)
      (setq process-fn-args nil)
      (setq callback
            (macher-test--make-once-only-callback
             (lambda (cb-exit-code _cb-execution cb-fsm)
               (setq callback-called t)
               (setq exit-code cb-exit-code))))

      (funcall setup-project "process-request-integration" '(("test.txt" . "original content")))

      ;; Load the project file in an actual file buffer, to avoid unsaved files appearing during
      ;; `save-some-buffers'.
      (find-file project-file)
      (setq project-file-buffer (current-buffer))

      (setq original-known-presets gptel--known-presets)

      (setq original-process-fn macher-process-request-function)

      ;; Alternate process function.
      (setq process-fn
            (lambda (reason context fsm-arg)
              (setq process-fn-called t)
              (setq process-fn-args (list reason context fsm-arg)))))

    (after-each
      (setq gptel--known-presets original-known-presets)
      (setq macher-process-request-function original-process-fn)
      (kill-buffer project-file-buffer))

    (it "calls process function with context containing edit after aborted tool request"
      ;; Set up backend to respond with a tool call that edits a file.
      (setq macher-process-request-function process-fn)
      (funcall setup-backend
               '((:tool-calls
                  [(:function
                    (:name
                     "edit_file_in_workspace"
                     :arguments
                     (:path
                      "test.txt"
                      :old_text "original content"
                      :new_text "modified content")))])
                 "I edited the file as requested"))

      (with-current-buffer project-file-buffer
        (let ((fsm (macher-implement "Edit the test file" callback)))

          ;; Abort the request after the tool is called.
          (sleep-for 0.1)
          (macher-abort)

          ;; The abort should be processed immediately.
          (expect callback-called :to-be-truthy)
          (expect exit-code :to-be 'abort)

          ;; Now call macher-process-request with the aborted FSM.
          (macher-process-request-dwim 'test-reason)

          ;; Should have called our spy process function.
          (expect process-fn-called :to-be-truthy)
          (expect (length process-fn-args) :to-be 3)

          ;; Check the arguments passed to the process function.
          (let ((reason (nth 0 process-fn-args))
                (context (nth 1 process-fn-args))
                (fsm-arg (nth 2 process-fn-args)))
            (expect reason :to-be 'test-reason)
            (expect context :to-be-truthy)
            (expect fsm-arg :to-be fsm)

            ;; Verify the context contains the edit made by the tool.
            (expect (macher-context-p context) :to-be-truthy)
            (expect (macher-context-dirty-p context) :to-be-truthy)

            ;; Check that the file was actually edited by the tool.
            (expect (cdr
                     (macher-context--contents-for-file
                      (expand-file-name "test.txt" project-dir) context))
                    :to-equal "modified content"))))))

  (describe "gptel preset integration"
    :var (callback-called exit-code callback project-file-buffer orig-known-presets)

    (before-each
      (setq callback-called nil)
      (setq exit-code nil)
      (setq callback
            (macher-test--make-once-only-callback
             (lambda (cb-exit-code _cb-execution cb-fsm)
               (setq callback-called t)
               (setq exit-code cb-exit-code))))
      (setq orig-known-presets gptel--known-presets)

      (funcall setup-project "gptel-preset-test")

      ;; Load the project file in an actual file buffer, to avoid unsaved files appearing during
      ;; `save-some-buffers'.
      (find-file project-file)
      (setq project-file-buffer (current-buffer)))

    (after-each
      (kill-buffer project-file-buffer)
      (setq gptel--known-presets orig-known-presets))

    (it "applies global gptel preset when referenced in macher-implement prompt"
      ;; Create a global gptel preset that we can test
      (gptel-make-preset 'test-preset
        :description "Test preset for integration testing"
        :system "You are a helpful coding assistant. Always respond with exactly this phrase: PRESET_WAS_APPLIED")

      (funcall setup-backend '("PRESET_WAS_APPLIED"))

      (with-current-buffer project-file-buffer
        (macher-implement "@test-preset testing inline presets" callback)

        ;; Wait for the async response.
        (let ((timeout 0))
          (while (and (not callback-called) (< timeout 100))
            (sleep-for 0.1)
            (setq timeout (1+ timeout))))

        (expect callback-called :to-be-truthy)
        (expect exit-code :to-be nil)

        ;; Check that the preset was applied by verifying the system message was included.
        (let* ((requests (funcall received-requests))
               (system-messages (funcall messages-of-type requests "system")))
          (expect (> (length requests) 0) :to-be-truthy)
          (expect (> (length system-messages) 0) :to-be-truthy)

          ;; Verify that the test preset's system message appears in the request.
          (let ((system-content
                 (mapconcat (lambda (msgs) (string-join msgs " ")) system-messages " ")))
            (expect
             "You are a helpful coding assistant. Always respond with exactly this phrase: PRESET_WAS_APPLIED"
             :to-appear-once-in system-content)))))))

(provide 'test-integration)
;;; test-integration.el ends here
