;;; test-unit.el --- Unit tests for macher.el -*- lexical-binding: t -*-

;;; Commentary:
;; Unit tests for macher.el using buttercup.

;;; Code:

(require 'buttercup)
(require 'macher)

(describe "unit tests"
  :var*
  ((original-project-vc-extra-root-markers)
   ;; Trackers for temp directories/files, if created via setup-project or setup-file.
   (project-dir) (project-file) (file)
   ;; Helper to create a simple project directory that will be recognized by project.el.
   (setup-project
    (lambda ()
      (when project-dir
        (error "Project already created"))
      (setq project-dir (make-temp-file "macher-test" t))
      (let ((subdir (expand-file-name "subdir" project-dir)))
        (make-directory subdir)
        (dolist (project-file-cons
                 `((,project-dir . "file1.txt") (,project-dir . "file2.el") (,subdir . "file3.md")))
          (let ((current-project-file
                 (expand-file-name (cdr project-file-cons) (car project-file-cons))))
            ;; Just pick one of the project files to store for easy access.
            (unless project-file
              (setq project-file current-project-file))
            (write-region "test" nil current-project-file)))
        (write-region "" nil (expand-file-name ".project" project-dir)))))

   ;; Helper to create a non-project file.
   (setup-file
    (lambda ()
      (when file
        (error "File already created"))
      (setq file (make-temp-file "macher-test-file" nil ".txt")))))

  (before-all
    ;; Recognize temporary projects.
    (require 'project)
    (setq original-project-vc-extra-root-markers project-vc-extra-root-markers)
    (setq project-vc-extra-root-markers '(".project")))

  (after-all
    (setq project-vc-extra-root-markers original-project-vc-extra-root-markers))

  (before-each
    ;; Sanity check that trackers are cleaned up between tests.
    (expect project-dir :to-be nil)
    (expect project-file :to-be nil)
    (expect file :to-be nil))

  ;; Clean up temp files if they were created.
  (after-each
    (when project-dir
      (delete-directory project-dir t))
    (when file
      (delete-file file))
    (setq project-dir nil)
    (setq project-file nil)
    (setq file nil))

  ;;; Context content management tests.

  (describe "macher-context--contents-for-file"
    :var (context temp-file contents)

    (before-each
      (setq context (macher--make-context))
      (setq temp-file (make-temp-file "macher-test"))
      ;; Write some test content to the file.
      (with-temp-buffer
        (insert "test file content")
        (write-region (point-min) (point-max) temp-file)))

    (after-each
      (when (file-exists-p temp-file)
        (delete-file temp-file)))

    (it "returns contents for existing file"
      (setq contents (macher-context--contents-for-file temp-file context))
      (expect contents :to-be-truthy)
      (expect (car contents) :to-be-truthy) ;; Original content should exist.
      (expect (cdr contents) :to-be-truthy) ;; New content should exist.
      (expect (stringp (car contents)) :to-be-truthy)
      (expect (stringp (cdr contents)) :to-be-truthy)
      ;; Both should initially contain the same content.
      (expect (car contents) :to-equal "test file content")
      (expect (cdr contents) :to-equal "test file content"))

    (it "caches contents for same file"
      (setq contents (macher-context--contents-for-file temp-file context))
      (let ((cached-contents (macher-context--contents-for-file temp-file context)))
        (expect (car contents) :to-be (car cached-contents))
        (expect (cdr contents) :to-be (cdr cached-contents))))

    (it "returns nil contents for non-existent file"
      (let* ((non-existent (concat temp-file "-nonexistent"))
             (non-existent-contents (macher-context--contents-for-file non-existent context)))
        (expect non-existent-contents :to-be-truthy)
        (expect (car non-existent-contents) :to-be nil) ;; Original content should be nil.
        (expect (cdr non-existent-contents) :to-be nil) ;; New content should be nil.
        ;; Should be added to context's contents list.
        (expect (assoc (macher--normalize-path non-existent) (macher-context-contents context))
                :to-be-truthy))))

  (describe "macher-context--set-new-content-for-file"
    :var (context temp-file original-contents)

    (before-each
      (setq context (macher--make-context))
      (setq temp-file (make-temp-file "macher-test"))
      ;; Write some test content to the file.
      (with-temp-buffer
        (insert "original file content")
        (write-region (point-min) (point-max) temp-file)))

    (after-each
      (when (file-exists-p temp-file)
        (delete-file temp-file)))

    (it "sets new content for file"
      ;; First get the original contents.
      (setq original-contents (macher-context--contents-for-file temp-file context))
      ;; Set new content for the file.
      (macher-context--set-new-content-for-file temp-file "modified content" context)
      ;; Check that the new content was set correctly.
      (let ((updated-contents (macher-context--contents-for-file temp-file context)))
        (expect (car updated-contents) :to-equal "original file content") ;; Original content unchanged.
        (expect (cdr updated-contents) :to-equal "modified content")) ;; New content updated.
      ;; Check the context's contents alist was updated.
      (let ((entry (assoc (macher--normalize-path temp-file) (macher-context-contents context))))
        (expect entry :to-be-truthy)
        (expect (cadr entry) :to-equal "original file content")
        (expect (cddr entry) :to-equal "modified content")))

    (it "sets content to nil for file deletion"
      ;; First get the original contents.
      (setq original-contents (macher-context--contents-for-file temp-file context))
      ;; Test setting to nil (for file deletion).
      (macher-context--set-new-content-for-file temp-file nil context)
      (let ((deletion-contents (macher-context--contents-for-file temp-file context)))
        (expect (car deletion-contents) :to-equal "original file content")
        (expect (cdr deletion-contents) :to-be nil))
      ;; Check the context's contents alist was updated for deletion.
      (let ((entry (assoc (macher--normalize-path temp-file) (macher-context-contents context))))
        (expect entry :to-be-truthy)
        (expect (cadr entry) :to-equal "original file content")
        (expect (cddr entry) :to-be nil)))

    (it "creates new file entry when file doesn't exist"
      (let ((new-file (concat temp-file "-new")))
        ;; Set content for a non-existent file.
        (macher-context--set-new-content-for-file new-file "new file content" context)
        ;; Check that the entry was created correctly.
        (let ((new-contents (macher-context--contents-for-file new-file context)))
          (expect (car new-contents) :to-be nil) ;; Original content should be nil (file creation).
          (expect (cdr new-contents) :to-equal "new file content")) ;; New content should be set.
        ;; Check the context's contents alist was updated.
        (let ((entry (assoc (macher--normalize-path new-file) (macher-context-contents context))))
          (expect entry :to-be-truthy)
          (expect (cadr entry) :to-be nil)
          (expect (cddr entry) :to-equal "new file content")))))

  (describe "edit tools"
    :var (temp-file context)

    (before-each
      (setq temp-file (make-temp-file "macher-test"))
      ;; Write some test content to the file.
      (with-temp-buffer
        (insert "original file content")
        (write-region (point-min) (point-max) temp-file))
      (setq context (macher--make-context :workspace `(file . ,temp-file)))

      ;; Shared sanity-check expectation.
      (expect (macher-context-dirty-p context) :to-be nil))

    (after-each
      (when (file-exists-p temp-file)
        (delete-file temp-file)))

    (describe "macher--tool-write-file"
      (it "replaces file contents"
        (macher--tool-write-file context temp-file "new content")
        (let ((contents (macher-context-contents context)))
          (expect contents :to-equal `((,temp-file . ("original file content" . "new content"))))))
      (it "sets the dirty-p flag"
        (macher--tool-write-file context temp-file "new content")
        (expect (macher-context-dirty-p context) :to-be-truthy)))

    (describe "macher--tool-move-file"
      (it "moves file to new location"
        (let ((dest-file (concat temp-file "-moved")))
          (macher--tool-move-file context temp-file dest-file)
          (let ((contents (macher-context-contents context)))
            ;; Original file should be deleted (content set to nil).
            (expect (cdr (assoc temp-file contents)) :to-equal '("original file content" . nil))
            ;; New file should have the original content.
            (expect (cdr (assoc dest-file contents)) :to-equal '(nil . "original file content")))))
      (it "sets the dirty-p flag"
        (let ((dest-file (concat temp-file "-moved")))
          (macher--tool-move-file context temp-file dest-file)
          (expect (macher-context-dirty-p context) :to-be-truthy))))

    (describe "macher--tool-delete-file"
      (it "deletes file content"
        (macher--tool-delete-file context temp-file)
        (let ((contents (macher-context-contents context)))
          (expect (cdr (assoc temp-file contents)) :to-equal '("original file content" . nil))))
      (it "sets the dirty-p flag"
        (macher--tool-delete-file context temp-file)
        (expect (macher-context-dirty-p context) :to-be-truthy)))

    (describe "macher--tool-edit-file"
      (it "edits file content with single replacement"
        (macher--tool-edit-file context temp-file "original" "modified" nil)
        (let ((contents (macher-context-contents context)))
          (expect (cdr (assoc temp-file contents))
                  :to-equal '("original file content" . "modified file content"))))
      (it "edits file content with replace-all"
        ;; Create a file with multiple occurrences.
        (with-temp-buffer
          (insert "hello world hello universe")
          (write-region (point-min) (point-max) temp-file))
        ;; Reload context with new content.
        (setq context (macher--make-context :workspace `(file . ,temp-file)))
        (expect (macher-context-dirty-p context) :to-be nil)
        (macher--tool-edit-file context temp-file "hello" "hi" t)
        (let ((contents (macher-context-contents context)))
          (expect (cdr (assoc temp-file contents))
                  :to-equal '("hello world hello universe" . "hi world hi universe"))))
      (it "sets the dirty-p flag"
        (macher--tool-edit-file context temp-file "original" "modified" nil)
        (expect (macher-context-dirty-p context) :to-be-truthy))))

  (describe "macher--process-request"
    :var (context fsm temp-file build-patch-called)

    (before-each
      (setq temp-file (make-temp-file "macher-test"))
      (funcall setup-file)
      (setq context (macher--make-context :workspace `(file . ,file)))
      (setq fsm (gptel-make-fsm))
      (setq build-patch-called nil)
      ;; Mock macher--build-patch to track if it's called.
      (spy-on 'macher--build-patch :and-call-through))

    (after-each
      (when (file-exists-p temp-file)
        (delete-file temp-file)))

    (it "calls macher--build-patch when dirty-p is non-nil"
      ;; Set dirty-p to t.
      (setf (macher-context-dirty-p context) t)
      ;; Call macher--process-request.
      (macher--process-request 'test context fsm)
      ;; Verify macher--build-patch was called.
      (expect #'macher--build-patch :to-have-been-called))

    (it "does not call macher--build-patch when dirty-p is nil"
      ;; Ensure dirty-p is nil (default).
      (expect (macher-context-dirty-p context) :to-be nil)
      ;; Call macher--process-request.
      (macher--process-request 'test context fsm)
      ;; Verify macher--build-patch was not called.
      (expect #'macher--build-patch :not :to-have-been-called))

    (it "does not call macher--build-patch when context is nil"
      ;; Call with nil context.
      (macher--process-request 'test nil fsm)
      ;; Verify macher--build-patch was not called.
      (expect #'macher--build-patch :not :to-have-been-called)))

  (describe "macher--context-for-fsm"
    :var (context)

    (before-each
      (funcall setup-project)
      (setq context (macher--make-context :workspace `(project . ,project-dir))))

    (it "extracts context from FSM with macher tools"
      ;; Create a macher tool with our context.
      (let ((tool (macher--make-tool context :name "test_tool" :function (lambda () "test"))))
        (with-temp-buffer
          ;; Set up tools in the buffer as gptel would.
          (setq-local gptel-tools (list tool))
          ;; Create FSM using gptel-request with dry-run to get proper setup.
          (let ((fsm (gptel-request "test prompt" :buffer (current-buffer) :dry-run t)))
            ;; Should find the context.
            (expect (macher--context-for-fsm fsm) :to-be context)))))

    (it "returns nil when FSM has no macher tools"
      ;; Create a regular gptel tool without macher context.
      (let ((regular-tool (gptel-make-tool :name "regular" :function (lambda () "test"))))
        (with-temp-buffer
          ;; Set up tools in the buffer as gptel would.
          (setq-local gptel-tools (list regular-tool))
          ;; Create FSM using gptel-request with dry-run to get proper setup.
          (let ((fsm (gptel-request "test prompt" :buffer (current-buffer) :dry-run t)))
            ;; Should return nil.
            (expect (macher--context-for-fsm fsm) :to-be nil)))))

    (it "returns nil when FSM is nil"
      (expect (macher--context-for-fsm nil) :to-be nil))

    (it "returns nil when FSM has no tools"
      (with-temp-buffer
        ;; No tools set up.
        (setq-local gptel-tools nil)
        ;; Create FSM using gptel-request with dry-run.
        (let ((fsm (gptel-request "test prompt" :buffer (current-buffer) :dry-run t)))
          ;; FSM with no tools should return nil.
          (expect (macher--context-for-fsm fsm) :to-be nil)))))

  (describe "macher-process-request"
    :var (context original-process-fn dummy-process-fn fsm workspace)

    (before-each
      (funcall setup-project)
      ;; Store original function.
      (setq original-process-fn macher-process-request-function)
      ;; Set up a spy for the process function.
      (setf (symbol-function 'dummy-process-fn) (lambda (&rest _)))
      (spy-on 'dummy-process-fn)

      (setq workspace `(project . ,project-dir))
      (setq context
            (macher--make-context
             :workspace workspace
             :process-request-function #'dummy-process-fn))
      (setq fsm (gptel-make-fsm)))

    (after-each
      (setq macher-process-request-function original-process-fn))

    (it "calls process function with extracted context"
      (with-temp-buffer
        ;; Spy on macher--context-for-request and return a known context.
        (spy-on 'macher--context-for-fsm :and-return-value context)
        ;; Call macher-process-request.
        (macher-process-request 'test-reason fsm)
        ;; Should have called the process function with correct arguments.
        (expect 'dummy-process-fn :to-have-been-called-with 'test-reason context fsm)))

    (it "uses macher--fsm-latest when FSM not provided"
      ;; Spy on macher--context-for-request and return a known context.
      (spy-on 'macher--context-for-fsm :and-return-value context)
      (setq macher--fsm-latest fsm)
      ;; Call without explicit FSM.
      (macher-process-request 'test-reason)
      ;; Should have called the process function with macher--fsm-latest.
      (expect 'dummy-process-fn :to-have-been-called-with 'test-reason context fsm))

    (it "does nothing when no context found"
      ;; Spy on macher--context-for-request and return nil.
      (spy-on 'macher--context-for-fsm :and-return-value nil)
      ;; Call without any macher context.
      (macher-process-request 'test-reason fsm)
      ;; Should not have called the process function.
      (expect 'dummy-process-fn :not :to-have-been-called))


    (describe "macher-process-request-dwim"
      :var (action-buffer)

      (before-each
        (setq action-buffer (macher-action-buffer workspace t)))

      (it "uses local macher--fsm-latest when present"
        ;; Spy on macher--context-for-request and return a known context.
        (spy-on 'macher--context-for-fsm :and-return-value context)
        ;; Create a simple FSM and set as latest in current buffer.
        (with-temp-buffer
          (setq-local macher--workspace workspace)
          (setq macher--fsm-latest fsm)
          ;; Call with local FSM present.
          (macher-process-request-dwim 'test-reason)
          ;; Should have called the process function with the local FSM.
          (expect 'dummy-process-fn :to-have-been-called-with 'test-reason context fsm)))

      (it "falls back to action buffer macher--fsm-latest when local not present"
        ;; Spy on macher--context-for-request and return a known context.
        (spy-on 'macher--context-for-fsm :and-return-value context)
        ;; Create a simple FSM and set as latest in action buffer.
        (with-current-buffer action-buffer
          (setq macher--fsm-latest fsm))
        ;; Call from a different buffer without local FSM.
        (with-temp-buffer
          (setq-local macher--workspace workspace)
          ;; Explicitly clear local FSM to ensure fallback behavior.
          (setq macher--fsm-latest nil)
          (macher-process-request-dwim 'test-reason)
          ;; Should have called the process function with the action buffer FSM.
          (expect 'dummy-process-fn :to-have-been-called-with 'test-reason context fsm)))))

  (describe "macher--read-string"
    (it "returns full content when no offset or limit specified"
      (let ((content "line1\nline2\nline3\nline4"))
        (expect (macher--read-string content) :to-equal content)))

    (it "returns first N lines when only limit is specified"
      (let ((content "line1\nline2\nline3\nline4"))
        (expect (macher--read-string content nil 2) :to-equal "line1\nline2")
        (expect (macher--read-string content nil 1) :to-equal "line1")
        (expect (macher--read-string content nil 0) :to-equal "")))

    (it "returns lines from offset to end when only offset is specified"
      (let ((content "line1\nline2\nline3\nline4"))
        (expect (macher--read-string content 2) :to-equal "line2\nline3\nline4")
        (expect (macher--read-string content 3) :to-equal "line3\nline4")
        (expect (macher--read-string content 4) :to-equal "line4")))

    (it "returns specific range when both offset and limit are specified"
      (let ((content "line1\nline2\nline3\nline4"))
        (expect (macher--read-string content 2 2) :to-equal "line2\nline3")
        (expect (macher--read-string content 1 1) :to-equal "line1")
        (expect (macher--read-string content 3 1) :to-equal "line3")))

    (it "handles limit larger than total lines"
      (let ((content "line1\nline2"))
        (expect (macher--read-string content nil 5) :to-equal "line1\nline2")))

    (it "handles offset larger than total lines"
      (let ((content "line1\nline2"))
        (expect (macher--read-string content 5) :to-equal "")))

    (it "handles offset and limit beyond content bounds"
      (let ((content "line1\nline2"))
        (expect (macher--read-string content 2 5) :to-equal "line2")
        (expect (macher--read-string content 3 2) :to-equal "")))

    (it "handles empty content"
      (let ((content ""))
        (expect (macher--read-string content) :to-equal "")
        (expect (macher--read-string content nil 5) :to-equal "")
        (expect (macher--read-string content 1) :to-equal "")
        (expect (macher--read-string content 1 3) :to-equal "")))

    (it "handles single line content"
      (let ((content "single line"))
        (expect (macher--read-string content nil 1) :to-equal "single line")
        (expect (macher--read-string content 1) :to-equal "single line")
        (expect (macher--read-string content 1 1) :to-equal "single line")))

    (it "handles content with trailing newlines"
      (let ((content "line1\nline2\n"))
        (expect (macher--read-string content nil 1) :to-equal "line1")
        (expect (macher--read-string content 2 1) :to-equal "line2")
        (expect (macher--read-string content 3 1) :to-equal "")))

    (it "handles zero offset (treats as 1)"
      (let ((content "line1\nline2\nline3"))
        (expect (macher--read-string content 0) :to-equal "line1\nline2\nline3")
        (expect (macher--read-string content 0 2) :to-equal "line1\nline2")))

    (describe "negative offset/limit handling"
      (it "handles negative offset (counts from end)"
        (let ((content "line1\nline2\nline3\nline4"))
          ;; -1 means start at 1 line before the end (line4)
          (expect (macher--read-string content -1) :to-equal "line4")
          ;; -2 means start at 2 lines before the end (line3)
          (expect (macher--read-string content -2) :to-equal "line3\nline4")
          ;; -3 means start at 3 lines before the end (line2)
          (expect (macher--read-string content -3) :to-equal "line2\nline3\nline4")
          ;; -4 means start at 4 lines before the end (line1)
          (expect (macher--read-string content -4) :to-equal "line1\nline2\nline3\nline4")
          ;; -5 (beyond bounds) should start at beginning
          (expect (macher--read-string content -5) :to-equal "line1\nline2\nline3\nline4")))

      (it "handles negative limit (counts from total)"
        (let ((content "line1\nline2\nline3\nline4"))
          ;; -1 means limit = 4 - 1 = 3 lines
          (expect (macher--read-string content nil -1) :to-equal "line1\nline2\nline3")
          ;; -2 means limit = 4 - 2 = 2 lines
          (expect (macher--read-string content nil -2) :to-equal "line1\nline2")
          ;; -3 means limit = 4 - 3 = 1 line
          (expect (macher--read-string content nil -3) :to-equal "line1")
          ;; -4 means limit = 4 - 4 = 0 lines
          (expect (macher--read-string content nil -4) :to-equal "")
          ;; -5 (beyond bounds) should result in 0 lines
          (expect (macher--read-string content nil -5) :to-equal "")))

      (it "handles negative offset and limit together"
        (let ((content "line1\nline2\nline3\nline4\nline5"))
          ;; Start at -2 (line4) and read -2 lines (5 - 2 = 3 lines)
          (expect (macher--read-string content -2 -2) :to-equal "line4\nline5")
          ;; Start at -3 (line3) and read -3 lines (5 - 3 = 2 lines)
          (expect (macher--read-string content -3 -3) :to-equal "line3\nline4")
          ;; Start at -1 (line5) and read -4 lines (5 - 4 = 1 line)
          (expect (macher--read-string content -1 -4) :to-equal "line5")))

      (it "handles mixed positive and negative values"
        (let ((content "line1\nline2\nline3\nline4\nline5"))
          ;; Start at positive offset 2 (line2) and read -2 lines (5 - 2 = 3 lines)
          (expect (macher--read-string content 2 -2) :to-equal "line2\nline3\nline4")
          ;; Start at negative offset -3 (line3) and read positive 2 lines
          (expect (macher--read-string content -3 2) :to-equal "line3\nline4")))

      (it "handles edge cases for negative values"
        (let ((content "line1\nline2"))
          ;; Negative offset beyond bounds
          (expect (macher--read-string content -10) :to-equal "line1\nline2")
          ;; Negative limit beyond bounds
          (expect (macher--read-string content nil -10) :to-equal "")
          ;; Negative offset and limit both beyond bounds
          (expect (macher--read-string content -10 -10) :to-equal "")))

      (it "handles empty content with negative values"
        (let ((content ""))
          (expect (macher--read-string content -1) :to-equal "")
          (expect (macher--read-string content nil -1) :to-equal "")
          (expect (macher--read-string content -1 -1) :to-equal "")))

      (it "handles single line content with negative values"
        (let ((content "single line"))
          ;; -1 offset should give the single line
          (expect (macher--read-string content -1) :to-equal "single line")
          ;; -1 limit should give 1 - 1 = 0 lines
          (expect (macher--read-string content nil -1) :to-equal "")
          ;; -1 offset and -1 limit together
          (expect (macher--read-string content -1 -1) :to-equal "")))

      (it "handles content with trailing newlines and negative offset/limit"
        (let ((content "line1\nline2\nline3\n")) ;; Note trailing newline
          ;; -1 offset should give the empty last "line" after the trailing newline
          (expect (macher--read-string content -1) :to-equal "")
          ;; -2 offset should give line3 + trailing newline
          (expect (macher--read-string content -2) :to-equal "line3\n")
          ;; -3 offset should give line2 + line3 + trailing newline
          (expect (macher--read-string content -3) :to-equal "line2\nline3\n")
          ;; -1 limit should give 4 - 1 = 3 lines (including the empty trailing line)
          (expect (macher--read-string content nil -1) :to-equal "line1\nline2\nline3")
          ;; -2 limit should give 4 - 2 = 2 lines
          (expect (macher--read-string content nil -2) :to-equal "line1\nline2")
          ;; Combine negative offset and limit with trailing newline
          (expect (macher--read-string content -2 -3) :to-equal "line3")))

      (it "handles multiple trailing newlines with negative offset/limit"
        (let ((content "line1\nline2\n\n\n")) ;; Multiple trailing newlines
          ;; -1 offset should give the last empty line
          (expect (macher--read-string content -1) :to-equal "")
          ;; -2 offset should give the second-to-last empty line and the last
          (expect (macher--read-string content -2) :to-equal "\n")
          ;; -3 offset should give empty line + empty line + empty line
          (expect (macher--read-string content -3) :to-equal "\n\n")
          ;; -4 offset should give line2 + all trailing newlines
          (expect (macher--read-string content -4) :to-equal "line2\n\n\n")
          ;; -1 limit should give 5 - 1 = 4 lines
          (expect (macher--read-string content nil -1) :to-equal "line1\nline2\n\n")
          ;; -2 limit should give 5 - 2 = 3 lines
          (expect (macher--read-string content nil -2) :to-equal "line1\nline2\n")
          ;; Combine negative offset and limit
          (expect (macher--read-string content -3 -4) :to-equal "")))

      (it "handles content that ends without newline and negative offset/limit"
        (let ((content "line1\nline2\nline3")) ;; No trailing newline
          ;; -1 offset should give line3
          (expect (macher--read-string content -1) :to-equal "line3")
          ;; -2 offset should give line2 + line3
          (expect (macher--read-string content -2) :to-equal "line2\nline3")
          ;; -1 limit should give 3 - 1 = 2 lines
          (expect (macher--read-string content nil -1) :to-equal "line1\nline2")
          ;; -2 limit should give 3 - 2 = 1 line
          (expect (macher--read-string content nil -2) :to-equal "line1")))

      (it "handles single line with trailing newline and negative offset/limit"
        (let ((content "single line\n")) ;; Single line with trailing newline
          ;; -1 offset should give the empty line after the newline
          (expect (macher--read-string content -1) :to-equal "")
          ;; -2 offset should give the single line + newline
          (expect (macher--read-string content -2) :to-equal "single line\n")
          ;; -1 limit should give 2 - 1 = 1 line
          (expect (macher--read-string content nil -1) :to-equal "single line")
          ;; -2 limit should give 2 - 2 = 0 lines
          (expect (macher--read-string content nil -2) :to-equal "")
          ;; Combine negative offset and limit
          (expect (macher--read-string content -1 -1) :to-equal ""))))

    (describe "numbered parameter support"
      (it "returns content with line numbers when numbered is true"
        (let ((content "line1\nline2\nline3\nline4"))
          (expect (macher--read-string content nil nil t)
                  :to-equal "1\tline1\n2\tline2\n3\tline3\n4\tline4")))

      (it "handles single line content with numbered parameter"
        (let ((content "single line"))
          (expect (macher--read-string content nil nil t) :to-equal "1\tsingle line")))

      (it "handles empty content with numbered parameter"
        (let ((content ""))
          (expect (macher--read-string content nil nil t) :to-equal "1\t")))

      (it "doesn't number the trailing newline, if any"
        (let ((content "line1\nline2\n"))
          (expect (macher--read-string content nil nil t) :to-equal "1\tline1\n2\tline2\n")))

      (it "combines numbered parameter with offset"
        (let ((content "line1\nline2\nline3\nline4"))
          (expect (macher--read-string content 2 nil t) :to-equal "2\tline2\n3\tline3\n4\tline4")))

      (it "combines numbered parameter with limit"
        (let ((content "line1\nline2\nline3\nline4"))
          (expect (macher--read-string content nil 2 t) :to-equal "1\tline1\n2\tline2")))

      (it "combines numbered parameter with both offset and limit"
        (let ((content "line1\nline2\nline3\nline4"))
          (expect (macher--read-string content 2 2 t) :to-equal "2\tline2\n3\tline3")))

      (it "correctly formats line numbers with proper spacing for content with 100+ lines"
        (let* ((lines (cl-loop for i from 1 to 105 collect (format "line%d" i)))
               (content (string-join lines "\n")))
          (let ((result (macher--read-string content nil nil t)))
            ;; Should have proper spacing for all line numbers.
            (expect result :to-match "^  2\tline2\n")
            (expect result :to-match "^ 99\tline99\n")
            (expect result :to-match "^100\tline100\n")
            (expect result :to-match "^101\tline101\n")
            (expect result :to-match "^105\tline105$")
            (let ((lines (split-string result "\n")))
              (expect (length lines) :to-equal 105)
              ;; Check that all lines have the same format with proper alignment.
              (dolist (line lines)
                (when (not (string-empty-p line))
                  ;; Each line should have the format: "NNN\tlineNNN" where NNN is right-aligned.
                  (expect line :to-match "^[ 0-9]+\t")
                  ;; The tab should be at the same position for all lines (after 3 characters for 3-digit max).
                  (expect (string-match "\t" line) :to-equal 3)))))))

      (it "handles negative offset with numbered parameter"
        (let ((content "line1\nline2\nline3\nline4"))
          ;; -2 offset should start at line3 (3rd line) and show line numbers 3 and 4
          (expect (macher--read-string content -2 nil t) :to-equal "3\tline3\n4\tline4")))

      (it "handles negative limit with numbered parameter"
        (let ((content "line1\nline2\nline3\nline4"))
          ;; -2 limit should show first 4-2=2 lines with line numbers 1 and 2
          (expect (macher--read-string content nil -2 t) :to-equal "1\tline1\n2\tline2")))

      (it "maintains proper spacing for line numbers when using different ranges"
        (let* ((lines (cl-loop for i from 1 to 50 collect (format "content%d" i)))
               (content (string-join lines "\n")))
          ;; Test different ranges to ensure spacing is calculated correctly for each range

          ;; Range 1-9: single digits, should use 1 character width
          (let ((result1 (macher--read-string content 1 9 t)))
            (expect result1 :to-match "^1\tcontent1\n")
            (expect result1 :to-match "^9\tcontent9$")
            (expect (string-match "\t" result1) :to-equal 1))

          ;; Range 5-15: spans single to double digits, should use 2 character width
          (let ((result2 (macher--read-string content 5 11 t)))
            (expect result2 :to-match "^ 5\tcontent5\n")
            (expect result2 :to-match "^ 9\tcontent9\n")
            (expect result2 :to-match "^10\tcontent10\n")
            (expect result2 :to-match "^15\tcontent15$")
            ;; All tabs should be at position 2 (after 2 characters)
            (let ((lines (split-string result2 "\n")))
              (dolist (line lines)
                (when (not (string-empty-p line))
                  (expect (string-match "\t" line) :to-equal 2))))))))

    (describe "trailing newline behavior"
      (it "preserves original string exactly when no parameters specified"
        ;; No parameters = return original string unchanged
        (let ((content-with-newline "line1\nline2\n"))
          (expect (macher--read-string content-with-newline) :to-equal "line1\nline2\n"))
        (let ((content-without-newline "line1\nline2"))
          (expect (macher--read-string content-without-newline) :to-equal "line1\nline2")))

      (it "preserves trailing newline behavior with offset-only"
        ;; Offset only = shouldn't affect trailing newline presence
        (let ((content-with-newline "line1\nline2\nline3\n"))
          (expect (macher--read-string content-with-newline 2) :to-equal "line2\nline3\n"))
        (let ((content-without-newline "line1\nline2\nline3"))
          (expect (macher--read-string content-without-newline 2) :to-equal "line2\nline3")))

      (it "removes trailing newline with limit parameter by default"
        ;; Limit parameter = no trailing newline unless it's part of the limited content
        (let ((content "line1\nline2\nline3\nline4"))
          (expect (macher--read-string content nil 2) :to-equal "line1\nline2")
          (expect (macher--read-string content nil 3) :to-equal "line1\nline2\nline3"))
        ;; But if the original content's trailing newline is within the limit, include it
        (let ((content-with-newline "line1\nline2\nline3\n"))
          (expect (macher--read-string content-with-newline nil 3) :to-equal "line1\nline2\nline3")
          (expect (macher--read-string content-with-newline nil 4)
                  :to-equal "line1\nline2\nline3\n")))

      (it "handles trailing newline with offset and limit combined"
        ;; Combined offset+limit = same limit behavior applies
        (let ((content "line1\nline2\nline3\nline4"))
          (expect (macher--read-string content 2 2) :to-equal "line2\nline3"))
        (let ((content-with-newline "line1\nline2\nline3\n"))
          (expect (macher--read-string content-with-newline 2 2) :to-equal "line2\nline3")
          (expect (macher--read-string content-with-newline 2 3) :to-equal "line2\nline3\n")))

      (it "handles multiple trailing newlines correctly"
        ;; Multiple trailing newlines
        (let ((content "line1\nline2\n\n\n"))
          ;; All lines without limit = preserve all trailing newlines
          (expect (macher--read-string content) :to-equal "line1\nline2\n\n\n")
          ;; With limit = include trailing newlines only if within limit
          (expect (macher--read-string content nil 2) :to-equal "line1\nline2")
          (expect (macher--read-string content nil 3) :to-equal "line1\nline2\n")
          (expect (macher--read-string content nil 4) :to-equal "line1\nline2\n\n")
          (expect (macher--read-string content nil 5) :to-equal "line1\nline2\n\n\n")))

      (it "handles empty lines at end vs trailing newlines"
        ;; Distinguish between empty lines and trailing newlines
        (let ((content-empty-line "line1\nline2\n"))
          (expect (macher--read-string content-empty-line nil 2) :to-equal "line1\nline2")
          (expect (macher--read-string content-empty-line nil 3) :to-equal "line1\nline2\n"))
        (let ((content-empty-lines "line1\nline2\n\n"))
          (expect (macher--read-string content-empty-lines nil 2) :to-equal "line1\nline2")
          (expect (macher--read-string content-empty-lines nil 3) :to-equal "line1\nline2\n")
          (expect (macher--read-string content-empty-lines nil 4) :to-equal "line1\nline2\n\n")))

      (describe "numbered parameter and trailing newlines"
        (it "replicates cat -n behavior for trailing newlines"
          ;; cat -n shows final trailing newline without a number
          (let ((content "line1\nline2\n"))
            (expect (macher--read-string content nil nil t) :to-equal "1\tline1\n2\tline2\n"))
          (let ((content "line1\nline2"))
            (expect (macher--read-string content nil nil t) :to-equal "1\tline1\n2\tline2")))

        (it "numbers all but the last trailing newline"
          ;; Multiple trailing newlines: all but the last get numbered
          (let ((content "line1\nline2\n\n\n"))
            (expect (macher--read-string content nil nil t)
                    :to-equal "1\tline1\n2\tline2\n3\t\n4\t\n"))
          (let ((content "line1\n\n\n"))
            (expect (macher--read-string content nil nil t) :to-equal "1\tline1\n2\t\n3\t\n")))

        (it "numbers blank lines in middle of content"
          ;; Blank lines in the middle always get numbered
          (let ((content "line1\n\nline3\n"))
            (expect (macher--read-string content nil nil t) :to-equal "1\tline1\n2\t\n3\tline3\n"))
          (let ((content "line1\n\n\nline4"))
            (expect (macher--read-string content nil nil t)
                    :to-equal "1\tline1\n2\t\n3\t\n4\tline4")))

        (it "numbers blank lines in the middle of the content, even if at the end of the limit"
          ;; When limit cuts off content, "trailing" newlines in the middle get numbered.
          (let ((content "line1\nline2\n\nline4\nline5"))
            (expect (macher--read-string content nil 3 t) :to-equal "1\tline1\n2\tline2\n3\t")
            (expect (macher--read-string content nil 4 t)
                    :to-equal "1\tline1\n2\tline2\n3\t\n4\tline4")))

        (it "combines numbered with offset and doesn't number trailing newlines"
          ;; Numbered with offset should still follow cat -n behavior.
          (let ((content "line1\nline2\nline3\n"))
            (expect (macher--read-string content 2 nil t) :to-equal "2\tline2\n3\tline3\n"))
          (let ((content "line1\nline2\nline3"))
            (expect (macher--read-string content 2 nil t) :to-equal "2\tline2\n3\tline3")))

        (it "combines numbered with limit and doesn't number trailing newlines"
          ;; With trailing newline.
          (let ((content "line1\nline2\nline3\nline4\n"))
            (expect (macher--read-string content nil 3 t) :to-equal "1\tline1\n2\tline2\n3\tline3")
            (expect (macher--read-string content nil 4 t)
                    :to-equal "1\tline1\n2\tline2\n3\tline3\n4\tline4")
            (expect (macher--read-string content nil 5 t)
                    :to-equal "1\tline1\n2\tline2\n3\tline3\n4\tline4\n"))

          ;; No trailing newline.
          (let ((content "line1\nline2\nline3"))
            (expect (macher--read-string content nil 4 t)
                    :to-equal "1\tline1\n2\tline2\n3\tline3"))))))

  (describe "macher--edit-string"
    (describe "basic replacement behavior"
      (it "replaces single occurrence of old text with new text"
        (let* ((content "Hello world! This is a test.")
               (old-text "world")
               (new-text "universe")
               (result (macher--edit-string content old-text new-text)))
          (expect result :to-equal "Hello universe! This is a test.")))

      (it "handles multiline content correctly"
        (let* ((content "Line 1\nLine 2\nLine 3")
               (old-text "Line 2")
               (new-text "Modified Line 2")
               (result (macher--edit-string content old-text new-text)))
          (expect result :to-equal "Line 1\nModified Line 2\nLine 3")))

      (it "replaces entire content when old text matches everything"
        (let* ((content "complete content")
               (old-text "complete content")
               (new-text "new content")
               (result (macher--edit-string content old-text new-text)))
          (expect result :to-equal "new content")))

      (it "handles empty new text (deletion)"
        (let* ((content "Remove this word from sentence")
               (old-text "this word ")
               (new-text "")
               (result (macher--edit-string content old-text new-text)))
          (expect result :to-equal "Remove from sentence"))))

    (describe "error conditions"
      (it "signals error when old_string and new_string are identical"
        (let ((content "Hello world")
              (old-text "world")
              (new-text "world"))
          (expect (macher--edit-string content old-text new-text) :to-throw)))

      (it "signals error when old_string is not found"
        (let ((content "Hello world")
              (old-text "missing")
              (new-text "replacement"))
          (expect (macher--edit-string content old-text new-text) :to-throw)))

      (it "signals error when multiple matches exist without replace-all"
        (let ((content "hello world hello universe")
              (old-text "hello")
              (new-text "hi"))
          (expect (macher--edit-string content old-text new-text) :to-throw))))

    (describe "exact whitespace matching"
      (it "requires exact whitespace match"
        (let* ((content "  function test() {\n    return 'hello';\n  }")
               (old-text "function test() {\n  return 'hello';\n}")
               (new-text "function test() {\n  return 'world';\n}"))
          ;; Should fail because whitespace doesn't match exactly.
          (expect (macher--edit-string content old-text new-text) :to-throw)))

      (it "succeeds with exact whitespace match"
        (let* ((content "  function test() {\n    return 'hello';\n  }")
               (old-text "  function test() {\n    return 'hello';\n  }")
               (new-text "  function test() {\n    return 'world';\n  }")
               (result (macher--edit-string content old-text new-text)))
          (expect result :to-equal "  function test() {\n    return 'world';\n  }")))

      (it "handles tabs and spaces exactly"
        (let* ((content "\tfunction test() {\n\t  return 'hello';\n\t}")
               (old-text "\tfunction test() {\n\t  return 'hello';\n\t}")
               (new-text "\tfunction test() {\n\t  return 'world';\n\t}")
               (result (macher--edit-string content old-text new-text)))
          (expect result :to-equal "\tfunction test() {\n\t  return 'world';\n\t}")))

      (it "fails when indentation differs"
        (let* ((content "    function test() {\n      return 'hello';\n    }")
               (old-text "  function test() {\n    return 'hello';\n  }")
               (new-text "  function test() {\n    return 'world';\n  }"))
          (expect (macher--edit-string content old-text new-text) :to-throw))))

    (describe "replace-all functionality"
      (it "replaces all occurrences when replace-all is true"
        (let* ((content "hello world hello universe hello there")
               (old-text "hello")
               (new-text "hi")
               (result (macher--edit-string content old-text new-text t)))
          (expect result :to-equal "hi world hi universe hi there")))

      (it "replaces all occurrences in multiline content"
        (let* ((content "hello world\nhello universe\nhello there")
               (old-text "hello")
               (new-text "hi")
               (result (macher--edit-string content old-text new-text t)))
          (expect result :to-equal "hi world\nhi universe\nhi there")))

      (it "handles replace-all with exact whitespace matching"
        (let* ((content "  hello world\n  hello universe\n  hello there")
               (old-text "  hello")
               (new-text "  hi")
               (result (macher--edit-string content old-text new-text t)))
          (expect result :to-equal "  hi world\n  hi universe\n  hi there")))

      (it "replace-all works with single occurrence"
        (let* ((content "hello world")
               (old-text "hello")
               (new-text "hi")
               (result (macher--edit-string content old-text new-text t)))
          (expect result :to-equal "hi world"))))

    (describe "function signature compatibility"
      (it "accepts replace-all parameter as fourth argument"
        (let* ((content "hello world hello universe")
               (old-text "hello")
               (new-text "hi"))
          ;; Should error when multiple matches exist without replace-all.
          (expect (macher--edit-string content old-text new-text nil) :to-throw)
          ;; Should replace all when replace-all is true.
          (let ((result (macher--edit-string content old-text new-text t)))
            (expect result :to-equal "hi world hi universe"))))

      (it "works with default behavior when replace-all is omitted"
        (let* ((content "hello world")
               (old-text "hello")
               (new-text "hi")
               (result (macher--edit-string content old-text new-text)))
          (expect result :to-equal "hi world"))))


    (describe "literal string matching (no regex support)"
      (it "treats oldText as literal string, not regex pattern"
        (let* ((content "function test() { return /hello/; }")
               (old-text "/hello/") ;; This looks like regex but should be treated literally
               (new-text "/world/")
               (result (macher--edit-string content old-text new-text)))
          ;; Should match the literal string "/hello/", not as regex
          (expect result :to-equal "function test() { return /world/; }")))

      (it "handles special regex characters as literal"
        (let* ((content "price is $100 (approx)")
               (old-text "$100") ;; $ is regex metacharacter but should be literal
               (new-text "$200")
               (result (macher--edit-string content old-text new-text)))
          ;; Should match the literal string "$100", not as regex
          (expect result :to-equal "price is $200 (approx)")))

      (it "handles brackets as literal characters"
        (let* ((content "array[0] = value")
               ;; Brackets are regex metacharacters but should be literal.
               (old-text "array[0]")
               (new-text "list[0]")
               (result (macher--edit-string content old-text new-text)))
          ;; Should match the literal string "array[0]", not as regex.
          (expect result :to-equal "list[0] = value")))))

  (describe "macher--format-size"
    (it "formats bytes correctly"
      (expect (macher--format-size 0) :to-equal "0 B")
      (expect (macher--format-size 512) :to-equal "512 B")
      (expect (macher--format-size 1024) :to-equal "1.0 KB")
      (expect (macher--format-size 1536) :to-equal "1.5 KB")
      (expect (macher--format-size 1048576) :to-equal "1.0 MB")
      (expect (macher--format-size 1073741824) :to-equal "1.0 GB")))

  (describe "macher--tool-list-directory"
    :var (context temp-dir)

    (before-each
      (setq temp-dir (make-temp-file "macher-test-dir" t))
      (setq context
            (macher--make-context :workspace (cons 'project (file-name-as-directory temp-dir))))
      ;; Create test directory structure.
      (make-directory (expand-file-name "subdir" temp-dir))
      (make-directory (expand-file-name "subdir/nested" temp-dir))
      (write-region "file1 content" nil (expand-file-name "file1.txt" temp-dir))
      (write-region "file2 content" nil (expand-file-name "file2.el" temp-dir))
      (write-region "nested file content" nil (expand-file-name "subdir/nested/file.txt" temp-dir))

      ;; Add the project marker.
      (write-region "" nil (expand-file-name ".project" temp-dir)))

    (after-each
      (when (and temp-dir (file-exists-p temp-dir))
        (delete-directory temp-dir t)))

    (it "lists directory contents without recursion or sizes"
      (let ((result (macher--tool-list-directory context ".")))
        (expect result :to-match "file: file1.txt")
        (expect result :to-match "file: file2.el")
        (expect result :to-match "dir: subdir")
        (expect result :not :to-match "nested")))

    (it "lists directory contents with sizes"
      (let ((result (macher--tool-list-directory context "." nil t)))
        (expect result :to-match "file: file1.txt (13 B)")
        (expect result :to-match "file: file2.el (13 B)")
        ;; No size for directories.
        (expect result :to-match "dir: subdir$")))

    (it "lists directory contents recursively"
      (let ((result (macher--tool-list-directory context "." t)))
        (expect result :to-match "file: file1.txt")
        (expect result :to-match "file: file2.el")
        (expect result :to-match "dir: subdir")
        (expect result :to-match "  dir: subdir/nested")
        (expect result :to-match "    file: subdir/nested/file.txt")))

    (it "lists directory contents recursively with sizes"
      (let ((result (macher--tool-list-directory context "." t t)))
        (expect result :to-match "file: file1.txt (13 B)")
        (expect result :to-match "file: file2.el (13 B)")
        (expect result :to-match "dir: subdir$")
        (expect result :to-match "  dir: subdir/nested$")
        (expect result :to-match "    file: subdir/nested/file.txt (19 B)")))

    (it "lists subdirectory contents"
      (let ((result (macher--tool-list-directory context "subdir")))
        (expect result :to-match "dir: nested")
        (expect result :not :to-match "file1.txt")
        (expect result :not :to-match "file2.el")))

    (it "handles empty directories"
      (let ((empty-dir (expand-file-name "empty" temp-dir)))
        (make-directory empty-dir)
        (let ((result (macher--tool-list-directory context "empty")))
          (expect result :to-equal "Directory is empty"))))

    (it "takes macher context into account - shows created files"
      ;; Create a file in the macher context.
      (macher-context--set-new-content-for-file
       (expand-file-name "new-file.txt" temp-dir) "new content" context)
      (let ((result (macher--tool-list-directory context ".")))
        (expect result :to-match "file: new-file.txt")))

    (it "takes macher context into account - hides deleted files"
      ;; Delete a file in the macher context.
      (macher-context--set-new-content-for-file (expand-file-name "file1.txt" temp-dir) nil context)
      (let ((result (macher--tool-list-directory context ".")))
        (expect result :not :to-match "file1.txt")
        (expect result :to-match "file: file2.el")))

    (it "takes macher context into account - shows correct sizes for modified files"
      ;; Modify a file in the macher context.
      (let ((longer-content "modified content that is longer"))
        (macher-context--set-new-content-for-file
         (expand-file-name "file1.txt" temp-dir) longer-content context)
        (let ((result (macher--tool-list-directory context "." nil t)))
          (expect result :to-match (format "file: file1.txt (%d B)" (length longer-content)))
          (expect result :to-match "file: file2.el (13 B)"))))

    (it "signals error for non-existent directory"
      (expect (macher--tool-list-directory context "non-existent") :to-throw))

    (it "signals error when path is a file"
      (expect (macher--tool-list-directory context "file1.txt") :to-throw))

    (it "includes real dotfiles but excludes . and .. meta-directories"
      ;; Create test files including dotfiles
      (write-region "gitignore content" nil (expand-file-name ".gitignore" temp-dir))
      (write-region "prettierrc content" nil (expand-file-name ".prettierrc" temp-dir))
      (make-directory (expand-file-name ".hidden" temp-dir) t)
      (write-region "hidden dir file" nil (expand-file-name ".hidden/file.txt" temp-dir))

      (let ((result (macher--tool-list-directory context ".")))
        ;; Should include real dotfiles
        (expect result :to-match "file: \\.gitignore")
        (expect result :to-match "file: \\.prettierrc")
        (expect result :to-match "dir: \\.hidden")
        ;; Should NOT include . and .. meta-directories
        (expect result :not :to-match "dir: \\.$")
        (expect result :not :to-match "dir: \\.\\.$")
        ;; Verify it doesn't cause infinite recursion by being able to complete
        (expect (stringp result) :to-be-truthy)))

    (it "does not load files into context when listing directories"
      ;; Create additional subdirectories with files to test with.
      (let ((subdir2 (expand-file-name "subdir2" temp-dir))
            (subdir3 (expand-file-name "subdir/subdir3" temp-dir)))
        (make-directory subdir2)
        (make-directory subdir3)
        (write-region "file in subdir2" nil (expand-file-name "file4.txt" subdir2))
        (write-region "file in nested subdir3" nil (expand-file-name "file5.txt" subdir3))

        ;; Verify context starts empty.
        (expect (macher-context-contents context) :to-be nil)

        ;; Call list-directory on various directories multiple times.
        (macher--tool-list-directory context ".")
        (expect (macher-context-contents context) :to-be nil)

        (macher--tool-list-directory context "subdir")
        (expect (macher-context-contents context) :to-be nil)

        (macher--tool-list-directory context "subdir2")
        (expect (macher-context-contents context) :to-be nil)

        ;; Recursive.
        (macher--tool-list-directory context "subdir" t)
        (expect (macher-context-contents context) :to-be nil)

        ;; Recursive with sizes.
        (macher--tool-list-directory context "." t t)
        (expect (macher-context-contents context) :to-be nil)

        ;; Verify that the context contents remain empty after all calls.
        (expect (macher-context-contents context) :to-be nil)))

    (it "shows symlinks with special prefix and target"
      ;; Create a symlink to a file.
      (let ((symlink-path (expand-file-name "file-symlink" temp-dir))
            (target-path (expand-file-name "file1.txt" temp-dir)))
        (make-symbolic-link target-path symlink-path)

        ;; Mock workspace files to include the symlink.
        (spy-on 'macher--workspace-files
                :and-call-fake
                (lambda (workspace)
                  (append (macher--project-files (cdr workspace)) (list symlink-path))))

        (let ((result (macher--tool-list-directory context ".")))
          (expect result :to-match "link: file-symlink ->")
          (expect result :to-match "file: file1.txt")
          (expect result :to-match "file: file2.el"))

        ;; Clean up.
        (delete-file symlink-path))

      ;; Create a symlink to a directory.
      (let ((dir-symlink-path (expand-file-name "dir-symlink" temp-dir))
            (target-dir (expand-file-name "subdir" temp-dir)))
        (make-symbolic-link target-dir dir-symlink-path)

        ;; Mock workspace files to include the directory symlink
        (spy-on 'macher--workspace-files
                :and-call-fake
                (lambda (workspace)
                  (append (macher--project-files (cdr workspace)) (list dir-symlink-path))))

        (let ((result (macher--tool-list-directory context ".")))
          (expect result :to-match "link: dir-symlink ->")
          (expect result :to-match "dir: subdir"))

        ;; Clean up.
        (delete-file dir-symlink-path))

      ;; Create a broken symlink.
      (let ((broken-symlink-path (expand-file-name "broken-symlink" temp-dir)))
        (make-symbolic-link "/nonexistent/target" broken-symlink-path)

        ;; Mock workspace files to include the broken symlink.
        (spy-on 'macher--workspace-files
                :and-call-fake
                (lambda (workspace)
                  (append (macher--project-files (cdr workspace)) (list broken-symlink-path))))

        (let ((result (macher--tool-list-directory context ".")))
          (expect result :to-match "link: broken-symlink -> /nonexistent/target"))

        ;; Clean up.
        (delete-file broken-symlink-path)))

    (it "lists contents of directories created through context (new files in non-existent directories)"
      ;; Test that we can list directories that don't exist on disk but contain new files in the
      ;; context. This validates the behavior when files are added to directories that didn't yet
      ;; exist.

      ;; Create a new file in a directory that doesn't exist on disk.
      (macher-context--set-new-content-for-file
       (expand-file-name "newdir/newfile.txt" temp-dir) "new file content" context)

      ;; Should be able to list the contents of the new directory.
      (let ((result (macher--tool-list-directory context "newdir")))
        (expect result :to-match "file: newfile.txt"))

      ;; Test with multiple nested subdirectories.
      (macher-context--set-new-content-for-file
       (expand-file-name "deep/nested/path/file1.txt" temp-dir) "deep file 1" context)
      (macher-context--set-new-content-for-file
       (expand-file-name "deep/nested/path/file2.txt" temp-dir) "deep file 2" context)
      (macher-context--set-new-content-for-file
       (expand-file-name "deep/nested/other/file3.txt" temp-dir) "deep file 3" context)

      ;; Should be able to list the deeply nested directory.
      (let ((result (macher--tool-list-directory context "deep/nested/path")))
        (expect result :to-match "file: file1.txt")
        (expect result :to-match "file: file2.txt")
        ;; file3.txt is in ../other/.
        (expect result :not :to-match "file3.txt"))

      ;; Should be able to list intermediate directories.
      (let ((result (macher--tool-list-directory context "deep/nested")))
        (expect result :to-match "dir: path")
        (expect result :to-match "dir: other"))

      ;; Test recursive listing from root to see the entire structure.
      (let ((result (macher--tool-list-directory context "." t)))
        (expect result :to-match "dir: newdir")
        (expect result :to-match "  file: newdir/newfile.txt")
        (expect result :to-match "dir: deep")
        (expect result :to-match "  dir: deep/nested")
        (expect result :to-match "    dir: deep/nested/path")
        (expect result :to-match "      file: deep/nested/path/file1.txt")
        (expect result :to-match "      file: deep/nested/path/file2.txt")
        (expect result :to-match "    dir: deep/nested/other")
        (expect result :to-match "      file: deep/nested/other/file3.txt"))

      ;; Test mixed scenario: existing directories with new files.
      (macher-context--set-new-content-for-file
       (expand-file-name "subdir/newfile-in-existing-dir.txt" temp-dir) "mixed content" context)

      (let ((result (macher--tool-list-directory context "subdir")))
        ;; New file.
        (expect result :to-match "file: newfile-in-existing-dir.txt")
        ;; Existing directory.
        (expect result :to-match "dir: nested"))

      ;; Test with sizes for new files.
      (let ((result (macher--tool-list-directory context "newdir" nil t)))
        (expect result :to-match "file: newfile.txt (16 B)"))

      ;; Test edge case: empty directory path in deeply nested structure.
      (macher-context--set-new-content-for-file
       (expand-file-name "edge/case/deeply/nested/empty.txt" temp-dir) "" context)

      (let ((result (macher--tool-list-directory context "edge/case/deeply/nested" nil t)))
        (expect result :to-match "file: empty.txt (0 B)")))

    (it "does not list files that aren't in context or workspace files list"
      ;; Create files on disk that are not included in the workspace's files list.
      (let ((untracked-file1 (expand-file-name "untracked1.txt" temp-dir))
            (untracked-file2 (expand-file-name "untracked2.txt" temp-dir))
            (subdir-untracked (expand-file-name "subdir/untracked-sub.txt" temp-dir)))
        (write-region "untracked content 1" nil untracked-file1)
        (write-region "untracked content 2" nil untracked-file2)
        (write-region "untracked sub content" nil subdir-untracked)

        ;; Mock workspace files to exclude the untracked files from the files list.
        ;; This simulates files that exist on disk but aren't tracked by the workspace.
        (spy-on 'macher--workspace-files
                :and-call-fake
                (lambda (workspace)
                  ;; Get the actual files from the real function but filter out untracked files.
                  (let ((files (macher--project-files (cdr workspace))))
                    (cl-remove-if
                     (lambda (file)
                       (or (string-match-p "untracked1\\.txt$" file)
                           (string-match-p "untracked2\\.txt$" file)
                           (string-match-p "untracked-sub\\.txt$" file)))
                     files))))

        ;; Test that untracked files don't appear in directory listing.
        (let ((result (macher--tool-list-directory context ".")))
          ;; Should contain workspace files.
          (expect result :to-match "file: file1.txt")
          (expect result :to-match "file: file2.el")
          (expect result :to-match "dir: subdir")
          ;; Should NOT contain untracked files.
          (expect result :not :to-match "untracked1.txt")
          (expect result :not :to-match "untracked2.txt"))

        ;; Test subdirectory as well.
        (let ((result (macher--tool-list-directory context "subdir")))
          ;; Should contain workspace files.
          (expect result :to-match "dir: nested")
          ;; Should NOT contain untracked files.
          (expect result :not :to-match "untracked-sub.txt"))

        ;; However, files in the context (even if not on disk) should appear.
        (macher-context--set-new-content-for-file
         (expand-file-name "context-only-file.txt" temp-dir) "context content" context)

        (let ((result (macher--tool-list-directory context ".")))
          ;; Should show context-only file even though it's not on disk.
          (expect result :to-match "file: context-only-file.txt")
          ;; Still should not show untracked disk files.
          (expect result :not :to-match "untracked1.txt"))

        ;; Test that modified files in context are still shown with correct content size.
        (let ((modified-content "modified content that is much longer"))
          (macher-context--set-new-content-for-file
           (expand-file-name "file1.txt" temp-dir) modified-content context)

          (let ((result (macher--tool-list-directory context "." nil t)))
            ;; Should show modified file with new content size.
            (expect result :to-match (format "file: file1.txt (%d B)" (length modified-content)))
            ;; Still should not show untracked files.
            (expect result :not :to-match "untracked1.txt")))

        ;; Clean up untracked files.
        (delete-file untracked-file1)
        (delete-file untracked-file2)
        (delete-file subdir-untracked)))

    (it "does not list directories that don't contain workspace files"
      ;; Create directories on disk that don't contain any workspace files.  Note this not only
      ;; tests that empty directories won't appear, but also that things ike node_modules/ and .git/
      ;; won't appear.
      (let ((empty-untracked-dir (expand-file-name "untracked-empty" temp-dir))
            (untracked-dir-with-files (expand-file-name "untracked-with-files" temp-dir)))
        (make-directory empty-untracked-dir)
        (make-directory untracked-dir-with-files)
        (write-region
         "untracked content" nil (expand-file-name "untracked.txt" untracked-dir-with-files))

        ;; Mock workspace files to exclude these directories entirely.
        (spy-on 'macher--workspace-files
                :and-call-fake
                (lambda (workspace)
                  ;; Get the actual files but filter out anything in untracked directories.
                  (let ((files (macher--project-files (cdr workspace))))
                    (cl-remove-if
                     (lambda (file)
                       (or (string-match-p "untracked-empty" file)
                           (string-match-p "untracked-with-files" file)))
                     files))))

        ;; Test that untracked directories don't appear in listing.
        (let ((result (macher--tool-list-directory context ".")))
          ;; Should contain workspace directories.
          (expect result :to-match "dir: subdir")
          ;; Should NOT contain directories that have no workspace files.
          (expect result :not :to-match "untracked-empty")
          (expect result :not :to-match "untracked-with-files"))

        ;; Clean up.
        (delete-directory empty-untracked-dir t)
        (delete-directory untracked-dir-with-files t)))

    (it "correctly handles directories with similar name prefixes"
      ;; Create directories "foo" and "foobar" to test that listing "foo" doesn't match "foobar".
      (let ((foo-dir (expand-file-name "foo" temp-dir))
            (foobar-dir (expand-file-name "foobar" temp-dir)))
        (make-directory foo-dir)
        (make-directory foobar-dir)
        (write-region "content in foo" nil (expand-file-name "file-in-foo.txt" foo-dir))
        (write-region "content in foobar" nil (expand-file-name "file-in-foobar.txt" foobar-dir))

        ;; List contents of "foo" directory.
        (let ((result (macher--tool-list-directory context "foo")))
          ;; Should contain file in foo directory.
          (expect result :to-match "file-in-foo\\.txt")
          ;; Should NOT contain file from foobar directory.
          (expect result :not :to-match "file-in-foobar\\.txt"))

        ;; List contents of root - both directories should appear.
        (let ((result (macher--tool-list-directory context ".")))
          ;; Both directories should appear. Use word boundaries (\\b) to ensure exact matches.
          ;; This prevents "foo" from matching "foobar" as a substring.
          (expect result :to-match "\\bfoo\\b")
          (expect result :to-match "\\bfoobar\\b"))

        ;; Clean up.
        (delete-directory foo-dir t)
        (delete-directory foobar-dir t)))

    (it "does not allow listing directories outside the workspace root"
      ;; Try to list parent directory (should fail).
      (let ((parent-dir (file-name-directory (directory-file-name temp-dir))))
        (expect (macher--tool-list-directory context "..") :to-throw))

      ;; Try to list absolute paths outside workspace (should fail).
      (expect (macher--tool-list-directory context "/etc") :to-throw)
      (expect (macher--tool-list-directory context "/tmp") :to-throw)

      ;; Try to list using relative paths that escape workspace (should fail).
      (expect (macher--tool-list-directory context "../..") :to-throw)
      (expect (macher--tool-list-directory context "../../etc") :to-throw)

      ;; Try to list using paths with .. components that escape (should fail).
      (expect (macher--tool-list-directory context "subdir/../../..") :to-throw)

      ;; Verify that valid paths within workspace still work.
      (let ((result (macher--tool-list-directory context ".")))
        (expect result :to-be-truthy))

      (let ((result (macher--tool-list-directory context "subdir")))
        (expect result :to-be-truthy)))

    (it "does not allow listing external directories even if they contain workspace files"
      ;; Create an external directory with a file.
      (let ((external-dir (make-temp-file "macher-external-test" t))
            (external-file-name "external-workspace-file.txt"))

        (unwind-protect
            (progn
              ;; Create a file in the external directory
              (let ((external-file (expand-file-name external-file-name external-dir)))
                (write-region "external file content" nil external-file)

                ;; Mock workspace-files to include the external file in the workspace files list.
                ;; This simulates a scenario where the workspace configuration incorrectly includes
                ;; files outside the workspace root.
                (spy-on 'macher--workspace-files
                        :and-call-fake
                        (lambda (workspace)
                          ;; Get the actual project files and add our external file
                          (let ((normal-files (macher--project-files (cdr workspace))))
                            (append normal-files (list external-file)))))

                ;; Even though the external file is in the workspace-files list, we should not be
                ;; able to list the external directory.
                (expect (macher--tool-list-directory context external-dir) :to-throw)

                ;; Also test with relative paths that would escape to the external dir.
                (let ((relative-to-external (file-relative-name external-dir temp-dir)))
                  (expect (macher--tool-list-directory context relative-to-external) :to-throw))

                ;; Verify that normal workspace operations still work.
                (let ((result (macher--tool-list-directory context ".")))
                  (expect result :to-be-truthy))))

          ;; Clean up the external directory
          (when (file-exists-p external-dir)
            (delete-directory external-dir t))))))

  (describe "macher--search-get-xref-matches"
    :var (context temp-dir)

    (describe "basic behavior"
      (before-each
        (setq temp-dir (make-temp-file "macher-test-search-dir" t))
        (setq context
              (macher--make-context :workspace (cons 'project (file-name-as-directory temp-dir))))
        ;; Create test directory structure.
        (make-directory (expand-file-name "subdir" temp-dir))
        (write-region
         "hello world\nhello universe\ngoodbye world" nil (expand-file-name "file1.txt" temp-dir))
        (write-region
         "hello javascript\nfunction test() {}\nconst x = 1;"
         nil
         (expand-file-name "file2.js" temp-dir))
        (write-region
         "hello python\ndef test():\n    pass" nil (expand-file-name "subdir/file3.py" temp-dir))
        (write-region
         "no matches here\nnothing to find" nil (expand-file-name "file4.txt" temp-dir))
        ;; Add the project marker.
        (write-region "" nil (expand-file-name ".project" temp-dir)))

      (after-each
        (when (and temp-dir (file-exists-p temp-dir))
          (delete-directory temp-dir t)))

      (it "returns matches alist with correct structure"
        (let ((result (macher--search-get-xref-matches context "hello")))
          (expect (listp result) :to-be-truthy)
          ;; Should not have entries for files without matches.
          (expect (assoc "file4.txt" result) :to-be nil)

          ;; Check that the entire structure exactly matches the expected one.
          (let ((expected-summaries-alist
                 '(("file1.txt" ("hello world" "hello universe"))
                   ("file2.js" ("hello javascript"))
                   ("subdir/file3.py" ("hello python"))))
                (actual-summaries-alist
                 (mapcar
                  (lambda (file-matches)
                    (list (car file-matches) (mapcar #'xref-item-summary (cdr file-matches))))
                  result)))
            (expect actual-summaries-alist :to-equal expected-summaries-alist))))

      (it "handles basic regexp filtering"
        (let ((result (macher--search-get-xref-matches context "hello" :file-regexp "\\.js$")))
          (expect (assoc "file2.js" result) :to-be-truthy)
          (expect (assoc "file1.txt" result) :to-be nil)))

      (it "handles path filtering with results relative to workspace root"
        (let ((result (macher--search-get-xref-matches context "hello" :path "subdir")))
          ;; When searching in "subdir", results should still be relative to workspace root (like grep).
          (expect (assoc "subdir/file3.py" result) :to-be-truthy)
          ;; Files outside the search directory should not appear.
          (expect (assoc "file1.txt" result) :to-be nil)
          (expect (assoc "file3.py" result) :to-be nil)))

      (it "handles context modifications"
        ;; Modify a file in context.
        (macher-context--set-new-content-for-file
         (expand-file-name "file1.txt" temp-dir)
         "modified hello content\nnew line with hello"
         context)
        (let ((result (macher--search-get-xref-matches context "modified")))
          (expect (assoc "file1.txt" result) :to-be-truthy)))

      (it "shows paths relative to workspace root even when path is specified"
        (let ((result (macher--search-get-xref-matches context "hello" :path "subdir")))
          ;; When searching in "subdir", results should still be relative to workspace root (like grep).
          (expect (assoc "subdir/file3.py" result) :to-be-truthy)
          ;; Should NOT show paths relative to search directory.
          (expect (assoc "file3.py" result) :to-be nil)
          ;; Files outside the search directory should not appear at all.
          (expect (assoc "file1.txt" result) :to-be nil)
          (expect (assoc "file2.js" result) :to-be nil)))

      (it "performs case-insensitive search"
        ;; Create files with mixed case content.
        (macher-context--set-new-content-for-file
         (expand-file-name "file1.txt" temp-dir) "Hello World\nthis is HELLO again" context)
        (macher-context--set-new-content-for-file
         (expand-file-name "file2.txt" temp-dir) "hello universe\nBYE" context)

        ;; Case-sensitive search should not find uppercase 'HELLO'.
        (let ((result (macher--search-get-xref-matches context "hello")))
          (expect (assoc "file1.txt" result) :to-be nil)
          (expect (length (cdr (assoc "file2.txt" result))) :to-be 1))

        ;; Case-insensitive search should find both.
        (let ((result (macher--search-get-xref-matches context "hello" :case-insensitive t)))
          (expect (assoc "file1.txt" result) :to-be-truthy)
          (expect (length (cdr (assoc "file1.txt" result))) :to-be 2)

          (expect (length (cdr (assoc "file2.txt" result))) :to-be 1))))

    (describe "regexp filtering"
      :var (context temp-dir)

      (before-each
        (setq temp-dir (make-temp-file "macher-test-regexp-dir" t))
        (setq context
              (macher--make-context :workspace (cons 'project (file-name-as-directory temp-dir))))
        ;; Create nested directory structure with various file types.
        (make-directory (expand-file-name "src" temp-dir))
        (make-directory (expand-file-name "src/components" temp-dir))
        (make-directory (expand-file-name "tests" temp-dir))
        (make-directory (expand-file-name "docs" temp-dir))

        ;; Root level files.
        (write-region "hello root js" nil (expand-file-name "app.js" temp-dir))
        (write-region "hello root py" nil (expand-file-name "main.py" temp-dir))
        (write-region "hello root txt" nil (expand-file-name "README.txt" temp-dir))

        ;; src/ level files
        (write-region "hello src js" nil (expand-file-name "src/utils.js" temp-dir))
        (write-region "hello src py" nil (expand-file-name "src/helper.py" temp-dir))

        ;; src/components/ level files.
        (write-region
         "hello component js" nil (expand-file-name "src/components/button.js" temp-dir))
        (write-region
         "hello component ts" nil (expand-file-name "src/components/modal.ts" temp-dir))

        ;; tests/ level files.
        (write-region "hello test js" nil (expand-file-name "tests/app.test.js" temp-dir))
        (write-region "hello test py" nil (expand-file-name "tests/test_main.py" temp-dir))

        ;; docs/ level files.
        (write-region "hello doc md" nil (expand-file-name "docs/guide.md" temp-dir))

        ;; Add the project marker.
        (write-region "" nil (expand-file-name ".project" temp-dir)))

      (after-each
        (when (and temp-dir (file-exists-p temp-dir))
          (delete-directory temp-dir t)))

      (it "handles simple extension regexp at root level only"
        (let ((result
               (macher--search-get-xref-matches context "hello" :file-regexp "^[^/]*\\.js$")))
          ;; Should match only root level .js files (relative to workspace root since no path
          ;; specified).
          (expect (assoc "app.js" result) :to-be-truthy)
          ;; Should NOT match .js files in subdirectories.
          (expect (assoc "src/utils.js" result) :to-be nil)
          (expect (assoc "src/components/button.js" result) :to-be nil)
          (expect (assoc "tests/app.test.js" result) :to-be nil)
          ;; Should NOT match other extensions.
          (expect (assoc "main.py" result) :to-be nil)
          (expect (assoc "README.txt" result) :to-be nil)))

      (it "handles recursive extension regexp across all directories"
        (let ((result (macher--search-get-xref-matches context "hello" :file-regexp "\\.js$")))
          ;; Should match .js files at all levels.
          (expect (assoc "app.js" result) :to-be-truthy)
          (expect (assoc "src/utils.js" result) :to-be-truthy)
          (expect (assoc "src/components/button.js" result) :to-be-truthy)
          (expect (assoc "tests/app.test.js" result) :to-be-truthy)
          ;; Should NOT match other extensions even in subdirectories.
          (expect (assoc "main.py" result) :to-be nil)
          (expect (assoc "src/helper.py" result) :to-be nil)
          (expect (assoc "src/components/modal.ts" result) :to-be nil)))

      (it "handles directory-specific regex patterns"
        (let ((result
               (macher--search-get-xref-matches context "hello" :file-regexp "^src/[^/]*\\.js$")))
          ;; Should match .js files only in src/ directory (not subdirectories).
          (expect (assoc "src/utils.js" result) :to-be-truthy)
          ;; Should NOT match root level files.
          (expect (assoc "app.js" result) :to-be nil)
          ;; Should NOT match files in src/components/.
          (expect (assoc "src/components/button.js" result) :to-be nil)
          ;; Should NOT match other extensions in src/.
          (expect (assoc "src/helper.py" result) :to-be nil)))

      (it "handles regexp patterns relative to search directory when path is specified"
        (let ((result
               (macher--search-get-xref-matches
                context
                "hello"
                :path "src"
                :file-regexp "^[^/]*\\.js$")))
          ;; When searching in "src" directory, regexp should be relative to "src". So
          ;; "^[^/]*\\.js$" should match .js files directly in src/ (results show relative to workspace root).
          (expect (assoc "src/utils.js" result) :to-be-truthy)
          ;; Should NOT match files in subdirectories of src/.
          (expect (assoc "src/components/button.js" result) :to-be nil)
          ;; Should NOT match other extensions
          (expect (assoc "src/helper.py" result) :to-be nil)
          ;; Should NOT match files from other directories.
          (expect (assoc "app.js" result) :to-be nil)))

      (it "handles deeply nested directory regexp patterns (when no path specified)"
        (let ((result
               (macher--search-get-xref-matches context "hello" :file-regexp "^src/components/")))
          ;; Should match all files in src/components/ (relative to workspace root).
          (expect (assoc "src/components/button.js" result) :to-be-truthy)
          (expect (assoc "src/components/modal.ts" result) :to-be-truthy)
          ;; Should NOT match files in other directories.
          (expect (assoc "app.js" result) :to-be nil)
          (expect (assoc "src/utils.js" result) :to-be nil)))

      (it "handles nested directory regexp patterns when searching within specific path"
        (let ((result
               (macher--search-get-xref-matches
                context
                "hello"
                :path "src"
                :file-regexp "^components/")))
          ;; When searching in "src", regexp "^components/" should match files in src/components/.
          ;; Results should be relative to workspace root.
          (expect (assoc "src/components/button.js" result) :to-be-truthy)
          (expect (assoc "src/components/modal.ts" result) :to-be-truthy)
          ;; Should NOT match files directly in src/
          (expect (assoc "src/utils.js" result) :to-be nil)
          (expect (assoc "src/helper.py" result) :to-be nil)))

      (it "handles recursive directory regexp patterns (when no path specified)"
        (let ((result
               (macher--search-get-xref-matches context "hello" :file-regexp "^src/.*\\.js$")))
          ;; Should match .js files in src/ and all its subdirectories (relative to workspace root).
          (expect (assoc "src/utils.js" result) :to-be-truthy)
          (expect (assoc "src/components/button.js" result) :to-be-truthy)
          ;; Should NOT match root level .js files.
          (expect (assoc "app.js" result) :to-be nil)
          ;; Should NOT match .js files in other top-level directories.
          (expect (assoc "tests/app.test.js" result) :to-be nil)
          ;; Should NOT match non-.js files even in src/ tree.
          (expect (assoc "src/helper.py" result) :to-be nil)
          (expect (assoc "src/components/modal.ts" result) :to-be nil)))

      (it "handles recursive regexp patterns when searching within specific path"
        (let ((result
               (macher--search-get-xref-matches
                context
                "hello"
                :path "src"
                :file-regexp ".*\\.js$")))
          ;; When searching in "src", regexp ".*\\.js$" should match all .js files in src/ tree.
          ;; Results should be relative to workspace root.
          (expect (assoc "src/utils.js" result) :to-be-truthy)
          (expect (assoc "src/components/button.js" result) :to-be-truthy)
          ;; Should NOT match non-.js files
          (expect (assoc "src/helper.py" result) :to-be nil)
          (expect (assoc "src/components/modal.ts" result) :to-be nil)
          ;; Should NOT match files from other directories.
          (expect (assoc "app.js" result) :to-be nil)
          (expect (assoc "tests/app.test.js" result) :to-be nil)))

      (it "handles multiple extension regexp patterns"
        (let ((result
               (macher--search-get-xref-matches
                context
                "hello"
                :file-regexp "^[^/]*\\.\\(js\\|py\\)$")))
          ;; Should match both .js and .py files at root level.
          (expect (assoc "app.js" result) :to-be-truthy)
          (expect (assoc "main.py" result) :to-be-truthy)
          ;; Should NOT match other extensions.
          (expect (assoc "README.txt" result) :to-be nil)
          ;; Should NOT match files in subdirectories.
          (expect (assoc "src/utils.js" result) :to-be nil)
          (expect (assoc "src/helper.py" result) :to-be nil)))

      (it "handles recursive multiple extension regexp patterns"
        (let ((result
               (macher--search-get-xref-matches context "hello" :file-regexp "\\.\\(js\\|ts\\)$")))
          ;; Should match both .js and .ts files at all levels.
          (expect (assoc "app.js" result) :to-be-truthy)
          (expect (assoc "src/utils.js" result) :to-be-truthy)
          (expect (assoc "src/components/button.js" result) :to-be-truthy)
          (expect (assoc "tests/app.test.js" result) :to-be-truthy)
          (expect (assoc "src/components/modal.ts" result) :to-be-truthy)
          ;; Should NOT match .py files.
          (expect (assoc "main.py" result) :to-be nil)
          (expect (assoc "src/helper.py" result) :to-be nil)
          (expect (assoc "tests/test_main.py" result) :to-be nil)))

      (it "handles complex path patterns"
        (let ((result
               (macher--search-get-xref-matches context "hello" :file-regexp "^.*/test[^/]*$")))
          ;; Should match files that start with 'test' anywhere in the tree.
          (expect (assoc "tests/test_main.py" result) :to-be-truthy)
          ;; Should NOT match files that just contain 'test' in the middle.
          (expect (assoc "tests/app.test.js" result) :to-be nil)
          ;; Should NOT match other files.
          (expect (assoc "app.js" result) :to-be nil)))

      (it "handles exact filename regexp patterns"
        (let ((result
               (macher--search-get-xref-matches context "hello" :file-regexp "button\\.js$")))
          ;; Should match button.js anywhere in the tree (relative to workspace root).
          (expect (assoc "src/components/button.js" result) :to-be-truthy)
          ;; Should NOT match other .js files.
          (expect (assoc "app.js" result) :to-be nil)
          (expect (assoc "src/utils.js" result) :to-be nil)))

      (it "handles exact filename regexp patterns when searching within specific path"
        (let ((result
               (macher--search-get-xref-matches
                context
                "hello"
                :path "src/components"
                :file-regexp "button\\.js$")))
          ;; Should match button.js within the search path, result relative to workspace root.
          (expect (assoc "src/components/button.js" result) :to-be-truthy)
          ;; Should NOT match modal.ts.
          (expect (assoc "src/components/modal.ts" result) :to-be nil)
          ;; Should NOT show search-path-relative results.
          (expect (assoc "button.js" result) :to-be nil)))

      (it "handles no matches with restrictive regexp"
        (let ((result (macher--search-get-xref-matches context "hello" :file-regexp "\\.xyz$")))
          ;; Should match no files since no .xyz files exist.
          (expect result :to-be nil)))

      (it "combines path and regexp filtering with results relative to workspace root"
        (let ((result
               (macher--search-get-xref-matches context "hello" :path "src" :file-regexp "\\.py$")))
          ;; Should match .py files in src/ directory, results relative to workspace root.
          (expect (assoc "src/helper.py" result) :to-be-truthy)
          ;; Should NOT match .py files in other directories (outside search path)...
          ;; ...in root, outside "src" path
          (expect (assoc "main.py" result) :to-be nil)
          ;; ...n tests/, outside "src" path.
          (expect (assoc "tests/test_main.py" result) :to-be nil)
          ;; Should NOT match other extensions in src/.
          (expect (assoc "src/utils.js" result) :to-be nil)
          ;; Should NOT show search-path-relative results.
          (expect (assoc "helper.py" result) :to-be nil)))

      (it "handles regex with path limiting to subdirectory with results relative to workspace root"
        (let ((result
               (macher--search-get-xref-matches
                context
                "hello"
                :path "src/components"
                :file-regexp ".*")))
          ;; Should match all files in src/components/, results relative to workspace root.
          (expect (assoc "src/components/button.js" result) :to-be-truthy)
          (expect (assoc "src/components/modal.ts" result) :to-be-truthy)
          ;; Should NOT match files outside that specific directory.
          (expect (assoc "src/utils.js" result) :to-be nil)
          (expect (assoc "app.js" result) :to-be nil)
          ;; Should NOT show search-path-relative results.
          (expect (assoc "button.js" result) :to-be nil)
          (expect (assoc "modal.ts" result) :to-be nil))))

    ;; Advanced xref function tests - workspace boundary enforcement and complex scenarios.
    (describe "boundary enforcement"
      :var (context temp-dir extra-file)

      (before-each
        (setq temp-dir (make-temp-file "macher-test-boundary-dir" t))
        (setq context
              (macher--make-context :workspace (cons 'project (file-name-as-directory temp-dir))))
        ;; Create project structure.
        (write-region "hello world" nil (expand-file-name "included.txt" temp-dir))
        (write-region "hello universe" nil (expand-file-name "also-included.js" temp-dir))
        ;; Create an extra file that won't be in workspace-files list.
        (setq extra-file (expand-file-name "excluded.txt" temp-dir))
        (write-region "hello excluded" nil extra-file)
        ;; Create project marker.
        (write-region "" nil (expand-file-name ".project" temp-dir)))

      (after-each
        (when (and temp-dir (file-exists-p temp-dir))
          (delete-directory temp-dir t)))

      (it "excludes files not in workspace-files list"
        ;; Mock workspace-files to exclude extra-file.
        (spy-on 'macher--workspace-files
                :and-call-fake
                (lambda (workspace)
                  (let ((files (macher--project-files (cdr workspace))))
                    (cl-remove-if (lambda (file) (string-match-p "excluded\\.txt$" file)) files))))

        (let ((result (macher--search-get-xref-matches context "hello")))
          ;; Should find files that are in workspace-files.
          (expect (assoc "included.txt" result) :to-be-truthy)
          (expect (assoc "also-included.js" result) :to-be-truthy)
          ;; Should NOT find files excluded from workspace-files, even if they exist and match.
          (expect (assoc "excluded.txt" result) :to-be nil)))

      (it "includes files from macher-context even if not in workspace-files"
        ;; Mock workspace-files to exclude extra-file.
        (spy-on 'macher--workspace-files
                :and-call-fake
                (lambda (workspace)
                  (let ((files (macher--project-files (cdr workspace))))
                    (cl-remove-if (lambda (file) (string-match-p "excluded\\.txt$" file)) files))))

        ;; Add content to context for the excluded file.
        (macher-context--set-new-content-for-file extra-file "hello from context" context)

        (let ((result (macher--search-get-xref-matches context "hello")))
          ;; Should find regular workspace files.
          (expect (assoc "included.txt" result) :to-be-truthy)
          ;; Should find files from context even if not in workspace-files.
          (expect (assoc "excluded.txt" result) :to-be-truthy)
          ;; Verify it searches the context content, not the file content.
          (let ((matches (cdr (assoc "excluded.txt" result))))
            ;; Extract summary from the first xref-match-item
            (expect (xref-item-summary (car matches)) :to-equal "hello from context")))))

    ;; Tests for projects under ~ using macher-context.
    (describe "projects under tilde"
      :var (context)

      (before-each
        ;; Create a mock workspace under ~ without creating actual files.
        (let ((mock-workspace-root "~/test-project/"))
          (setq context (macher--make-context :workspace (cons 'project mock-workspace-root)))

          ;; Mock workspace-files to return fake file list.
          (spy-on 'macher--workspace-files
                  :and-return-value
                  '("~/test-project/src/main.el"
                    "~/test-project/tests/test-main.el"
                    "~/test-project/README.md"))
          ;; Mock workspace-root.
          (spy-on 'macher--workspace-root :and-return-value mock-workspace-root)))

      (it "handles tilde paths with context-only files"
        ;; Add files to context (no actual filesystem files created).
        (macher-context--set-new-content-for-file
         "~/test-project/src/main.el"
         "function hello() {\n  console.log('hello world');\n}"
         context)
        (macher-context--set-new-content-for-file
         "~/test-project/tests/test-main.el"
         "describe('hello function', () => {\n  it('should say hello', () => {\n    hello();\n  });\n});"
         context)
        (macher-context--set-new-content-for-file
         "~/test-project/README.md"
         "# Test Project\n\nThis is a test project with hello functionality."
         context)

        (let ((result (macher--search-get-xref-matches context "hello")))
          ;; Should find matches in all files under ~.
          (expect (assoc "src/main.el" result) :to-be-truthy)
          (expect (assoc "tests/test-main.el" result) :to-be-truthy)
          (expect (assoc "README.md" result) :to-be-truthy)

          ;; Verify content is from context - check first line which should contain "function
          ;; hello".
          (let ((main-matches (cdr (assoc "src/main.el" result))))
            ;; Extract summary from the first xref-match-item.
            (expect (xref-item-summary (car main-matches)) :to-match "hello"))))

      (it "applies regexp filtering with tilde paths"
        ;; Add same files to context.
        (macher-context--set-new-content-for-file
         "~/test-project/src/main.el" "hello world" context)
        (macher-context--set-new-content-for-file
         "~/test-project/tests/test-main.el" "hello test" context)
        (macher-context--set-new-content-for-file "~/test-project/README.md" "hello readme" context)

        ;; Test filtering to only .el files.
        (let ((result (macher--search-get-xref-matches context "hello" :file-regexp "\\.el$")))
          (expect (assoc "src/main.el" result) :to-be-truthy)
          (expect (assoc "tests/test-main.el" result) :to-be-truthy)
          (expect (assoc "README.md" result) :to-be nil)))

      (it "handles regexp patterns starting with ^ for tilde paths"
        ;; Add files to context with specific directory structure.
        (macher-context--set-new-content-for-file
         "~/test-project/src/main.js" "function main() { console.log('main function'); }" context)
        (macher-context--set-new-content-for-file
         "~/test-project/src/other.js"
         "function other() { console.log('other function'); }"
         context)
        (macher-context--set-new-content-for-file
         "~/test-project/lib/main.js" "function lib() { console.log('lib function'); }" context)
        (macher-context--set-new-content-for-file
         "~/test-project/tests/main.test.js"
         "test('main', () => { expect(main()).toBeTruthy(); });"
         context)

        ;; Mock workspace-files to include these files.
        (spy-on 'macher--workspace-files
                :and-return-value
                '("~/test-project/src/main.js"
                  "~/test-project/src/other.js"
                  "~/test-project/lib/main.js"
                  "~/test-project/tests/main.test.js"))

        ;; Test pattern that should only match files starting with "src/" (relative to workspace root).
        (let ((result (macher--search-get-xref-matches context "main" :file-regexp "^src/")))
          ;; Should match only the file in src/ directory (relative to workspace root).
          (expect (assoc "src/main.js" result) :to-be-truthy)
          ;; Should NOT match files in other directories.
          (expect (assoc "lib/main.js" result) :to-be nil)
          (expect (assoc "src/other.js" result) :to-be nil)
          (expect (assoc "tests/main.test.js" result) :to-be nil)))

      (it "handles regexp patterns starting with ^ when searching within specific path"
        ;; Add files to context
        (macher-context--set-new-content-for-file
         "~/test-project/src/main.js" "function main() { console.log('main function'); }" context)
        (macher-context--set-new-content-for-file
         "~/test-project/src/components/button.js" "component main button" context)
        (macher-context--set-new-content-for-file
         "~/test-project/src/utils/main.js" "utility main functions" context)

        ;; Mock workspace-files.
        (spy-on 'macher--workspace-files
                :and-return-value
                '("~/test-project/src/main.js"
                  "~/test-project/src/components/button.js"
                  "~/test-project/src/utils/main.js"))

        ;; Test pattern when searching within "src" directory.
        (let ((result
               (macher--search-get-xref-matches
                context
                "main"
                :path "src"
                :file-regexp "^[^/]*\\.js$")))
          ;; Should match only files directly in src/ (not in subdirectories), relative to workspace root.
          (expect (assoc "src/main.js" result) :to-be-truthy)
          ;; Should NOT match files in subdirectories.
          (expect (assoc "src/components/button.js" result) :to-be nil)
          (expect (assoc "src/utils/main.js" result) :to-be nil)
          ;; Should NOT show search-path-relative results.
          (expect (assoc "main.js" result) :to-be nil)))))

  (describe "buffer management"
    :var (context temp-dir)

    (before-each
      (setq temp-dir (make-temp-file "macher-test-buffer-mgmt" t))
      (setq context
            (macher--make-context :workspace (cons 'project (file-name-as-directory temp-dir))))
      ;; Create test files that are not currently loaded in buffers.
      (write-region "hello world\ntest content" nil (expand-file-name "test1.txt" temp-dir))
      (write-region "hello universe\nmore content" nil (expand-file-name "test2.txt" temp-dir))
      ;; Add the project marker.
      (write-region "" nil (expand-file-name ".project" temp-dir)))

    (after-each
      (when (and temp-dir (file-exists-p temp-dir))
        (delete-directory temp-dir t)))

    (it "does not create any new buffers when searching"
      (let ((buffer-names-before (mapcar #'buffer-name (buffer-list))))

        ;; Perform search that should find matches in files not loaded as buffers.
        (let ((result (macher--search-get-xref-matches context "hello")))
          ;; Verify we found matches.
          (expect (length result) :to-be 2)
          (expect (assoc "test1.txt" result) :to-be-truthy)
          (expect (assoc "test2.txt" result) :to-be-truthy)

          (let ((buffer-names-after (mapcar #'buffer-name (buffer-list))))
            (expect buffer-names-after :to-equal buffer-names-before)))))

    (it "loads matched files into macher-context"
      ;; Create test files that are NOT already in the context.
      ;; These files contain "hello" and should be matched and loaded.
      (write-region "hello world\ntest content" nil (expand-file-name "context-test1.txt" temp-dir))
      (write-region
       "hello universe\nmore content" nil (expand-file-name "context-test2.txt" temp-dir))
      ;; Create files that do NOT match the search pattern and should NOT be loaded.
      (write-region
       "goodbye world\nno matches here" nil (expand-file-name "no-match1.txt" temp-dir))
      (write-region
       "farewell universe\nnothing to find" nil (expand-file-name "no-match2.txt" temp-dir))
      ;; Add the project marker.
      (write-region "" nil (expand-file-name ".project" temp-dir))

      ;; Verify files are NOT already loaded in context.
      (let ((context-files-before (mapcar #'car (macher-context-contents context))))
        ;; Matching files should not be in context yet.
        (expect (member (expand-file-name "context-test1.txt" temp-dir) context-files-before)
                :to-be nil)
        (expect (member (expand-file-name "context-test2.txt" temp-dir) context-files-before)
                :to-be nil)
        ;; Non-matching files should not be in context yet.
        (expect (member (expand-file-name "no-match1.txt" temp-dir) context-files-before)
                :to-be nil)
        (expect (member (expand-file-name "no-match2.txt" temp-dir) context-files-before)
                :to-be nil)

        ;; Perform search that should find matches and load files into context.
        (let ((result (macher--search-get-xref-matches context "hello")))
          ;; Verify we found matches only in files that contain "hello".
          (expect (assoc "context-test1.txt" result) :to-be-truthy)
          (expect (assoc "context-test2.txt" result) :to-be-truthy)
          ;; Verify no matches were found in files that don't contain "hello".
          (expect (assoc "no-match1.txt" result) :to-be nil)
          (expect (assoc "no-match2.txt" result) :to-be nil)

          ;; Verify matched files are now loaded in context, but non-matching files are NOT.
          (let ((context-files-after (mapcar #'car (macher-context-contents context))))
            ;; Files with matches should now be loaded in context.
            (expect (member (expand-file-name "context-test1.txt" temp-dir) context-files-after)
                    :to-be-truthy)
            (expect (member (expand-file-name "context-test2.txt" temp-dir) context-files-after)
                    :to-be-truthy)
            ;; Files without matches should NOT be loaded in context.
            (expect (member (expand-file-name "no-match1.txt" temp-dir) context-files-after)
                    :to-be nil)
            (expect (member (expand-file-name "no-match2.txt" temp-dir) context-files-after)
                    :to-be nil)

            ;; Verify content was loaded correctly (without creating buffers).
            (let* ((file1-path (expand-file-name "context-test1.txt" temp-dir))
                   (file1-contents (macher-context--contents-for-file file1-path context))
                   (file2-path (expand-file-name "context-test2.txt" temp-dir))
                   (file2-contents (macher-context--contents-for-file file2-path context)))
              ;; Check that content matches what we wrote to disk.
              (expect (cdr file1-contents) :to-equal "hello world\ntest content")
              (expect (cdr file2-contents) :to-equal "hello universe\nmore content")))))))


  (describe "macher--search-format-files-mode"
    :var (context temp-dir)

    (before-each
      (setq temp-dir (make-temp-file "macher-test-files-mode" t))
      (setq context
            (macher--make-context :workspace (cons 'project (file-name-as-directory temp-dir))))
      (write-region "hello world\nhello universe" nil (expand-file-name "file1.txt" temp-dir))
      (write-region "hello javascript" nil (expand-file-name "file2.js" temp-dir))
      ;; Add the project marker.
      (write-region "" nil (expand-file-name ".project" temp-dir)))

    (after-each
      (when (and temp-dir (file-exists-p temp-dir))
        (delete-directory temp-dir t)))

    (it "formats files mode output correctly"
      (let* ((matches-alist (macher--search-get-xref-matches context "hello"))
             (result (macher--search-format-files-mode matches-alist)))
        (expect result
                :to-equal
                (concat
                 "file1.txt (2 matches)\n"
                 "file2.js (1 match)\n"
                 "\n"
                 "Total: 3 matches in 2 files"))))

    (it "handles empty matches alist"
      (let ((result (macher--search-format-files-mode '())))
        (expect result :to-equal "")))

    (it "uses correct singular/plural forms"
      (let* ((matches-alist (macher--search-get-xref-matches context "javascript"))
             (result (macher--search-format-files-mode matches-alist)))
        (expect result :to-match "1 match")
        (expect result :to-match "1 file"))))

  (describe "macher--search-format-content-mode"
    :var*
    (context
     temp-dir original-max-columns
     ;; Local helper function to get matches without calling the full
     ;; `macher--search-get-xref-matches'.
     (get-matches-alist
      (lambda (pattern)
        (require 'xref)
        (let* ((files (project-files (project-current nil temp-dir)))
               (all-matches (xref-matches-in-files pattern files))
               (matches-by-file (make-hash-table :test 'equal)))
          ;; Group matches by relative file path.
          (dolist (match all-matches)
            (let* ((location (xref-item-location match))
                   (file (xref-file-location-file location))
                   (rel-path (file-relative-name file temp-dir)))
              (push match (gethash rel-path matches-by-file))))
          ;; Convert hash table to alist and reverse match lists to maintain order.
          (let ((result-alist nil))
            (maphash
             (lambda (rel-path matches)
               (push (cons rel-path (reverse matches)) result-alist))
             matches-by-file)

            (reverse result-alist))))))

    (before-each
      (setq temp-dir (make-temp-file "macher-test-content-dir" t))
      (setq context
            (macher--make-context :workspace (cons 'project (file-name-as-directory temp-dir))))
      (setq original-max-columns macher-match-max-columns)
      (write-region
       "line1\nhello world\nhello universe\nline4" nil (expand-file-name "test.txt" temp-dir))
      ;; Add the project marker.
      (write-region "" nil (expand-file-name ".project" temp-dir)))

    (after-each
      (setq macher-match-max-columns original-max-columns)
      (when (and temp-dir (file-exists-p temp-dir))
        (delete-directory temp-dir t)))

    ;; Sanity-check test for the helper function.
    (it "local helper returns matches in expected format (sanity check)"
      ;; Create additional test files for proper grouping verification.
      (write-region
       "function hello() {\n  return 'hello from js';\n}\n// end hello"
       nil
       (expand-file-name "test.js" temp-dir))
      (write-region
       "# Hello markdown\nThis is a hello test file." nil (expand-file-name "test.md" temp-dir))
      ;; Sanity check test for the helper function to verify it returns matches in expected format
      ;; and properly groups everything by file.
      (let* ((matches-alist (funcall get-matches-alist "hello")))
        ;; Verify it returns an alist.
        (expect (listp matches-alist) :to-be-truthy)
        ;; Verify we have matches from multiple files (proper grouping).
        (expect (length matches-alist) :to-be 3)
        ;; Check that the entire structure exactly matches the expected one.
        (let ((expected-summaries-alist
               '(("test.js" ("function hello() {" "  return 'hello from js';" "// end hello"))
                 ("test.md"
                  ("# Hello markdown" "This is a hello test file."))
                 ("test.txt" ("hello world" "hello universe"))))
              (actual-summaries-alist
               (mapcar
                (lambda (file-matches)
                  (list (car file-matches) (mapcar #'xref-item-summary (cdr file-matches))))
                matches-alist)))
          (expect actual-summaries-alist :to-equal expected-summaries-alist))))

    (it "formats content mode output without line numbers"
      (let* ((matches-alist (funcall get-matches-alist "hello"))
             (result (macher--search-format-content-mode context matches-alist nil nil nil)))
        ;; Verify exact output structure: filename:matched_line for each match, separated by newlines.
        (expect result :to-equal (concat "test.txt:hello world\n" "test.txt:hello universe\n"))))

    (it "formats content mode output with line numbers"
      (let* ((matches-alist (funcall get-matches-alist "hello"))
             (result (macher--search-format-content-mode context matches-alist nil nil t)))
        ;; Verify exact output with line numbers: filename:line_number:matched_line.
        (expect result
                :to-equal (concat "test.txt:2:hello world\n" "test.txt:3:hello universe\n"))))

    (it "includes context lines when specified"
      (let* ((matches-alist (funcall get-matches-alist "hello world"))
             (result (macher--search-format-content-mode context matches-alist 1 1 nil)))
        ;; Verify exact output with context: before lines use '-', match lines use ':', after lines use '-'.
        (expect
         result
         :to-equal (concat "test.txt-line1\n" "test.txt:hello world\n" "test.txt-hello universe\n"))))

    (it "merges overlapping context ranges"
      (let* ((matches-alist (funcall get-matches-alist "hello"))
             (result (macher--search-format-content-mode context matches-alist 1 1 nil)))
        ;; Verify exact merged output: overlapping context should be merged into one continuous block.
        ;; Line 1 (before first match), Line 2 (first match), Line 3 (second match), Line 4 (after second match).
        ;; No separators (--) should appear since the context ranges overlap and merge.
        (expect result
                :to-equal
                (concat
                 "test.txt-line1\n"
                 "test.txt:hello world\n"
                 "test.txt:hello universe\n"
                 "test.txt-line4\n"))))

    (it "separates non-overlapping context ranges"
      ;; Create a file with widely separated matches to test separator behavior.
      (write-region
       "line1\nhello first\nline3\nline4\nline5\nline6\nhello second\nline8"
       nil
       (expand-file-name "separate.txt" temp-dir))

      (let* ((matches-alist (funcall get-matches-alist "hello"))
             ;; Get just the matches from separate.txt.
             (separate-matches (assoc "separate.txt" matches-alist))
             (result (macher--search-format-content-mode context (list separate-matches) 1 1 nil)))
        ;; Verify output has separator (--) between non-overlapping context ranges.
        ;; Range 1: line1, hello first, line3
        ;; Range 2: line6, hello second, line8
        (expect result
                :to-equal
                (concat
                 "separate.txt-line1\n"
                 "separate.txt:hello first\n"
                 "separate.txt-line3\n"
                 "--\n"
                 "separate.txt-line6\n"
                 "separate.txt:hello second\n"
                 "separate.txt-line8\n"))))

    (it "handles large files and complex match patterns"
      ;; Create a large file with multiple overlapping and non-overlapping match ranges, including multi-match lines.
      ;; Include lines longer than macher-match-max-columns (300 characters) for both context lines and match lines.
      (let* ((long-context-line (make-string (+ macher-match-max-columns 50) ?-))
             (long-match-line
              (concat
               "console.log('hello " (make-string (+ macher-match-max-columns 20) ?x) " world');"))
             (large-content
              (concat
               "large.js line1\n"
               long-context-line
               "\n"
               "function test() {\n"
               "  console.log('hello world and hello galaxy and hello universe');\n"
               "  "
               long-match-line
               "\n"
               "  console.log('hello there');\n"
               "}\n"
               "line8\n"
               "line9\n"
               "line10\n"
               "line11\n"
               "line12\n"
               "function other() {\n"
               "  console.log('hello again and hello once more');\n"
               "line15\n"
               "  console.log('hello final');\n"
               "}\n"
               "line18\n"
               "line19\n"
               "line20\n"
               "line21\n"
               "line22\n"
               "function last() {\n"
               "  console.log('hello end');\n"
               "}\n"
               "line26\n")))
        (write-region large-content nil (expand-file-name "large.js" temp-dir))

        ;; Retrieve the full list of matches in the project (not just large.js).
        (let* ((matches-alist (funcall get-matches-alist "hello"))
               ;; Result with no before/after context lines.
               (content-no-context
                (macher--search-format-content-mode context matches-alist nil nil t))
               ;; Result with 2 lines before and after matches.
               (content-with-context
                (macher--search-format-content-mode context matches-alist 2 2 t))
               (expected-content-no-context
                (concat
                 "large.js:4:  console.log('hello world and hello galaxy and hello universe');\n"
                 "large.js:5:[Omitted long line with 1 matches]\n"
                 "large.js:6:  console.log('hello there');\n"
                 "large.js:14:  console.log('hello again and hello once more');\n"
                 "large.js:16:  console.log('hello final');\n"
                 "large.js:24:  console.log('hello end');\n"
                 "test.txt:2:hello world\n"
                 "test.txt:3:hello universe\n"))
               (expected-content-with-context
                (concat
                 "large.js-2-[Omitted long context line]\n"
                 "large.js-3-function test() {\n"
                 "large.js:4:  console.log('hello world and hello galaxy and hello universe');\n"
                 "large.js:5:[Omitted long line with 1 matches]\n"
                 "large.js:6:  console.log('hello there');\n"
                 "large.js-7-}\n"
                 "large.js-8-line8\n"
                 "--\n"
                 "large.js-12-line12\n"
                 "large.js-13-function other() {\n"
                 "large.js:14:  console.log('hello again and hello once more');\n"
                 "large.js-15-line15\n"
                 "large.js:16:  console.log('hello final');\n"
                 "large.js-17-}\n"
                 "large.js-18-line18\n"
                 "--\n"
                 "large.js-22-line22\n"
                 "large.js-23-function last() {\n"
                 "large.js:24:  console.log('hello end');\n"
                 "large.js-25-}\n"
                 "large.js-26-line26\n"
                 "test.txt-1-line1\n"
                 "test.txt:2:hello world\n"
                 "test.txt:3:hello universe\n"
                 "test.txt-4-line4\n")))
          ;; Verify the results matches the expected content exactly.
          (expect content-no-context :to-equal expected-content-no-context)
          (expect content-with-context :to-equal expected-content-with-context))))

    (it "renders lines with multiple matches only once in output without context"
      ;; Test that lines with multiple matches are only rendered once.
      (write-region
       "normal line\nhello world and hello universe on same line\nanother line"
       nil
       (expand-file-name "multiple.txt" temp-dir))

      (let* ((matches-alist (funcall get-matches-alist "hello"))
             ;; Get just the matches from multiple.txt.
             (multiple-matches (assoc "multiple.txt" matches-alist))
             (result
              (macher--search-format-content-mode context (list multiple-matches) nil nil nil)))
        ;; Verify that the line with multiple matches appears exactly once.
        (expect "multiple.txt:hello world and hello universe on same line"
                :to-appear-once-in result)
        ;; Verify the result doesn't contain duplicate entries.
        (expect result :to-equal "multiple.txt:hello world and hello universe on same line\n")))

    (it "renders lines with multiple matches only once in output with context"
      ;; Test that lines with multiple matches are only rendered once even with context lines.
      (write-region
       "before line\nhello world and hello universe and hello again\nafter line"
       nil
       (expand-file-name "multi-context.txt" temp-dir))

      (let* ((matches-alist (funcall get-matches-alist "hello"))
             ;; Get just the matches from multi-context.txt.
             (multi-matches (assoc "multi-context.txt" matches-alist))
             (result (macher--search-format-content-mode context (list multi-matches) 1 1 nil)))
        ;; Verify that the line with multiple matches appears exactly once as a match line.
        (expect "multi-context.txt:hello world and hello universe and hello again"
                :to-appear-once-in result)
        ;; Verify context lines are included.
        (expect result :to-match "before line")
        (expect result :to-match "after line")
        ;; Full expected output.
        (expect result
                :to-equal
                (concat
                 "multi-context.txt-before line\n"
                 "multi-context.txt:hello world and hello universe and hello again\n"
                 "multi-context.txt-after line\n"))))

    (it "renders long lines with multiple matches only once with ripgrep-style placeholder"
      ;; Test that long lines with multiple matches are only rendered once and show correct count.
      (let* ((long-segment (make-string (+ macher-match-max-columns 50) ?x))
             (content
              (concat "hello start " long-segment " hello middle " long-segment " hello end")))
        (write-region content nil (expand-file-name "long-multi.txt" temp-dir))

        (let* ((matches-alist (funcall get-matches-alist "hello"))
               ;; Get just the matches from long-multi.txt.
               (long-matches (assoc "long-multi.txt" matches-alist))
               (result
                (macher--search-format-content-mode context (list long-matches) nil nil nil)))
          ;; Verify the omitted line placeholder appears exactly once.
          (expect "\\[Omitted long line with 3 matches\\]" :to-appear-once-in result)
          ;; Verify the long content is not present.
          (expect result :not :to-match (regexp-quote (substring long-segment 0 50)))
          ;; Full expected output.
          (expect result :to-equal "long-multi.txt:[Omitted long line with 3 matches]\n"))))

    (it "renders long lines with multiple matches only once with context and ripgrep-style placeholder"
      ;; Test that long lines with multiple matches in context mode are only rendered once.
      (let* ((long-segment (make-string (+ macher-match-max-columns 30) ?y))
             (content
              (concat
               "context before\nhello prefix "
               long-segment
               " hello suffix "
               long-segment
               " hello final\ncontext after")))
        (write-region content nil (expand-file-name "long-context-multi.txt" temp-dir))

        (let* ((matches-alist (funcall get-matches-alist "hello"))
               ;; Get just the matches from long-context-multi.txt.
               (long-context-matches (assoc "long-context-multi.txt" matches-alist))
               (result
                (macher--search-format-content-mode context (list long-context-matches) 1 1 nil)))
          ;; Verify the omitted line placeholder appears exactly once.
          (expect "\\[Omitted long line with 3 matches\\]" :to-appear-once-in result)
          ;; Verify context lines are included.
          (expect result :to-match "context before")
          (expect result :to-match "context after")
          ;; Verify the long content is not present.
          (expect result :not :to-match (regexp-quote (substring long-segment 0 30)))
          ;; Full expected output.
          (expect result
                  :to-equal
                  (concat
                   "long-context-multi.txt-context before\n"
                   "long-context-multi.txt:[Omitted long line with 3 matches]\n"
                   "long-context-multi.txt-context after\n")))))

    (it "replaces long lines with ripgrep-style placeholder in content mode (context lines)"
      ;; Test that long lines are replaced with ripgrep-style "[Omitted long line with N matches]" in context mode.
      (let* ((long-line (make-string (+ macher-match-max-columns 100) ?x))
             (content (concat "short line\n" "hello " long-line "\nshort line again")))
        (write-region content nil (expand-file-name "longlines.txt" temp-dir))

        (let* ((matches-alist (funcall get-matches-alist "hello"))
               ;; Get just the matches from longlines.txt.
               (longlines-matches (assoc "longlines.txt" matches-alist))
               (result
                (macher--search-format-content-mode context (list longlines-matches) 1 1 nil)))
          ;; Verify that the long line is replaced with the ripgrep-style placeholder.
          (expect result :to-match "\\[Omitted long line with 1 matches\\]")
          ;; Verify that short lines are preserved.
          (expect result :to-match "short line")
          (expect result :to-match "short line again")
          ;; Verify the long line content is not present.
          (expect result :not :to-match (regexp-quote (substring long-line 0 100))))))

    (it "replaces long lines with ripgrep-style placeholder in content mode (simple mode)"
      ;; Test that long summaries are replaced with ripgrep-style "[Omitted long line with N matches]" in simple content mode.
      (let* ((long-line (make-string (+ macher-match-max-columns 50) ?y))
             (content (concat "match " long-line)))
        (write-region content nil (expand-file-name "longmatch.txt" temp-dir))

        (let* ((matches-alist (funcall get-matches-alist "match"))
               ;; Get just the matches from longmatch.txt.
               (longmatch-matches (assoc "longmatch.txt" matches-alist))
               ;; Use simple content mode (no context lines).
               (result
                (macher--search-format-content-mode context (list longmatch-matches) nil nil nil)))
          ;; Verify that the long line is replaced with the ripgrep-style placeholder.
          (expect result :to-match "\\[Omitted long line with 1 matches\\]")
          ;; Verify the long line content is not present.
          (expect result :not :to-match (regexp-quote (substring long-line 0 100))))))

    (it "preserves lines under the length limit"
      ;; Test that lines just under the limit are preserved intact.
      (let* ((prefix "test")
             (just-under-limit (make-string (- macher-match-max-columns (+ 1 (length prefix))) ?z))
             (content (concat prefix just-under-limit)))
        (write-region content nil (expand-file-name "justunder.txt" temp-dir))

        (let* ((matches-alist (funcall get-matches-alist prefix))
               ;; Get just the matches from justunder.txt.
               (justunder-matches (assoc "justunder.txt" matches-alist))
               (result
                (macher--search-format-content-mode context (list justunder-matches) nil nil nil)))
          (expect (length matches-alist) :to-be 1)
          ;; Verify that the line is preserved and not replaced with any omission placeholder.
          (expect result :not :to-match "\\[Omitted long")
          (expect result :to-match (regexp-quote just-under-limit)))))

    (it "renders long lines when max-columns is nil (no before/after context)"
      (let* (
             ;; Much longer than the typical limit.
             (very-long-line (make-string 2000 ?x))
             (content (concat "match " very-long-line " end")))
        ;; Set max-columns to nil to disable the limit.
        (setq macher-match-max-columns nil)
        (write-region content nil (expand-file-name "verylong.txt" temp-dir))

        (let* ((matches-alist (funcall get-matches-alist "match"))
               ;; Get just the matches from verylong.txt.
               (verylong-matches (assoc "verylong.txt" matches-alist))
               (result
                (macher--search-format-content-mode context (list verylong-matches) nil nil nil)))
          ;; Verify that the very long line is NOT replaced with any omission placeholder.
          (expect result :not :to-match "Omitted")
          ;; Verify that the full long line content is present in the result.
          (expect result :to-match (regexp-quote very-long-line)))))

    (it "renders long lines when max-columns is nil (with before/after context)"
      (let* ((very-long-match-line (make-string 1500 ?m))
             (very-long-context-line (make-string 1800 ?c))
             (content
              (concat
               "short before\n"
               very-long-context-line
               "\n"
               "match "
               very-long-match-line
               " end\n"
               "short after")))
        ;; Set max-columns to nil to disable the limit.
        (setq macher-match-max-columns nil)
        (write-region content nil (expand-file-name "longcontext.txt" temp-dir))

        (let* ((matches-alist (funcall get-matches-alist "match"))
               ;; Get just the matches from longcontext.txt.
               (longcontext-matches (assoc "longcontext.txt" matches-alist))
               (result
                (macher--search-format-content-mode
                 context (list longcontext-matches) 1 1 nil))) ; 1 line before/after
          ;; Verify that neither match line nor context lines are truncated.
          (expect result :not :to-match "Omitted")
          ;; Verify that both very long lines are present in full.
          (expect result :to-match (regexp-quote very-long-match-line))
          (expect result :to-match (regexp-quote very-long-context-line)))))

    (it "uses ripgrep-style placeholder for context lines"
      ;; Test that long context lines (non-matching) get "[Omitted long context line]".
      (let* ((long-context-line (make-string (+ macher-match-max-columns 50) ?c))
             (content (concat long-context-line "\nhello world\nshort line")))
        (write-region content nil (expand-file-name "contextlong.txt" temp-dir))

        (let* ((matches-alist (funcall get-matches-alist "hello"))
               ;; Get just the matches from contextlong.txt.
               (contextlong-matches (assoc "contextlong.txt" matches-alist))
               ;; Use context mode to include the long context line.
               (result
                (macher--search-format-content-mode context (list contextlong-matches) 1 1 nil)))
          ;; Verify that the long context line is replaced with the ripgrep-style context placeholder.
          (expect result :to-match "\\[Omitted long context line\\]")
          ;; Verify that the matching line and short lines are preserved.
          (expect result :to-match "hello world")
          (expect result :to-match "short line")
          ;; Verify the long context line content is not present.
          (expect result :not :to-match (regexp-quote (substring long-context-line 0 100))))))

    (it "counts multiple matches on same line for ripgrep-style placeholder"
      ;; Test that lines with multiple matches show correct count in "[Omitted long line with N matches]".
      (let* ((long-line (make-string (+ macher-match-max-columns 50) ?x))
             ;; Create a line with "match" appearing twice.
             (content (concat "match start " long-line " match end")))
        (write-region content nil (expand-file-name "multimatches.txt" temp-dir))

        (let* ((matches-alist (funcall get-matches-alist "match"))
               ;; Get just the matches from multimatches.txt.
               (multimatches-matches (assoc "multimatches.txt" matches-alist))
               (result
                (macher--search-format-content-mode
                 context (list multimatches-matches) nil nil nil)))
          ;; Verify that the long line with multiple matches shows the correct count.
          (expect result :to-match "\\[Omitted long line with 2 matches\\]")
          ;; Verify the long line content is not present.
          (expect result :not :to-match (regexp-quote (substring long-line 0 100))))))

    (it "handles multiple matches on same line in context mode"
      ;; Test that context mode correctly counts multiple matches on the same line.
      (let* ((long-line (make-string (+ macher-match-max-columns 50) ?x))
             ;; Create a line with "test" appearing three times, with context.
             (content
              (concat
               "before line\ntest start "
               long-line
               " test middle "
               long-line
               " test end\nafter line")))
        (write-region content nil (expand-file-name "contextmulti.txt" temp-dir))

        (let* ((matches-alist (funcall get-matches-alist "test"))
               ;; Get just the matches from contextmulti.txt.
               (contextmulti-matches (assoc "contextmulti.txt" matches-alist))
               ;; Use context mode to include surrounding lines.
               (result
                (macher--search-format-content-mode context (list contextmulti-matches) 1 1 nil)))
          ;; Verify that the long line with multiple matches shows the correct count.
          (expect result :to-match "\\[Omitted long line with 3 matches\\]")
          ;; Verify context lines are preserved.
          (expect result :to-match "before line")
          (expect result :to-match "after line")
          ;; Verify the long line content is not present.
          (expect result :not :to-match (regexp-quote (substring long-line 0 100)))))))

  (describe "macher--tool-search-helper"
    :var (context temp-dir)

    (before-each
      (setq temp-dir (make-temp-file "macher-test-search-dir" t))
      (setq context
            (macher--make-context :workspace (cons 'project (file-name-as-directory temp-dir))))
      ;; Create test directory structure.
      (make-directory (expand-file-name "subdir" temp-dir))
      (write-region
       "hello world\nhello universe\ngoodbye world" nil (expand-file-name "file1.txt" temp-dir))
      (write-region
       "hello javascript\nfunction test() {}\nconst x = 1;"
       nil
       (expand-file-name "file2.js" temp-dir))
      (write-region
       "hello python\ndef test():\n    pass" nil (expand-file-name "subdir/file3.py" temp-dir))
      (write-region "no matches here\nnothing to find" nil (expand-file-name "file4.txt" temp-dir))
      ;; Add the project marker.
      (write-region "" nil (expand-file-name ".project" temp-dir)))

    (after-each
      (when (and temp-dir (file-exists-p temp-dir))
        (delete-directory temp-dir t)))

    (it "handles basic search functionality"
      (let ((result (macher--tool-search-helper context "hello")))
        ;; Verify exact files mode output structure with correct counts and totals.
        (expect result
                :to-equal
                (concat
                 "file1.txt (2 matches)\n"
                 "file2.js (1 match)\n"
                 "subdir/file3.py (1 match)\n"
                 "\n"
                 "Total: 4 matches in 3 files"))))

    (it "handles content mode"
      (let ((result (macher--tool-search-helper context "hello" :mode "content")))
        ;; Verify exact content mode output with all matching lines from all files.
        (expect result
                :to-equal
                (concat
                 "file1.txt:hello world\n"
                 "file1.txt:hello universe\n"
                 "file2.js:hello javascript\n"
                 "subdir/file3.py:hello python\n"))))

    (it "applies head-limit correctly"
      (let ((result (macher--tool-search-helper context "hello" :head-limit 2)))
        ;; Verify exact head-limited output: only first 2 lines from files mode.
        (expect result :to-equal (concat "file1.txt (2 matches)\n" "file2.js (1 match)"))))

    (it "handles content mode with context lines"
      (let ((result
             (macher--tool-search-helper
              context
              "hello world"
              :mode "content"
              :lines-before 1
              :lines-after 1)))
        ;; Verify exact content output with context lines for specific pattern match. "hello world"
        ;; is on line 1, so no line before, "hello universe" is line after.
        (expect result :to-equal (concat "file1.txt:hello world\n" "file1.txt-hello universe\n"))))

    (it "handles specific pattern matching"
      (let ((result (macher--tool-search-helper context "javascript")))
        ;; Verify exact output for pattern that matches only one file.
        (expect result :to-equal (concat "file2.js (1 match)\n" "\n" "Total: 1 match in 1 file"))))

    (it "validates parameters correctly"
      (expect (macher--tool-search-helper context "hello" :mode "invalid") :to-throw)
      (expect (macher--tool-search-helper context "") :to-throw))

    (it "shows exact path when searching specific file"
      ;; When path points to a specific file rather than directory, results should show exact path.
      (let ((result (macher--tool-search-helper context "hello" :path "file1.txt")))
        (expect result :to-match "file1.txt (2 matches)")
        (expect result :not :to-match "subdir/")
        (expect result :not :to-match "file2.js")))

    (describe "error handling"
      :var
      (context workspace temp-file)
      (before-each
        (funcall setup-project)
        ;; Create workspace and context.
        (setq workspace `(project . ,project-dir))
        (setq context (macher--make-context :workspace workspace))
        ;; Create a test file with content that includes parentheses.
        (setq temp-file (expand-file-name "test-search.el" project-dir))
        (with-temp-buffer
          (insert "(describe \"search_in_workspace\"\n")
          (insert "  (it \"test case\"\n")
          (insert "    (expect true :to-be-truthy)))\n")
          (write-region (point-min) (point-max) temp-file)))

      (it "handles invalid regex patterns in search"
        ;; Test pattern with unmatched opening parenthesis - this should trigger regex parse error.
        (expect (macher--tool-search context "\\(describe \"search_in_workspace\"")
                :to-throw 'user-error)))

    (describe "comprehensive tests"
      :var (context temp-dir)

      (before-each
        (setq temp-dir (make-temp-file "macher-test-helper-dir" t))
        (setq context
              (macher--make-context :workspace (cons 'project (file-name-as-directory temp-dir))))
        ;; Create comprehensive test structure.
        (make-directory (expand-file-name "src" temp-dir))
        (make-directory (expand-file-name "tests" temp-dir))
        (make-directory (expand-file-name "docs" temp-dir))

        ;; Create files with varied content.
        (write-region
         "function processUser(user) {\n  console.log('Processing user:', user.name);\n  return user;\n}"
         nil
         (expand-file-name "src/user.js" temp-dir))
        (write-region
         "def process_user(user):\n    print(f'Processing user: {user.name}')\n    return user"
         nil
         (expand-file-name "src/user.py" temp-dir))
        (write-region
         "describe('processUser', () => {\n  it('should process user correctly', () => {});\n});"
         nil
         (expand-file-name "tests/user.test.js" temp-dir))
        (write-region
         "# User Processing\n\nThis module processes user data efficiently."
         nil
         (expand-file-name "docs/user.md" temp-dir))

        ;; Add project marker.
        (write-region "" nil (expand-file-name ".project" temp-dir)))

      (after-each
        (when (and temp-dir (file-exists-p temp-dir))
          (delete-directory temp-dir t)))

      (it "handles complex regexp patterns with context and path filters"
        ;; Add additional content to context.
        (macher-context--set-new-content-for-file
         (expand-file-name "src/new-feature.js" temp-dir)
         "// New feature for processing user requests\nfunction processUserRequest() {}"
         context)

        ;; Search for "process" in src directory, JS files only.
        (let ((result
               (macher--tool-search-helper context "process" :path "src" :file-regexp "\\.js$")))
          ;; Should find matches in JS files in src/, results relative to workspace root (like
          ;; grep).
          (expect result :to-match "src/user.js")
          (expect result :to-match "src/new-feature.js")
          ;; Should NOT find matches in Python files or other directories.
          (expect result :not :to-match "user.py")
          (expect result :not :to-match "tests/")
          (expect result :not :to-match "docs/")
          ;; Should NOT show paths relative to the search root.
          (expect result :not :to-match "^user.js")
          (expect result :not :to-match "^new-feature.js")))

      (it "handles complex patterns in context files"
        ;; Add content to context
        (macher-context--set-new-content-for-file
         (expand-file-name "src/config.js" temp-dir)
         "const config = {\n  user: {\n    processing: true\n  }\n};"
         context)

        ;; Search for pattern that spans multiple words
        (let ((result (macher--tool-search-helper context "processing")))
          (expect result :to-match "src/config.js")))

      (it "respects head-limit with mixed content sources"
        ;; Add several context files
        (dotimes (i 5)
          (macher-context--set-new-content-for-file
           (expand-file-name (format "context-file-%d.txt" i) temp-dir)
           "This file contains the word target for searching"
           context))

        ;; Search with head-limit
        (let ((result (macher--tool-search-helper context "target" :head-limit 3)))
          ;; Count lines in result - should be 3 or fewer
          (let ((lines (split-string result "\n" t)))
            (expect (<= (length lines) 3) :to-be-truthy))))

      (it "combines multiple filters effectively"
        ;; Add specific test content.
        (macher-context--set-new-content-for-file
         (expand-file-name "tests/integration.test.js" temp-dir)
         "describe('Integration tests', () => {\n expect(true).toBe(true);\n});"
         context)

        (let ((result
               (macher--tool-search-helper
                context
                "test"
                :path "tests"
                :file-regexp "\\.js$"
                :show-line-numbers t)))
          ;; Should match files in tests directory that contain "test" in content, results relative
          ;; to workspace root (like grep).
          (expect result :to-match "tests/integration.test.js")
          (expect result :to-match "test")
          ;; Should NOT match files that don't contain the pattern in content.
          (expect result :not :to-match "tests/user.test.js")
          ;; Should NOT show relative paths.
          (expect result :not :to-match "^integration.test.js")
          (expect result :not :to-match "^user.test.js")))

      (it "handles projects under tilde with context files"
        ;; Create mock tilde workspace
        (let ((tilde-context (macher--make-context :workspace '(project . "~/my-project/"))))
          ;; Mock workspace functions
          (spy-on 'macher--workspace-files
                  :and-return-value '("~/my-project/main.py" "~/my-project/test.py"))
          (spy-on 'macher--workspace-root :and-return-value "~/my-project/")

          ;; Add context files
          (macher-context--set-new-content-for-file
           "~/my-project/main.py"
           "def main():\n    print('Hello from main')\n    return 0"
           tilde-context)
          (macher-context--set-new-content-for-file
           "~/my-project/test.py"
           "import unittest\nclass TestMain(unittest.TestCase):\n    def test_main(self): pass"
           tilde-context)

          (let ((result (macher--tool-search-helper tilde-context "main" :file-regexp "\\.py$")))
            ;; Should find matches in tilde-based project (relative to workspace root).
            (expect result :to-match "main.py")
            (expect result :to-match "test.py"))))

      (it "handles case-insensitive search"
        ;; Add files with mixed case content.
        (macher-context--set-new-content-for-file
         (expand-file-name "mixed-case.txt" temp-dir)
         "Hello World\nthis is HELLO again\nGoodbye"
         context)
        (macher-context--set-new-content-for-file
         (expand-file-name "lower-case.txt" temp-dir) "hello universe\nbye" context)

        ;; Case-sensitive search (default) should only find lowercase 'hello'
        (let ((result (macher--tool-search-helper context "hello")))
          (expect result :to-match "lower-case.txt")
          (expect result :not :to-match "mixed-case.txt"))

        ;; Case-insensitive search should find both
        (let ((result (macher--tool-search-helper context "hello" :case-insensitive t)))
          (expect result :to-match "lower-case.txt")
          (expect result :to-match "mixed-case.txt")))

      (it "shows exact path when searching specific file rather than directory"
        ;; Add content to a specific file.
        (macher-context--set-new-content-for-file
         (expand-file-name "src/user.js" temp-dir)
         "function processUser(user) {\n  console.log('Processing user:', user.name);\n  return user;\n}"
         context)

        ;; When searching a specific file, results should show the exact path provided.
        (let ((result (macher--tool-search-helper context "process" :path "src/user.js")))
          ;; Should show the exact file path as provided, not relative to anything.
          (expect result :to-match "src/user.js")
          ;; Should NOT show just the filename.
          (expect result :not :to-match "^user.js"))))

    (describe "float input handling"
      (it "handles float inputs for lines-after parameter"
        ;; Float should be rounded to nearest integer. Must use content mode for context lines.
        (let ((result-2.3
               (macher--tool-search-helper context "hello" :mode "content" :lines-after 2.3))
              (result-2.7
               (macher--tool-search-helper context "hello" :mode "content" :lines-after 2.7)))
          ;; 2.3 should round to 2 lines after.
          (expect result-2.3 :to-match "hello world")
          (expect result-2.3 :to-match "hello universe")
          ;; 2.7 should round to 3 lines after.
          (expect result-2.7 :to-match "hello world")
          (expect result-2.7 :to-match "hello universe")
          (expect result-2.7 :to-match "goodbye world")))

      (it "handles float inputs for lines-before parameter"
        ;; Test lines-before with float inputs. Must use content mode for context lines.
        (let ((result-1.4
               (macher--tool-search-helper context "universe" :mode "content" :lines-before 1.4))
              (result-1.6
               (macher--tool-search-helper context "universe" :mode "content" :lines-before 1.6)))
          ;; 1.4 should round to 1 line before.
          (expect result-1.4 :to-match "hello world")
          (expect result-1.4 :to-match "hello universe")
          ;; 1.6 should round to 2 lines before - but there's only 1 line before "universe".
          (expect result-1.6 :to-match "hello world")
          (expect result-1.6 :to-match "hello universe")))

      (it "handles float inputs for head-limit parameter"
        ;; head-limit should limit the number of matches shown.
        (let ((result-1.3 (macher--tool-search-helper context "hello" :head-limit 1.3))
              (result-2.7 (macher--tool-search-helper context "hello" :head-limit 2.7)))
          ;; 1.3 should round to 1, showing only the first match.
          (expect (length (split-string result-1.3 "\n" t)) :to-be-less-than 5)
          ;; 2.7 should round to 3, showing up to 3 matches.
          (expect (length (split-string result-2.7 "\n" t)) :to-be-less-than 8)))

      (it "handles edge case float inputs"
        ;; Test with 0.x values and negative floats. Must use content mode for context lines.
        (let ((result-0.4
               (macher--tool-search-helper context "hello" :mode "content" :lines-after 0.4))
              (result-0.6
               (macher--tool-search-helper context "hello" :mode "content" :lines-after 0.6)))
          ;; 0.4 should round to 0 (no extra lines).
          (expect result-0.4 :to-match "hello")
          ;; 0.6 should round to 1 (1 extra line).
          (expect result-0.6 :to-match "hello")))))

  (describe "macher--tool-read-file"
    :var (context temp-dir)

    (before-each
      (setq temp-dir (make-temp-file "macher-test-dir" t))
      (setq context (macher--make-context :workspace (cons 'project temp-dir)))
      ;; Create test files.
      (write-region "test file content" nil (expand-file-name "test-file.txt" temp-dir))

      ;; Add the project marker.
      (write-region "" nil (expand-file-name ".project" temp-dir)))

    (after-each
      (when (and temp-dir (file-exists-p temp-dir))
        (delete-directory temp-dir t)))

    (it "reads regular file content"
      (let ((result (macher--tool-read-file context "test-file.txt")))
        (expect result :to-equal "test file content")))

    (it "returns symlink target for symlinks instead of following them"
      ;; Create a symlink to the test file.
      (let ((symlink-path (expand-file-name "test-symlink" temp-dir))
            (target-path (expand-file-name "test-file.txt" temp-dir)))
        (make-symbolic-link target-path symlink-path)

        ;; Mock workspace files to include the symlink
        (spy-on 'macher--workspace-files
                :and-call-fake
                (lambda (workspace)
                  (append (macher--project-files (cdr workspace)) (list symlink-path))))

        (let ((result (macher--tool-read-file context "test-symlink")))
          (expect result :to-match "Symlink target:")
          (expect result :to-match target-path))

        ;; Clean up.
        (delete-file symlink-path))

      ;; Create a broken symlink.
      (let ((broken-symlink-path (expand-file-name "broken-symlink" temp-dir)))
        (make-symbolic-link "/nonexistent/target" broken-symlink-path)

        ;; Mock workspace files to include the broken symlink
        (spy-on 'macher--workspace-files
                :and-call-fake
                (lambda (workspace)
                  (append (macher--project-files (cdr workspace)) (list broken-symlink-path))))

        (let ((result (macher--tool-read-file context "broken-symlink")))
          (expect result :to-match "Symlink target:")
          (expect result :to-match "/nonexistent/target"))

        ;; Clean up.
        (delete-file broken-symlink-path))

      ;; Create a relative symlink.
      (let ((rel-symlink-path (expand-file-name "rel-symlink" temp-dir)))
        (make-symbolic-link "./test-file.txt" rel-symlink-path)

        ;; Mock workspace files to include the relative symlink
        (spy-on 'macher--workspace-files
                :and-call-fake
                (lambda (workspace)
                  (append (macher--project-files (cdr workspace)) (list rel-symlink-path))))

        (let ((result (macher--tool-read-file context "rel-symlink")))
          (expect result :to-match "Symlink target:")
          (expect result :to-match "./test-file.txt"))

        ;; Clean up.
        (delete-file rel-symlink-path)))

    (it "signals error for non-existent files"
      (expect (macher--tool-read-file context "nonexistent.txt") :to-throw))

    (describe "float parameter handling"
      (it "handles float offset values by rounding them"
        ;; Create test file for this test
        (write-region "line1\nline2\nline3\nline4" nil (expand-file-name "multiline.txt" temp-dir))
        ;; 1.0 should round to 1.
        (expect (macher--tool-read-file context "multiline.txt" 1.0)
                :to-equal "line1\nline2\nline3\nline4")
        ;; 2.3 should round to 2.
        (expect (macher--tool-read-file context "multiline.txt" 2.3)
                :to-equal "line2\nline3\nline4")
        ;; 2.7 should round to 3.
        (expect (macher--tool-read-file context "multiline.txt" 2.7) :to-equal "line3\nline4")
        ;; 40.0 should round to 40 (beyond bounds).
        (expect (macher--tool-read-file context "multiline.txt" 40.0) :to-equal ""))

      (it "handles float limit values by rounding them"
        ;; Create test file for this test
        (write-region "line1\nline2\nline3\nline4" nil (expand-file-name "multiline.txt" temp-dir))
        ;; 1.0 should round to 1.
        (expect (macher--tool-read-file context "multiline.txt" nil 1.0) :to-equal "line1")
        ;; 2.3 should round to 2.
        (expect (macher--tool-read-file context "multiline.txt" nil 2.3) :to-equal "line1\nline2")
        ;; 2.7 should round to 3.
        (expect (macher--tool-read-file context "multiline.txt" nil 2.7)
                :to-equal "line1\nline2\nline3")
        ;; 40.0 should round to 40 (beyond bounds, return all).
        (expect (macher--tool-read-file context "multiline.txt" nil 40.0)
                :to-equal "line1\nline2\nline3\nline4"))

      (it "handles negative float values by rounding them"
        ;; Create test file for this test
        (write-region "line1\nline2\nline3\nline4" nil (expand-file-name "multiline.txt" temp-dir))
        ;; -1.0 should round to -1.
        (expect (macher--tool-read-file context "multiline.txt" -1.0) :to-equal "line4")
        ;; -2.3 should round to -2.
        (expect (macher--tool-read-file context "multiline.txt" -2.3) :to-equal "line3\nline4")
        ;; -2.7 should round to -3.
        (expect (macher--tool-read-file context "multiline.txt" -2.7)
                :to-equal "line2\nline3\nline4")
        ;; -1.0 limit should round to -1 (4 - 1 = 3 lines).
        (expect (macher--tool-read-file context "multiline.txt" nil -1.0)
                :to-equal "line1\nline2\nline3")
        ;; -2.3 limit should round to -2 (4 - 2 = 2 lines).
        (expect (macher--tool-read-file context "multiline.txt" nil -2.3) :to-equal "line1\nline2")
        ;; -2.7 limit should round to -3 (4 - 3 = 1 line).
        (expect (macher--tool-read-file context "multiline.txt" nil -2.7) :to-equal "line1"))

      (it "handles both float offset and limit together"
        ;; Create a 5-line file for this test
        (write-region
         "line1\nline2\nline3\nline4\nline5" nil (expand-file-name "fiveline.txt" temp-dir))
        ;; 2.4 should round to 2, 2.6 should round to 3.
        (expect (macher--tool-read-file context "fiveline.txt" 2.4 2.6)
                :to-equal "line2\nline3\nline4")
        ;; 1.5 should round to 2, 1.4 should round to 1.
        (expect (macher--tool-read-file context "fiveline.txt" 1.5 1.4) :to-equal "line2")
        ;; Mix positive and negative floats: -2.3 should round to -2, 2.7 should round to 3.
        (expect (macher--tool-read-file context "fiveline.txt" -2.3 2.7) :to-equal "line4\nline5"))

      (it "handles float values with show-line-numbers parameter"
        ;; Create test file for this test
        (write-region "line1\nline2\nline3\nline4" nil (expand-file-name "multiline.txt" temp-dir))
        ;; 1.8 should round to 2, 2.4 should round to 2.
        (expect (macher--tool-read-file context "multiline.txt" 1.8 2.4 t)
                :to-equal "2\tline2\n3\tline3")
        ;; -1.5 should round to -2 (line3 start), 1.9 should round to 2 (2 lines).
        (expect (macher--tool-read-file context "multiline.txt" -1.5 1.9 t)
                :to-equal "3\tline3\n4\tline4"))

      (it "handles edge case float values"
        ;; Create a 3-line file for this test
        (write-region "line1\nline2\nline3" nil (expand-file-name "threeline.txt" temp-dir))
        ;; 0.4 should round to 0, which gets treated as 1.
        (expect (macher--tool-read-file context "threeline.txt" 0.4)
                :to-equal "line1\nline2\nline3")
        ;; 0.6 should round to 1.
        (expect (macher--tool-read-file context "threeline.txt" 0.6)
                :to-equal "line1\nline2\nline3")
        ;; Very small positive float should still round to 0 then treated as 1.
        (expect (macher--tool-read-file context "threeline.txt" 0.1)
                :to-equal "line1\nline2\nline3")
        ;; Very small negative float should round to limit 0.
        (expect (macher--tool-read-file context "threeline.txt" nil -0.1) :to-equal ""))))

  (describe "macher--project-files"
    :var (temp-dir file1 file2 subdir file3 project-file)

    (it "signals error with nil project root"
      (expect (macher--project-files nil) :to-throw))

    (it "returns project files with relative paths"
      (funcall setup-project)
      (expect project-dir :to-be-truthy)
      (let ((files (macher--project-files (directory-file-name project-dir))))
        (expect files :to-be-truthy)
        (expect (listp files) :to-be-truthy)
        ;; Should contain relative paths.
        (expect files :to-contain "file1.txt")
        (expect files :to-contain "file2.el")
        (expect files :to-contain "subdir/file3.md"))))

  (describe "macher--context-string"
    (describe "main functionality"
      :var (temp-dir file1 file2 subdir file3 project-file)

      (before-each
        (setq temp-dir (make-temp-file "macher-test-workspace" t))
        (setq file1 (expand-file-name "file1.txt" temp-dir))
        (setq file2 (expand-file-name "file2.el" temp-dir))
        (setq subdir (expand-file-name "subdir" temp-dir))
        (setq file3 (expand-file-name "file3.md" subdir))
        (setq project-file (expand-file-name ".project" temp-dir))
        ;; Create test files.
        (make-directory subdir)
        (write-region "test content" nil file1)
        (write-region ";;; Elisp file content" nil file2)
        (write-region "# Markdown content" nil file3)
        ;; Write to the project file to make it a real project.
        (write-region "" nil project-file))

      (after-each
        (delete-directory temp-dir t))

      (it "generates context string for project workspace"
        (let* ((macher--workspace (cons 'project temp-dir))
               (contexts `((,file1) (,file2)))
               (result (macher--context-string contexts)))
          (expect (stringp result) :to-be-truthy)
          ;; Should contain workspace information.
          (expect result :to-match "WORKSPACE CONTEXT")
          ;; Should contain our test files with full relative paths.
          (expect result :to-match "file1.txt")
          (expect result :to-match "file2.el")
          (expect result :to-match "subdir/file3.md")
          ;; Should distinguish between files in context and files available for editing.
          ;; Files in context should be in "already provided" section with proper structure.
          (expect result
                  :to-match "Files already provided above.*\n\\(    [^\n]*\n\\)*    file1\\.txt")
          (expect result
                  :to-match "Files already provided above.*\n\\(    [^\n]*\n\\)*    file2\\.el")
          ;; Files not in context should be in "available for editing" section.
          (expect
           result
           :to-match "Other files available for editing:\n\\(    [^\n]*\n\\)*    subdir/file3\\.md")
          ;; Should contain workspace description.
          (expect result :to-match "In-memory editing environment")))

      (it "generates context string for single-file workspace"
        (let* ((macher--workspace (cons 'file file1))
               (contexts `((,file1)))
               (result (macher--context-string contexts)))
          (expect (stringp result) :to-be-truthy)
          ;; Should contain workspace information.
          (expect result :to-match "WORKSPACE CONTEXT")
          ;; Should contain only the single file.
          (expect result :to-match "file1.txt")
          ;; Should not contain other files.
          (expect result :not :to-match "file2.el")
          (expect result :not :to-match "file3.md")))

      (it "properly categorizes files with same name in different directories"
        (let* ((workspace '(project . "/test/project/"))
               (workspace-files '("/test/project/test.txt" "/test/project/subdir/test.txt"))
               (contexts '(("/test/project/test.txt")))
               result)
          ;; Mock workspace functions.
          (spy-on 'macher--workspace-root :and-return-value "/test/project/")
          (spy-on 'macher--workspace-name :and-return-value "test-project")
          (spy-on 'macher--workspace-files :and-return-value workspace-files)

          ;; Set the test workspace.
          (with-temp-buffer
            (setq-local macher--workspace workspace)
            (setq result (macher--context-string contexts)))

          ;; Verify the result structure.
          (expect (stringp result) :to-be-truthy)
          (expect result :to-match "WORKSPACE CONTEXT")

          ;; test.txt should be in the "already provided" section since it's in contexts
          (expect result
                  :to-match "Files already provided above.*\n\\(    [^\n]*\n\\)*    test\\.txt")

          ;; subdir/test.txt should be in "available for editing" since it's not in contexts
          (expect
           result
           :to-match "Other files available for editing:\n\\(    [^\n]*\n\\)*    subdir/test\\.txt")

          ;; Should contain workspace description
          (expect result :to-match "In-memory editing environment"))))

    (describe "macher--context-string with max-files limit"
      :var (temp-dir workspace all-files original-max-files)

      (before-each
        (setq temp-dir (make-temp-file "macher-context-test" t))
        (setq workspace `(project . ,(directory-file-name temp-dir)))
        (setq original-max-files macher-context-string-max-files)

        ;; Create test files
        (let ((file1 (expand-file-name "file1.txt" temp-dir))
              (file2 (expand-file-name "file2.txt" temp-dir))
              (file3 (expand-file-name "file3.txt" temp-dir))
              (file4 (expand-file-name "file4.txt" temp-dir))
              (file5 (expand-file-name "file5.txt" temp-dir)))
          (write-region "content1" nil file1)
          (write-region "content2" nil file2)
          (write-region "content3" nil file3)
          (write-region "content4" nil file4)
          (write-region "content5" nil file5)
          (setq all-files (list file1 file2 file3 file4 file5)))

        ;; Create project marker
        (write-region "" nil (expand-file-name ".project" temp-dir))

        ;; Mock workspace functions
        (spy-on 'macher-workspace :and-return-value workspace)
        (spy-on 'macher--workspace-name :and-return-value "test-project")
        (spy-on 'macher--workspace-root :and-return-value temp-dir)
        (spy-on 'macher--workspace-files :and-return-value all-files))

      (after-each
        (setq macher-context-string-max-files original-max-files)
        (when (and temp-dir (file-exists-p temp-dir))
          (delete-directory temp-dir t)))

      (it "lists all files when max-files is nil (no limit)"
        (setq macher-context-string-max-files nil)
        (let ((result (macher--context-string '())))
          (expect result :to-match "file1.txt")
          (expect result :to-match "file2.txt")
          (expect result :to-match "file3.txt")
          (expect result :to-match "file4.txt")
          (expect result :to-match "file5.txt")
          (expect result :not :to-match "truncated")))

      (it "respects max-files limit for non-context files"
        (setq macher-context-string-max-files 3)
        (let ((result (macher--context-string '())))
          ;; Should show only first 3 files in workspace order
          (expect result :to-match "file1.txt")
          (expect result :to-match "file2.txt")
          (expect result :to-match "file3.txt")
          (expect result :not :to-match "file4.txt")
          (expect result :not :to-match "file5.txt")
          (expect result :to-match "2 more files")))

      (it "always lists gptel context files first, even if they exceed the limit"
        (setq macher-context-string-max-files 2)
        ;; Create context with 3 files (more than the limit)
        (let* ((file1 (nth 0 all-files))
               (file3 (nth 2 all-files))
               (file5 (nth 4 all-files))
               (contexts `((,file1) (,file3) (,file5)))
               (result (macher--context-string contexts)))

          ;; All 3 context files should be listed in the "already provided" section
          (expect result :to-match "Files already provided above")
          (expect result :to-match "file1.txt")
          (expect result :to-match "file3.txt")
          (expect result :to-match "file5.txt")

          ;; No additional files should be listed since context files exceed the limit
          (expect result :not :to-match "Other files available")
          (expect result :not :to-match "file2.txt")
          (expect result :not :to-match "file4.txt")))

      (it "lists remaining files up to limit after context files"
        (setq macher-context-string-max-files 4)
        ;; Create context with 2 files, leaving room for 2 more
        (let* ((file1 (nth 0 all-files))
               (file3 (nth 2 all-files))
               (contexts `((,file1) (,file3)))
               (result (macher--context-string contexts)))
          ;; Context files should be in "already provided" section
          (expect result :to-match "Files already provided above")
          (expect result :to-match "file1.txt")
          (expect result :to-match "file3.txt")

          ;; Should list 2 more files (file2.txt and file4.txt in workspace order)
          (expect result :to-match "Other files available")
          (expect result :to-match "file2.txt")
          (expect result :to-match "file4.txt")

          ;; file5.txt should be truncated
          (expect result :not :to-match "file5.txt")
          (expect result :to-match "1 more files")))

      (it "shows correct truncation count when context files are included"
        (setq macher-context-string-max-files 3)
        ;; Context has 1 file, limit allows 2 more, but 4 files remain
        (let* ((file1 (nth 0 all-files))
               (contexts `((,file1)))
               (result (macher--context-string contexts)))

          ;; Should show file1 in context, file2 and file3 in available, truncate file4 and file5
          (expect result :to-match "file1.txt")
          (expect result :to-match "file2.txt")
          (expect result :to-match "file3.txt")
          (expect result :not :to-match "file4.txt")
          (expect result :not :to-match "file5.txt")
          (expect result :to-match "2 more files")))

      (it "handles case where all files are in context"
        (setq macher-context-string-max-files 3)
        ;; All files are in context
        (let* ((contexts (mapcar #'list all-files))
               (result (macher--context-string contexts)))

          ;; All files should be listed in "already provided" section
          (expect result :to-match "Files already provided above")
          (expect result :to-match "file1.txt")
          (expect result :to-match "file2.txt")
          (expect result :to-match "file3.txt")
          (expect result :to-match "file4.txt")
          (expect result :to-match "file5.txt")

          ;; No "other files" section
          (expect result :not :to-match "Other files available")
          (expect result :not :to-match "more files")))

      (it "shows no truncation message when limit is nil"
        (setq macher-context-string-max-files nil)
        (let* ((file1 (nth 0 all-files))
               (contexts `((,file1)))
               (result (macher--context-string contexts)))

          (expect result :to-match "file1.txt")
          (expect result :to-match "file2.txt")
          (expect result :to-match "file3.txt")
          (expect result :to-match "file4.txt")
          (expect result :to-match "file5.txt")
          (expect result :not :to-match "more files")))))

  (describe "Workspace detection functions"
    (before-each
      (funcall setup-project)
      (funcall setup-file)
      ;; Sanity checks.
      (expect project-file :to-be-truthy)
      (expect project-dir :to-be-truthy)
      (expect file :to-be-truthy))

    (describe "macher--project-workspace"
      (it "returns project workspace when in a project"
        (with-temp-buffer
          (find-file project-file)
          (let ((result (macher--project-workspace)))
            (expect result :to-be-truthy)
            (expect (car result) :to-be 'project)
            (expect (file-truename (directory-file-name (cdr result)))
                    :to-equal (file-truename (directory-file-name project-dir))))))

      (it "returns nil when not in a project"
        (with-temp-buffer
          (find-file file)
          (let ((result (macher--project-workspace)))
            (expect result :to-be nil)))))

    (describe "macher--file-workspace"
      (it "returns file workspace when buffer visits a file"
        (with-temp-buffer
          (find-file file)
          (let ((result (macher--file-workspace)))
            (expect result :to-be-truthy)
            (expect (car result) :to-be 'file)
            (expect (cdr result) :to-equal file))))

      (it "returns nil when buffer doesn't visit a file"
        (with-temp-buffer
          (let ((result (macher--file-workspace)))
            (expect result :to-be nil)))))

    (describe "macher-workspace"
      (it "returns buffer-local workspace when set"
        (with-temp-buffer
          (find-file project-file)
          (setq-local macher--workspace '(custom . "/custom/path"))
          (let ((result (macher-workspace)))
            (expect result :to-be-truthy)
            (expect (car result) :to-be 'custom)
            (expect (cdr result) :to-equal "/custom/path"))))

      (it "detects project workspace via hook"
        (with-temp-buffer
          (find-file project-file)
          (let ((result (macher-workspace)))
            (expect result :to-be-truthy)
            (expect (car result) :to-be 'project)
            (expect (file-truename (directory-file-name (cdr result)))
                    :to-equal (file-truename (directory-file-name project-dir))))))

      (it "falls back to file workspace when no project found"
        (with-temp-buffer
          (find-file file)
          (let ((result (macher-workspace)))
            (expect result :to-be-truthy)
            (expect (car result) :to-be 'file)
            (expect (cdr result) :to-equal file))))

      (it "returns nil when no workspace can be determined"
        (with-temp-buffer
          ;; Don't set buffer-file-name and mock hook to return nil.
          (let ((macher-workspace-functions nil))
            (expect (macher-workspace) :to-be 'nil))))

      (it "respects custom workspace hook functions"
        (let* ((custom-workspace-fn (lambda () '(custom . "/custom/workspace")))
               (macher-workspace-functions (list custom-workspace-fn)))
          (with-temp-buffer
            (let ((result (macher-workspace)))
              (expect result :to-be-truthy)
              (expect (car result) :to-be 'custom)
              (expect (cdr result) :to-equal "/custom/workspace")))))

      (it "respects hook function order"
        (let* ((first-fn (lambda () '(first . "/first")))
               (second-fn (lambda () '(second . "/second")))
               (macher-workspace-functions (list first-fn second-fn)))
          (with-temp-buffer
            (let ((result (macher-workspace)))
              ;; Should return result from first function.
              (expect result :to-be-truthy)
              (expect (car result) :to-be 'first)
              (expect (cdr result) :to-equal "/first")))))

      (it "continues to next hook function when one returns nil"
        (let* ((nil-fn (lambda () nil))
               (success-fn (lambda () '(success . "/success")))
               (macher-workspace-functions (list nil-fn success-fn)))
          (with-temp-buffer
            (let ((result (macher-workspace)))
              ;; Should return result from second function.
              (expect result :to-be-truthy)
              (expect (car result) :to-be 'success)
              (expect (cdr result) :to-equal "/success")))))

      (it "works with explicit buffer argument"
        (with-temp-buffer
          (find-file project-file)
          (let ((test-buffer (current-buffer)))
            (with-temp-buffer
              (find-file file)
              ;; Sanity check the current buffer has a different workspace.
              (expect (car (macher-workspace)) :to-be 'file)
              (let ((result (macher-workspace test-buffer)))
                (expect result :to-be-truthy)
                (expect (car result) :to-be 'project)
                (expect (file-truename (directory-file-name (cdr result)))
                        :to-equal (file-truename (directory-file-name project-dir))))))))

      ;; Test the actual default hook behavior.
      (describe "integration tests"
        (it "detects non-project files"
          (with-temp-buffer
            (find-file file)
            (let ((result (macher-workspace)))
              (expect result :to-be-truthy)
              (expect (car result) :to-be 'file)
              (expect (cdr result) :to-equal file))))

        (it "prefers project over file"
          (with-temp-buffer
            (find-file project-file)
            (let ((result (macher-workspace)))
              (expect result :to-be-truthy)
              (expect (car result) :to-be 'project)
              (expect (file-truename (directory-file-name (cdr result)))
                      :to-equal (file-truename (directory-file-name project-dir))))))))

    (describe "macher--workspace-root"
      (before-each
        ;; Sanity checks.
        (expect project-file :to-be-truthy)
        (expect project-dir :to-be-truthy))

      (it "throws error when workspace type is not in macher-workspace-types-alist"
        (let ((macher-workspace-types-alist nil)
              (test-workspace '(nonexistent-type . "/some/path")))
          ;; Test macher--workspace-root
          (expect (macher--workspace-root test-workspace) :to-throw 'error)
          ;; Test macher--workspace-name
          (expect (macher--workspace-name test-workspace) :to-throw 'error)
          ;; Test macher--workspace-files
          (expect (macher--workspace-files test-workspace) :to-throw 'error)))

      (it "throws error when workspace type config is missing required functions"
        ;; Test missing :get-root
        (let ((macher-workspace-types-alist
               '((test-type . (:get-name (lambda (id) "test") :get-files (lambda (id) nil)))))
              (test-workspace '(test-type . "/some/path")))
          (expect (macher--workspace-root test-workspace) :to-throw 'error))

        ;; Test missing :get-name
        (let ((macher-workspace-types-alist
               '((test-type . (:get-root (lambda (id) "/some/root") :get-files (lambda (id) nil)))))
              (test-workspace '(test-type . "/some/path")))
          (expect (macher--workspace-name test-workspace) :to-throw 'error)))

      (it "throws error when root function returns nil"
        (let ((macher-workspace-types-alist
               '((test-type
                  .
                  (:get-root
                   (lambda (id) nil)
                   :get-name (lambda (id) "test")
                   :get-files (lambda (id) nil)))))
              (test-workspace '(test-type . "/some/path")))
          (expect (macher--workspace-root test-workspace) :to-throw 'error)))

      (it "throws error when root function returns non-absolute path"
        (let ((macher-workspace-types-alist
               '((test-type
                  .
                  (:get-root
                   (lambda (id) "relative/path")
                   :get-name (lambda (id) "test")
                   :get-files (lambda (id) nil)))))
              (test-workspace '(test-type . "/some/path")))
          (expect (macher--workspace-root test-workspace) :to-throw 'error)))

      (it "throws error when root function returns path to non-existent directory"
        (let ((macher-workspace-types-alist
               '((test-type
                  .
                  (:get-root
                   (lambda (id) "/nonexistent/directory")
                   :get-name (lambda (id) "test")
                   :get-files (lambda (id) nil)))))
              (test-workspace '(test-type . "/some/path")))
          (expect (macher--workspace-root test-workspace) :to-throw 'error)))

      (it "throws error when root function returns path to a file instead of directory"
        (let* ((temp-file (make-temp-file "macher-test-file"))
               (macher-workspace-types-alist
                `((test-type
                   .
                   (:get-root
                    (lambda (id) ,temp-file)
                    :get-name (lambda (id) "test")
                    :get-files (lambda (id) nil)))))
               (test-workspace '(test-type . "/some/path")))
          (unwind-protect
              (expect (macher--workspace-root test-workspace) :to-throw 'error)
            ;; Clean up
            (when (file-exists-p temp-file)
              (delete-file temp-file)))))

      (it "works correctly with valid workspace type configuration"
        (let* ((temp-dir (make-temp-file "macher-test-workspace" t))
               (macher-workspace-types-alist
                `((test-type
                   .
                   (:get-root
                    (lambda (id) ,temp-dir)
                    :get-name (lambda (id) "Test Workspace")
                    :get-files
                    (lambda (id) '("file1.txt" "file2.txt"))))))
               (test-workspace '(test-type . "/some/path")))
          (unwind-protect
              (progn
                ;; All functions should work correctly
                (expect (macher--workspace-root test-workspace) :to-equal temp-dir)
                (expect (macher--workspace-name test-workspace) :to-equal "Test Workspace")
                (let ((files (macher--workspace-files test-workspace)))
                  (expect files
                          :to-equal
                          (list
                           (expand-file-name "file1.txt" temp-dir)
                           (expand-file-name "file2.txt" temp-dir)))))
            ;; Clean up
            (when (file-exists-p temp-dir)
              (delete-directory temp-dir t)))))))
  (describe "macher--append-if-missing"
    (it "appends new items to empty list"
      (let ((result (macher--append-if-missing '() '(1 2 3))))
        (expect result :to-equal '(1 2 3))))

    (it "appends new items to non-empty list"
      (let ((result (macher--append-if-missing '(1 2) '(3 4))))
        (expect result :to-equal '(1 2 3 4))))

    (it "skips items already present in list"
      (let ((result (macher--append-if-missing '(1 2 4) '(2 3 4))))
        (expect result :to-equal '(1 2 4 3))))

    (it "returns copy when all items already present"
      (let* ((original '(1 2 3))
             (result (macher--append-if-missing original '(1 2 3))))
        (expect result :to-equal '(1 2 3))
        ;; Should be a copy, not the same list.
        (expect result :not :to-be original)))

    (it "returns copy of original list"
      (let* ((original '(1 2 3))
             (result (macher--append-if-missing original '())))
        (expect result :to-equal original)
        ;; Should be a copy, not the same list.
        (expect result :not :to-be original)))

    (it "handles empty new-items list"
      (let ((result (macher--append-if-missing '(1 2 3) '())))
        (expect result :to-equal '(1 2 3))))

    (it "uses custom test function when provided"
      (let* ((testfn (lambda (a b) (= (mod a 10) (mod b 10))))
             (result (macher--append-if-missing '(1 2 3) '(11 14) testfn)))
        ;; 11 matches 1, so shouldn't be added.
        ;; 14 matches 4 (not present), so should be added.
        (expect result :to-equal '(1 2 3 14))))

    (it "uses equal as default test function"
      (let ((result (macher--append-if-missing '("a" "b") '("b" "c"))))
        (expect result :to-equal '("a" "b" "c"))))

    (it "preserves order of original list"
      (let ((result (macher--append-if-missing '(3 1 2) '(4 5))))
        (expect result :to-equal '(3 1 2 4 5))))

    (it "appends items in order from new-items"
      (let ((result (macher--append-if-missing '(1) '(5 4 3 2))))
        (expect result :to-equal '(1 5 4 3 2))))

    (it "handles lists with nil values"
      (let ((result (macher--append-if-missing '(1 nil 2) '(nil 3))))
        ;; nil already exists in the original list, so it won't be added again.
        (expect result :to-equal '(1 nil 2 3))))

    (it "appends nil when not already present"
      (let ((result (macher--append-if-missing '(1 2) '(nil 3))))
        (expect result :to-equal '(1 2 nil 3))))

    (it "handles complex data structures"
      (let* ((obj1 '(a . 1))
             (obj2 '(b . 2))
             (obj3 '(c . 3))
             (result (macher--append-if-missing (list obj1 obj2) (list obj2 obj3))))
        (expect result :to-equal (list obj1 obj2 obj3))))

    (it "uses custom testfn for complex structures"
      (let* ((testfn (lambda (a b) (equal (car a) (car b))))
             (result (macher--append-if-missing '((a . 1) (b . 2)) '((b . 999) (c . 3)) testfn)))
        ;; (b . 999) should be skipped because car matches (b . 2).
        (expect result :to-equal '((a . 1) (b . 2) (c . 3))))))

  (describe "macher--merge-tools"
    (before-each
      (funcall setup-project))

    (it "merges tools into empty preset and creates context"
      (with-temp-buffer
        (find-file project-file)
        (let* ((preset '(:tools nil :prompt-transform-functions nil))
               (test-tools-function
                (lambda (context make-tool-function)
                  (list
                   (funcall make-tool-function
                            :name "test_tool"
                            :function (lambda () "test")
                            :description "A test tool"))))
               (result (macher--merge-tools preset test-tools-function)))
          ;; Should return an updated preset.
          (expect (plistp result) :to-be-truthy)
          ;; Should have tools.
          (let ((tools (plist-get result :tools)))
            (expect (length tools) :to-equal 1)
            (expect (gptel-tool-name (car tools)) :to-equal "test_tool")))))

    (it "merges tools with existing regular tools in preset"
      (with-temp-buffer
        (find-file project-file)
        (let* ((regular-tool
                (gptel-make-tool
                 :name "regular_tool"
                 :function (lambda () "regular")
                 :description "A regular tool"))
               (preset `(:tools (,regular-tool) :prompt-transform-functions nil))
               (test-tools-function
                (lambda (context make-tool-function)
                  (list
                   (funcall make-tool-function
                            :name "macher_tool"
                            :function (lambda () "macher")
                            :description "A macher tool"))))
               (result (macher--merge-tools preset test-tools-function)))
          ;; Should have both tools.
          (let ((tools (plist-get result :tools)))
            (expect (length tools) :to-equal 2)
            ;; Should contain the regular tool.
            (expect (cl-find-if
                     (lambda (tool) (string= (gptel-tool-name tool) "regular_tool")) tools)
                    :to-be-truthy)
            ;; Should contain the macher tool.
            (expect (cl-find-if
                     (lambda (tool) (string= (gptel-tool-name tool) "macher_tool")) tools)
                    :to-be-truthy)))))

    (it "reuses existing macher context from same workspace"
      (with-temp-buffer
        (find-file project-file)
        (let* ((existing-context (macher--make-context :workspace (macher-workspace)))
               (existing-tool
                (macher--make-tool
                 existing-context
                 :name "existing_macher_tool"
                 :function (lambda () "existing")
                 :description "An existing macher tool"))
               (preset `(:tools (,existing-tool) :prompt-transform-functions nil))
               (context-received nil)
               (test-tools-function
                (lambda (context make-tool-function)
                  (setq context-received context)
                  ;; Should reuse the existing context.
                  (expect context :to-be existing-context)
                  (list
                   (funcall make-tool-function
                            :name "new_macher_tool"
                            :function (lambda () "new")
                            :description "A new macher tool"))))
               (result (macher--merge-tools preset test-tools-function)))
          ;; Should have both tools.
          (let ((tools (plist-get result :tools)))
            (expect (length tools) :to-equal 2)
            ;; Should contain the existing tool.
            (expect (cl-find-if
                     (lambda (tool) (string= (gptel-tool-name tool) "existing_macher_tool")) tools)
                    :to-be-truthy)
            ;; Should contain the new tool.
            (expect (cl-find-if
                     (lambda (tool) (string= (gptel-tool-name tool) "new_macher_tool")) tools)
                    :to-be-truthy))
          ;; Verify the context was reused.
          (expect context-received :to-be existing-context))))

    (it "removes existing tools with same names as new ones"
      (with-temp-buffer
        (find-file project-file)
        (let* ((regular-tool
                (gptel-make-tool
                 :name "conflicting_tool"
                 :function (lambda () "regular")
                 :description "A regular tool"))
               (macher-context (macher--make-context :workspace '(project . "/other/workspace")))
               (macher-tool
                (macher--make-tool
                 macher-context
                 :name "other_tool"
                 :function (lambda () "other")
                 :description "Another tool"))
               (preset `(:tools (,regular-tool ,macher-tool) :prompt-transform-functions nil))
               (test-tools-function
                (lambda (context make-tool-function)
                  (list
                   (funcall make-tool-function
                            :name "conflicting_tool"
                            :function (lambda () "new conflicting")
                            :description "New conflicting tool")
                   (funcall make-tool-function
                            :name "unique_tool"
                            :function (lambda () "unique")
                            :description "Unique tool"))))
               (result (macher--merge-tools preset test-tools-function)))
          ;; Should have 3 tools: other_tool + 2 new tools.
          (let ((tools (plist-get result :tools)))
            (expect (length tools) :to-equal 3)
            ;; Should not contain the old conflicting tool.
            (expect (cl-find-if
                     (lambda (tool)
                       (and (string= (gptel-tool-name tool) "conflicting_tool")
                            (not (macher--tool-context tool))))
                     tools)
                    :to-be nil)
            ;; Should contain the new conflicting tool.
            (expect (cl-find-if
                     (lambda (tool)
                       (and (string= (gptel-tool-name tool) "conflicting_tool")
                            (macher--tool-context tool)))
                     tools)
                    :to-be-truthy)
            ;; Should contain the other tool.
            (expect (cl-find-if (lambda (tool) (string= (gptel-tool-name tool) "other_tool")) tools)
                    :to-be-truthy)
            ;; Should contain the unique tool.
            (expect (cl-find-if
                     (lambda (tool) (string= (gptel-tool-name tool) "unique_tool")) tools)
                    :to-be-truthy)))))

    (it "creates new context when no existing macher tools found"
      (with-temp-buffer
        (find-file project-file)
        (let* ((regular-tool
                (gptel-make-tool
                 :name "regular_tool"
                 :function (lambda () "regular")
                 :description "A regular tool"))
               (preset `(:tools (,regular-tool) :prompt-transform-functions nil))
               (context-received nil)
               (test-tools-function
                (lambda (context make-tool-function)
                  (setq context-received context)
                  ;; Should get a new context.
                  (expect (macher-context-p context) :to-be-truthy)
                  ;; Should have the current workspace.
                  (expect (macher-context-workspace context) :to-equal (macher-workspace))
                  (list
                   (funcall make-tool-function
                            :name "new_tool"
                            :function (lambda () "new")
                            :description "A new tool"))))
               (result (macher--merge-tools preset test-tools-function)))
          ;; Should have both tools.
          (let ((tools (plist-get result :tools)))
            (expect (length tools) :to-equal 2))
          ;; Verify a new context was created.
          (expect (macher-context-p context-received) :to-be-truthy)
          (expect (macher-context-workspace context-received) :to-equal (macher-workspace)))))

    (it "warns and returns unchanged preset when no workspace found"
      (with-temp-buffer
        ;; Don't set up any workspace.
        (let ((macher-workspace-functions nil)
              (preset '(:tools nil :prompt-transform-functions nil))
              (warning-triggered nil))
          ;; Mock the warn function to capture warnings.
          (spy-on 'display-warning
                  :and-call-fake
                  (lambda (type msg &optional level buffer-name)
                    (when (string-match "No macher workspace found" msg)
                      (setq warning-triggered t))))
          (let ((result (macher--merge-tools preset (lambda (context make-tool-function) nil))))
            ;; Should return the original preset unchanged.
            (expect result :to-equal preset)
            ;; Should have triggered a warning.
            (expect warning-triggered :to-be-truthy))))))

  (describe "macher--with-preset"
    :var (original-presets)

    (before-each
      (setq original-presets gptel--known-presets)
      (setq gptel--known-presets nil))

    (after-each
      (setq gptel--known-presets original-presets))

    (it "applies macher preset with callback"
      (let ((callback-called nil))
        (expect gptel--known-presets :to-be nil)
        (expect gptel-tools :to-be nil)
        (macher--with-preset
         'macher
         (lambda ()
           (expect gptel-tools :not :to-be nil)
           (setq callback-called t)))
        ;; Verify the callback was called.
        (expect callback-called :to-be-truthy)
        ;; Verify global state was not modified by the callback.
        (expect gptel-tools :to-be nil)
        (expect gptel--known-presets :to-be nil)))

    (it "applies macher-ro preset with callback"
      (let ((callback-called nil))
        (expect gptel--known-presets :to-be nil)
        (expect gptel-tools :to-be nil)
        (macher--with-preset
         'macher-ro
         (lambda ()
           (expect gptel-tools :not :to-be nil)
           (setq callback-called t)))
        ;; Verify the callback was called.
        (expect callback-called :to-be-truthy)
        ;; Verify the global state was not modified by the callback.
        (expect gptel-tools :to-be nil)
        (expect gptel--known-presets :to-be nil)))

    (it "signals error for invalid preset symbol"
      (expect (macher--with-preset 'nonexistent-preset (lambda ())) :to-throw))

    (it "accepts raw preset spec"
      (let ((callback-called nil)

            (gptel-use-tools nil))
        (macher--with-preset
         '(:use-tools t :description "Test preset")
         (lambda ()
           (expect gptel-use-tools :to-be t)
           (setq callback-called t)))
        ;; Verify the callback was called.
        (expect callback-called :to-be-truthy)
        ;; Verify global state was not modified by the callback.
        (expect gptel-use-tools :to-be nil)
        (expect gptel--known-presets :to-be nil)))

    (it "preserves existing presets"
      (let ((gptel--known-presets nil))
        (gptel-make-preset 'test :description "test")
        (expect (length gptel--known-presets) :to-be 1)
        (macher--with-preset 'macher (lambda ()))
        (expect (length gptel--known-presets) :to-be 1)))

    (it "preserves existing prompt transforms"
      (let* ((original-transforms gptel-prompt-transform-functions)
             (existing-transform
              (lambda ()
                "Test transform function"
                nil))
             (captured-transforms nil)
             ;; Set up the existing transforms list.
             (gptel-prompt-transform-functions (list existing-transform)))
        (macher--with-preset
         'macher-notools
         (lambda ()
           ;; Capture the transforms list inside the preset context.
           (setq captured-transforms gptel-prompt-transform-functions)
           ;; Verify that the existing transform is preserved.
           (expect (member existing-transform gptel-prompt-transform-functions) :to-be-truthy)
           ;; Verify that macher transform is also present.
           (expect (member #'macher--prompt-transform-add-context gptel-prompt-transform-functions)
                   :to-be-truthy)))
        ;; Verify that the global transforms list is restored after the preset.
        (expect gptel-prompt-transform-functions :to-equal (list existing-transform))
        ;; Verify that the existing transform was indeed in the captured transforms list.
        (expect (member existing-transform captured-transforms) :to-be-truthy)))

    (it "preserves existing tools"
      (let* ((original-tools gptel-tools)
             (existing-tool
              (gptel-make-tool
               :name "existing_tool"
               :function (lambda () "existing")
               :description "An existing tool"))
             (captured-tools nil)
             ;; Set up the existing tools list.
             (gptel-tools (list existing-tool)))
        (macher--with-preset
         'macher-ro
         (lambda ()
           ;; Capture the tools list inside the preset context.
           (setq captured-tools gptel-tools)
           ;; Verify that the existing tool is preserved.
           (expect (cl-find-if
                    (lambda (tool) (string= (gptel-tool-name tool) "existing_tool")) gptel-tools)
                   :to-be-truthy)
           ;; Verify that macher tools are also present.
           (expect (cl-find-if
                    (lambda (tool)
                      (string= (gptel-tool-name tool) "read_file_in_workspace"))
                    gptel-tools)
                   :to-be-truthy)))
        ;; Verify that the global tools list is restored after the preset.
        (expect gptel-tools :to-equal (list existing-tool))
        ;; Verify that the existing tool was indeed in the captured tools list.
        (expect (cl-find-if
                 (lambda (tool) (string= (gptel-tool-name tool) "existing_tool")) captured-tools)
                :to-be-truthy))))

  (describe "macher-install"
    :var (original-presets)

    (before-each
      (setq original-presets gptel--known-presets)
      (setq gptel--known-presets nil))

    (after-each
      (setq gptel--known-presets original-presets))

    (it "installs all macher presets"
      ;; Ensure gptel--known-presets is empty at the beginning.
      (expect gptel--known-presets :to-be nil)
      ;; Call macher-install.
      (macher-install)
      ;; Verify that gptel--known-presets now contains the expected presets.
      (expect gptel--known-presets :not :to-be nil)
      ;; Check that all three macher presets are installed.
      (expect (assq 'macher gptel--known-presets) :to-be-truthy)
      (expect (assq 'macher-ro gptel--known-presets) :to-be-truthy)
      (expect (assq 'macher-notools gptel--known-presets) :to-be-truthy)
      ;; Verify the descriptions of the installed presets.
      (let ((macher-preset (gptel-get-preset 'macher))
            (macher-ro-preset (gptel-get-preset 'macher-ro))
            (macher-notools-preset (gptel-get-preset 'macher-notools)))
        (expect (plist-get macher-preset :description)
                :to-equal "Send macher workspace context + tools to read files and propose edits")
        (expect (plist-get macher-ro-preset :description)
                :to-equal "Send macher workspace context + tools to read files")
        (expect (plist-get macher-notools-preset :description)
                :to-equal "Send macher workspace context without tools"))))

  (describe "macher-patch-buffer"
    (before-each
      (funcall setup-project))

    (it "runs setup hook when creating new patch buffer"
      (with-temp-buffer
        (find-file project-file)
        (let* ((setup-hook-called nil)
               (macher-patch-buffer-setup-hook (list (lambda () (setq setup-hook-called t)))))
          ;; Create a new patch buffer
          (let ((buffer (macher-patch-buffer nil t)))
            (expect buffer :to-be-truthy)
            (expect setup-hook-called :to-be t)))))

    (it "does not run setup hook when getting existing patch buffer"
      (with-temp-buffer
        (find-file project-file)
        (let* ((setup-hook-call-count 0)
               (macher-patch-buffer-setup-hook
                (list (lambda () (setq setup-hook-call-count (1+ setup-hook-call-count))))))
          ;; Create a new patch buffer
          (let ((buffer1 (macher-patch-buffer nil t)))
            (expect setup-hook-call-count :to-equal 1)
            ;; Get the same buffer again
            (let ((buffer2 (macher-patch-buffer nil nil)))
              (expect buffer2 :to-be buffer1)
              (expect setup-hook-call-count :to-equal 1))))))

    (it "sets up patch buffer with default configuration"
      (with-temp-buffer
        (find-file project-file)
        ;; Create a new patch buffer with default setup
        (let ((buffer (macher-patch-buffer nil t)))
          (expect buffer :to-be-truthy)
          (with-current-buffer buffer
            ;; Check that read-only mode is enabled
            (expect major-mode :to-equal 'diff-mode)
            ;; Check that truncate-lines is set
            (expect truncate-lines :to-be-truthy))))))

  (describe "macher--patch-buffer-setup"
    (it "sets up diff UI correctly"
      (let ((macher-patch-buffer-ui 'diff))
        (with-temp-buffer
          (setq-local macher--workspace '(test . "/tmp/test"))
          (macher--patch-buffer-setup)
          ;; Check that diff-mode is enabled
          (expect (derived-mode-p 'diff-mode) :to-be-truthy)
          ;; Check that buffer is read-only
          (expect buffer-read-only :to-be-truthy)
          ;; Check that truncate-lines is set
          (expect truncate-lines :to-be-truthy)
          ;; Check that the patch-ready hook was added buffer-locally
          (expect (member #'macher--patch-ready macher-patch-ready-hook) :to-be-truthy))))

    (it "performs no setup when UI is nil"
      (let ((macher-patch-buffer-ui nil))
        (with-temp-buffer
          (setq-local macher--workspace '(test . "/tmp/test"))
          (macher--patch-buffer-setup)
          ;; Check that major mode is still fundamental-mode
          (expect major-mode :to-be 'fundamental-mode)
          ;; Check that buffer is NOT read-only
          (expect buffer-read-only :to-be nil)
          ;; Check that no hooks were added
          (expect (member #'macher--patch-ready macher-patch-ready-hook) :to-be nil))))

    (it "raises error for unrecognized UI value"
      (let ((macher-patch-buffer-ui 'invalid-value))
        (with-temp-buffer
          (setq-local macher--workspace '(test . "/tmp/test"))
          (expect (macher--patch-buffer-setup) :to-throw 'user-error)))))

  (describe "macher-action-buffer"
    (before-each
      (funcall setup-project))

    (it "runs setup hook when creating new action buffer"
      (with-temp-buffer
        (find-file project-file)
        (let* ((setup-hook-called nil)
               (macher-action-buffer-setup-hook (list (lambda () (setq setup-hook-called t)))))
          ;; Create a new action buffer
          (let ((buffer (macher-action-buffer nil t)))
            (expect buffer :to-be-truthy)
            (expect setup-hook-called :to-be t)))))

    (it "does not run setup hook when getting existing action buffer"
      (with-temp-buffer
        (find-file project-file)
        (let* ((setup-hook-call-count 0)
               (macher-action-buffer-setup-hook
                (list (lambda () (setq setup-hook-call-count (1+ setup-hook-call-count))))))
          ;; Create a new action buffer
          (let ((buffer1 (macher-action-buffer nil t)))
            (expect setup-hook-call-count :to-equal 1)
            ;; Get the same buffer again
            (let ((buffer2 (macher-action-buffer nil nil)))
              (expect buffer2 :to-be buffer1)
              (expect setup-hook-call-count :to-equal 1)))))))

  (describe "macher--action-buffer-setup"
    (it "sets up basic UI correctly"
      (let ((macher-action-buffer-ui 'basic))
        (with-temp-buffer
          (setq-local macher--workspace '(test . "/tmp/test"))
          (macher--action-buffer-setup)
          ;; Check that the before-action hook was added.
          (expect (member #'macher--before-action macher-before-action-functions) :to-be-truthy)
          ;; Check that gptel-mode is NOT enabled (basic doesn't enable it).
          (expect (bound-and-true-p gptel-mode) :to-be nil))))

    (it "sets up default UI correctly"
      (let ((macher-action-buffer-ui 'default))
        (with-temp-buffer
          (setq-local macher--workspace '(test . "/tmp/test"))
          (macher--action-buffer-setup)
          ;; Check that gptel-mode is enabled.
          (expect (bound-and-true-p gptel-mode) :to-be-truthy)
          ;; Check that visual-line-mode is enabled.
          (expect (bound-and-true-p visual-line-mode) :to-be-truthy)
          ;; Check that the before-action hook was added.
          (expect (member #'macher--before-action macher-before-action-functions) :to-be-truthy))))

    (it "sets up org UI correctly"
      (let ((macher-action-buffer-ui 'org))
        (with-temp-buffer
          (setq-local macher--workspace '(test . "/tmp/test"))
          (macher--action-buffer-setup)
          ;; Check that org-mode is enabled.
          (expect (derived-mode-p 'org-mode) :to-be-truthy)
          ;; Check that gptel-mode is enabled.
          (expect (bound-and-true-p gptel-mode) :to-be-truthy)
          ;; Check that the before-action hook was added.
          (expect (member #'macher--before-action macher-before-action-functions) :to-be-truthy))))

    (it "performs no setup when UI is nil"
      (let ((macher-action-buffer-ui nil))
        (with-temp-buffer
          (setq-local macher--workspace '(test . "/tmp/test"))
          (macher--action-buffer-setup)
          ;; Check that no hooks were added.
          (expect (member #'macher--before-action macher-before-action-functions) :to-be nil)
          ;; Check that gptel-mode is NOT enabled.
          (expect (bound-and-true-p gptel-mode) :to-be nil)
          ;; Check that major mode is still fundamental-mode.
          (expect major-mode :to-be 'fundamental-mode))))

    (it "raises error for unrecognized UI value"
      (let ((macher-action-buffer-ui 'invalid-value))
        (with-temp-buffer
          (setq-local macher--workspace '(test . "/tmp/test"))
          (expect (macher--action-buffer-setup) :to-throw 'user-error)))))

  (describe "macher--before-action"
    (describe "prefix insertion"
      (it "inserts prefix at beginning of empty buffer"
        (with-temp-buffer
          (setq-local gptel-prompt-prefix-alist '((fundamental-mode . "### ")))
          (let ((execution
                 (macher--make-action-execution
                  :action 'test
                  :prompt "Test prompt"
                  :summary "Test prompt"
                  :buffer (current-buffer))))
            (macher--before-action execution)
            ;; The buffer should start with the prefix.
            (expect (buffer-substring-no-properties 1 5) :to-equal "### "))))

      (it "inserts newline before prefix when not at beginning of line"
        (with-temp-buffer
          (setq-local gptel-prompt-prefix-alist '((fundamental-mode . "### ")))
          ;; Put some content that doesn't end with a newline.
          (insert "previous response content")
          (let ((execution
                 (macher--make-action-execution
                  :action 'test
                  :prompt "Test prompt"
                  :summary "Test prompt"
                  :buffer (current-buffer))))
            (macher--before-action execution)
            ;; Check that a newline was inserted before the prefix.
            (let ((content (buffer-substring-no-properties (point-min) (point-max))))
              (expect content :to-match "previous response content\n### ")))))

      (it "does not insert extra newline when already at beginning of line"
        (with-temp-buffer
          (setq-local gptel-prompt-prefix-alist '((fundamental-mode . "### ")))
          ;; Put some content that ends with a newline.
          (insert "previous response content\n")
          (let ((execution
                 (macher--make-action-execution
                  :action 'test
                  :prompt "Test prompt"
                  :summary "Test prompt"
                  :buffer (current-buffer))))
            (macher--before-action execution)
            ;; Check that no extra newline was added.
            (let ((content (buffer-substring-no-properties (point-min) (point-max))))
              (expect content :to-match "previous response content\n### ")
              ;; Should not have double newlines.
              (expect content :not :to-match "previous response content\n\n### ")))))

      (it "does not insert prefix if buffer already ends with it"
        (with-temp-buffer
          (setq-local gptel-prompt-prefix-alist '((fundamental-mode . "### ")))
          ;; Buffer already ends with the prefix.
          (insert "previous content\n### ")
          (let ((execution
                 (macher--make-action-execution
                  :action 'test
                  :prompt "Test prompt"
                  :summary "Test prompt"
                  :buffer (current-buffer))))
            (macher--before-action execution)
            ;; Prefix should not be duplicated.
            (let ((content (buffer-substring-no-properties (point-min) (point-max))))
              ;; The header and prompt will be added, but not another prefix before them.
              (expect content :to-match "^previous content\n### `test` Test prompt")))))))

  (describe "macher--make-tool and macher--tool-context"
    :var (original-tools)

    (before-each
      (setq original-tools gptel--known-tools)
      (setq gptel--known-tools nil))

    (after-each
      (setq gptel--known-tools original-tools))

    (it "creates macher tool and retrieves context"
      ;; Ensure gptel--known-tools is empty at the beginning.
      (expect gptel--known-tools :to-be nil)
      ;; Create a test context and tool.
      (let* ((context (macher--make-context))
             (test-function (lambda (arg1 arg2) (list arg1 arg2)))
             (tool
              (macher--make-tool
               context
               :name "test_tool"
               :function test-function
               :description "A test tool"
               :args '((:name "arg1" :type string) (:name "arg2" :type number)))))
        ;; Verify gptel--known-tools is still empty after calling macher--make-tool.
        (expect gptel--known-tools :to-be nil)
        ;; Verify macher--tool-context returns the correct context.
        (let ((retrieved-context (macher--tool-context tool)))
          (expect retrieved-context :to-be context))
        ;; Verify the tool function can be called.
        (let* ((tool-fn (gptel-tool-function tool))
               (result (funcall tool-fn "hello" 42)))
          (expect result :to-equal '("hello" 42)))))

    (it "returns nil for regular tools"
      ;; Create a regular tool using gptel-make-tool (not macher--make-tool).
      (let ((regular-tool
             (gptel-make-tool
              :name "regular_tool"
              :function (lambda (x) x)
              :description "A regular tool"
              :args '((:name "x" :type string)))))
        ;; Verify macher--tool-context returns nil for this regular tool.
        (expect (macher--tool-context regular-tool) :to-be nil))))

  (describe "macher--functional-preset"
    :var (original-presets test-spec-function test-counter gptel-use-tools-original gptel-tools-original)

    (before-each
      (setq original-presets gptel--known-presets)
      (setq gptel--known-presets nil)
      (setq test-counter 0)
      (setq gptel-use-tools-original gptel-use-tools)
      (setq gptel-tools-original gptel-tools)
      ;; Create a test spec function that increments a counter and returns dynamic values.
      (setq test-spec-function
            (lambda ()
              (setq test-counter (1+ test-counter))
              `(:use-tools
                ,(if (= test-counter 1)
                     t
                   nil)
                :tools
                ,(if (= test-counter 1)
                     '(tool1)
                   '(tool2))))))

    (after-each
      (setq gptel--known-presets original-presets)
      (setq gptel-use-tools gptel-use-tools-original)
      (setq gptel-tools gptel-tools-original))

    (it "creates a functional preset with dynamic values"
      (let ((preset-spec
             (macher--functional-preset
              test-spec-function
              :description "Test functional preset"
              :use-tools nil
              :tools nil)))
        ;; Verify the preset spec has the expected structure.
        (expect (plistp preset-spec) :to-be-truthy)
        (expect (plist-get preset-spec :description) :to-equal "Test functional preset")
        (expect (functionp (plist-get preset-spec :pre)) :to-be-truthy)
        (expect (functionp (plist-get preset-spec :post)) :to-be-truthy)))

    (it "applies dynamic values through :pre and :post functions"
      (let* ((preset-spec
              (macher--functional-preset
               test-spec-function
               :description "Test functional preset"
               :use-tools nil
               :tools nil))
             (pre-fn (plist-get preset-spec :pre))
             (post-fn (plist-get preset-spec :post)))
        ;; Initially, test-counter should be 0.
        (expect test-counter :to-equal 0)

        ;; Call :pre function - this should call our spec function and return the dynamic spec.
        (let ((dynamic-spec (funcall pre-fn)))
          (expect test-counter :to-equal 1)
          (expect (plist-get dynamic-spec :use-tools) :to-be t)
          (expect (plist-get dynamic-spec :tools) :to-equal '(tool1)))

        ;; Call :post function - this should apply the values to gptel variables.
        (funcall post-fn)
        (expect gptel-use-tools :to-be t)
        (expect gptel-tools :to-equal '(tool1))))

    (it "works with gptel-with-preset to apply and reset values"
      (let ((preset-spec
             (macher--functional-preset
              test-spec-function
              :description "Test functional preset"
              :use-tools nil
              :tools nil)))
        ;; Store original values.
        (let ((original-use-tools gptel-use-tools)
              (original-tools gptel-tools))
          ;; Temporarily register the preset.
          (unwind-protect
              (progn
                (apply #'gptel-make-preset 'test-functional-preset preset-spec)
                ;; Use gptel-with-preset to apply the preset temporarily.
                (gptel-with-preset 'test-functional-preset
                  ;; Inside the preset context, values should be applied.
                  (expect gptel-use-tools :to-be t)
                  (expect gptel-tools :to-equal '(tool1)))
                ;; After exiting the preset context, values should be reset.
                (expect gptel-use-tools :to-equal original-use-tools)
                (expect gptel-tools :to-equal original-tools))
            ;; Clean up the temporary preset.
            (setq gptel--known-presets
                  (assq-delete-all 'test-functional-preset gptel--known-presets))))))

    (it "prevents spec function from using invalid keys"
      (let* ((bad-spec-function
              (lambda ()
                ;; Return a spec with a key that's not in the baseline.
                '(:invalid-key "invalid value" :use-tools t)))
             (preset-spec
              (macher--functional-preset
               bad-spec-function
               :description "Test functional preset"
               :use-tools nil)))
        ;; The :pre function should work.
        (let ((pre-fn (plist-get preset-spec :pre)))
          (funcall pre-fn))
        ;; But the :post function should signal an error for the invalid key.
        (let ((post-fn (plist-get preset-spec :post)))
          (expect (funcall post-fn) :to-throw))))

    (it "handles multiple calls to pre/post correctly"
      (let* ((preset-spec
              (macher--functional-preset
               test-spec-function
               :description "Test functional preset"
               :use-tools nil
               :tools nil))
             (pre-fn (plist-get preset-spec :pre))
             (post-fn (plist-get preset-spec :post)))

        ;; First call cycle.
        (let ((dynamic-spec1 (funcall pre-fn)))
          (expect test-counter :to-equal 1)
          (expect (plist-get dynamic-spec1 :use-tools) :to-be t)
          (expect (plist-get dynamic-spec1 :tools) :to-equal '(tool1)))
        (funcall post-fn)
        (expect gptel-use-tools :to-be t)
        (expect gptel-tools :to-equal '(tool1))

        ;; Second call cycle with different values.
        (let ((dynamic-spec2 (funcall pre-fn)))
          (expect test-counter :to-equal 2)
          (expect (plist-get dynamic-spec2 :use-tools) :to-be nil)
          (expect (plist-get dynamic-spec2 :tools) :to-equal '(tool2)))
        (funcall post-fn)
        (expect gptel-use-tools :to-be nil)
        (expect gptel-tools :to-equal '(tool2))))

    (it "handles nil spec function results gracefully"
      (let* ((nil-spec-function (lambda () nil))
             (preset-spec
              (macher--functional-preset
               nil-spec-function
               :description "Test preset with nil spec"
               :use-tools t))
             (pre-fn (plist-get preset-spec :pre))
             (post-fn (plist-get preset-spec :post)))

        ;; :pre function should return nil.
        (let ((dynamic-spec (funcall pre-fn)))
          (expect dynamic-spec :to-be nil))

        ;; :post function should handle nil gracefully and not change anything.
        (let ((original-use-tools gptel-use-tools))
          (funcall post-fn)
          (expect gptel-use-tools :to-equal original-use-tools))))

    (it "only sets variables that exist and are bound"
      (let* ((spec-with-nonexistent-var
              (lambda ()
                ;; Use a key that won't correspond to an existing gptel variable.
                '(:nonexistent-var "test value")))
             (preset-spec
              (macher--functional-preset
               spec-with-nonexistent-var
               :description "Test preset"
               :nonexistent-var nil))
             (post-fn (plist-get preset-spec :post)))

        ;; Call pre to set up the spec.
        (funcall (plist-get preset-spec :pre))

        ;; Post should not error even though the variable doesn't exist.
        (expect (funcall post-fn) :not :to-throw))))

  (describe "macher-action"
    :var ((original-action-buffer-setup-hook macher-action-buffer-setup-hook) project-file-buffer)

    (before-each
      (funcall setup-project)
      ;; Spy on gptel-request and to avoid making requests, but still return a real fsm.
      (spy-on 'gptel-request :and-call-fake (lambda (&rest _) (gptel-make-fsm)))
      ;; Verify in advance that there are no pending file modifications that might have been
      ;; introduced by other tests, as this would cause `save-some-buffers' to try prompting about
      ;; save.
      (expect (seq-find (lambda (b) (and (buffer-file-name b) (buffer-modified-p b))) (buffer-list))
              :to-be nil)

      ;; Use a buffer with the real file contents, to avoid prompts from `save-some-buffers'.
      (setq project-file-buffer (find-file-noselect project-file)))

    (after-each
      (setq macher-action-buffer-setup-hook original-action-buffer-setup-hook)
      (kill-buffer project-file-buffer)
      (setq project-file-buffer nil))

    (it "invokes the action dispatch hooks"
      (let ((dispatched-p nil))
        (with-current-buffer project-file-buffer
          (let ((macher-action-dispatch-hook
                 `(,(lambda ()
                      (setq dispatched-p t)
                      (expect (current-buffer) :to-be project-file-buffer)))))
            (macher-action 'implement nil "test prompt")
            (expect dispatched-p :to-be t)))))

    (it "passes original prompt when no before-action functions modify it"
      (with-current-buffer project-file-buffer
        (let* ((original-prompt "original test prompt")
               ;; Make sure the before actions are empty.
               (macher-before-action-functions nil))
          ;; Call macher-action with the original prompt.
          (macher-action 'implement nil original-prompt)
          ;; Verify gptel-request was called.
          (expect 'gptel-request :to-have-been-called)
          ;; Get the arguments passed to gptel-request.
          (let ((call-args (spy-calls-args-for 'gptel-request 0)))
            ;; The first argument should be the prompt.
            (expect (nth 0 call-args) :to-match original-prompt)))))

    (it "passes modified prompt when before-action functions modify execution object"
      (with-current-buffer project-file-buffer
        (let* ((original-prompt "original test prompt")
               (modified-prompt "modified test prompt")
               (action-called nil)
               (captured-prompt nil)
               (macher-before-action-functions
                (list
                 (lambda (execution)
                   ;; Modify the prompt in the execution object.
                   (setf (macher-action-execution-prompt execution) modified-prompt)))))
          ;; Call macher-action with the original prompt.
          (macher-action 'implement nil original-prompt)
          ;; Verify gptel-request was called.
          (expect 'gptel-request :to-have-been-called)
          ;; Get the arguments passed to gptel-request.
          (let ((call-args (spy-calls-args-for 'gptel-request 0)))
            ;; The first argument should be the modified prompt.
            (expect (nth 0 call-args) :to-match modified-prompt)))))

    (it "passes context when before-action functions set context in execution object"
      (with-current-buffer project-file-buffer
        (let* ((original-prompt "test prompt")
               (test-context '(("test-file.txt" "test content")))
               (action-called nil)
               (captured-context nil)
               (macher-before-action-functions
                (list
                 (lambda (execution)
                   ;; Set context in the execution object.
                   (setf (macher-action-execution-context execution) test-context)))))
          ;; Call macher-action with the original prompt.
          (macher-action 'implement nil original-prompt)
          ;; Verify gptel-request was called.
          (expect 'gptel-request :to-have-been-called)
          ;; Get the arguments passed to gptel-request.
          (let* ((call-args (spy-calls-args-for 'gptel-request 0))
                 (call-plist (cdr call-args))
                 (passed-context (plist-get call-plist :context)))
            ;; The :context key should contain our test context.
            (expect passed-context :to-equal test-context))))))

  (describe "macher--add-transition-handler"
    :var (fsm test-handler handler-calls)

    (before-each
      (setq handler-calls '())
      (setq test-handler (lambda (fsm) (push (gptel-fsm-state fsm) handler-calls)))
      ;; Create an FSM with gptel's standard transitions and handlers
      (setq fsm
            (gptel-make-fsm :table gptel-request--transitions :handlers gptel-request--handlers)))

    (it "adds handler to all states that can be transitioned to"
      ;; Add our test handler
      (macher--add-transition-handler fsm test-handler)

      ;; Verify the handler was added to the correct states
      (let ((handlers (gptel-fsm-handlers fsm)))
        ;; From gptel-request--transitions, these are the states that can be transitioned TO:
        ;; WAIT (from INIT), TYPE (from WAIT), ERRS (from TYPE and TOOL), TOOL (from TYPE), DONE (from TYPE and TOOL)
        ;; So our handler should be added to: WAIT, TYPE, ERRS, TOOL, DONE

        ;; Check that WAIT has our handler.
        (let ((wait-handlers (cdr (assq 'WAIT handlers))))
          (expect (member test-handler wait-handlers) :to-be-truthy))

        ;; Check that TYPE has our handler.
        (let ((type-handlers (cdr (assq 'TYPE handlers))))
          (expect (member test-handler type-handlers) :to-be-truthy))

        ;; Check that ERRS has our handler.
        (let ((errs-handlers (cdr (assq 'ERRS handlers))))
          (expect (member test-handler errs-handlers) :to-be-truthy))

        ;; Check that TOOL has our handler.
        (let ((tool-handlers (cdr (assq 'TOOL handlers))))
          (expect (member test-handler tool-handlers) :to-be-truthy))

        ;; Check that DONE has our handler.
        (let ((done-handlers (cdr (assq 'DONE handlers))))
          (expect (member test-handler done-handlers) :to-be-truthy))

        ;; Check that INIT does NOT have our handler (it's not a target state).
        (let ((init-handlers (cdr (assq 'INIT handlers))))
          (if init-handlers
              (expect (member test-handler init-handlers) :to-be nil)
            ;; INIT might not have any handlers, which is also correct.
            (expect init-handlers :to-be nil)))))

    (it "preserves existing handlers when adding new ones"
      (let* ((existing-handler (lambda (fsm) "existing"))
             (original-handlers (gptel-fsm-handlers fsm)))

        ;; Add an existing handler to WAIT state.
        (let ((wait-entry (assq 'WAIT original-handlers)))
          (if wait-entry
              (setcdr wait-entry (cons existing-handler (cdr wait-entry)))
            ;; If WAIT doesn't have handlers yet, add it.
            (setf (gptel-fsm-handlers fsm) (cons (list 'WAIT existing-handler) original-handlers))))

        ;; Now add our transition handler.
        (macher--add-transition-handler fsm test-handler)

        ;; Verify both handlers are present in WAIT.
        (let ((wait-handlers (cdr (assq 'WAIT (gptel-fsm-handlers fsm)))))
          (expect (member existing-handler wait-handlers) :to-be-truthy)
          (expect (member test-handler wait-handlers) :to-be-truthy))))

    (it "works correctly with custom transition tables"
      (let* ((custom-transitions '((START . ((t . MIDDLE))) (MIDDLE . ((t . END))) (END . ())))
             (custom-handlers '())
             (custom-fsm (gptel-make-fsm :table custom-transitions :handlers custom-handlers)))

        ;; Add our handler to the custom FSM.
        (macher--add-transition-handler custom-fsm test-handler)

        ;; In this transition table:
        ;; START -> MIDDLE, MIDDLE -> END
        ;; So target states are: MIDDLE, END
        ;; START is not a target state

        (let ((handlers (gptel-fsm-handlers custom-fsm)))
          ;; MIDDLE should have our handler (it's a target of START).
          (let ((middle-handlers (cdr (assq 'MIDDLE handlers))))
            (expect (member test-handler middle-handlers) :to-be-truthy))

          ;; END should have our handler (it's a target of MIDDLE).
          (let ((end-handlers (cdr (assq 'END handlers))))
            (expect (member test-handler end-handlers) :to-be-truthy))

          ;; START should NOT have our handler (it's not a target state).
          (let ((start-handlers (cdr (assq 'START handlers))))
            (if start-handlers
                (expect (member test-handler start-handlers) :to-be nil)
              (expect start-handlers :to-be nil))))))

    (it "does not destructively modify the original transitions or handlers"
      (let* ((original-transitions (copy-alist gptel-request--transitions))
             (original-handlers (copy-alist gptel-request--handlers))
             (original-fsm-transitions (gptel-fsm-table fsm))
             (original-fsm-handlers (gptel-fsm-handlers fsm)))

        ;; Add our handler
        (macher--add-transition-handler fsm test-handler)

        ;; Verify that the global constants are unchanged
        (expect gptel-request--transitions :to-equal original-transitions)
        (expect gptel-request--handlers :to-equal original-handlers)

        ;; Verify that the FSM's transitions table is unchanged
        (expect (gptel-fsm-table fsm) :to-equal original-transitions)

        ;; Verify that our handler was actually added to the FSM in the right places.
        (expect (gptel-fsm-handlers fsm) :not :to-equal original-handlers)
        (let ((found-handler-states nil))
          (dolist (state-handlers (gptel-fsm-handlers fsm))
            (when (member test-handler (cdr state-handlers))
              (push (car state-handlers) found-handler-states)))
          (let ((expected-handler-states '(DONE ERRS TOOL TYPE WAIT)))
            (expect (length found-handler-states) :to-equal (length expected-handler-states))
            (expect found-handler-states :to-have-same-items-as expected-handler-states))))))

  (describe "macher--add-termination-handler"
    :var (fsm test-handler handler-calls)

    (before-each
      (setq handler-calls '())
      (setq test-handler (lambda (fsm) (push (gptel-fsm-state fsm) handler-calls)))
      ;; Create an FSM with gptel's standard transitions and handlers
      (setq fsm
            (gptel-make-fsm :table gptel-request--transitions :handlers gptel-request--handlers)))

    (it "adds handler to all terminal states"
      ;; Add our test handler
      (macher--add-termination-handler fsm test-handler)

      ;; Verify the handler was added to the correct states
      (let ((handlers (gptel-fsm-handlers fsm)))
        ;; From gptel-request--transitions, the terminal states are those that either:
        ;; 1. Don't appear as keys in the transitions table, OR
        ;; 2. Appear as keys but have no possible transitions (empty cdr)
        ;; Looking at gptel-request--transitions: DONE and ERRS are terminal states

        ;; Check that DONE has our handler
        (let ((done-handlers (cdr (assq 'DONE handlers))))
          (expect (member test-handler done-handlers) :to-be-truthy))

        ;; Check that ERRS has our handler
        (let ((errs-handlers (cdr (assq 'ERRS handlers))))
          (expect (member test-handler errs-handlers) :to-be-truthy))

        ;; Check that non-terminal states do NOT have our handler
        (let ((wait-handlers (cdr (assq 'WAIT handlers))))
          (if wait-handlers
              (expect (member test-handler wait-handlers) :to-be nil)
            (expect wait-handlers :to-be nil)))

        (let ((type-handlers (cdr (assq 'TYPE handlers))))
          (if type-handlers
              (expect (member test-handler type-handlers) :to-be nil)
            (expect type-handlers :to-be nil)))

        (let ((tool-handlers (cdr (assq 'TOOL handlers))))
          (if tool-handlers
              (expect (member test-handler tool-handlers) :to-be nil)
            (expect tool-handlers :to-be nil)))))

    (it "preserves existing handlers when adding new ones"
      (let* ((existing-handler (lambda (fsm) "existing"))
             (original-handlers (gptel-fsm-handlers fsm)))

        ;; Add an existing handler to DONE state
        (let ((done-entry (assq 'DONE original-handlers)))
          (if done-entry
              (setcdr done-entry (cons existing-handler (cdr done-entry)))
            ;; If DONE doesn't have handlers yet, add it
            (setf (gptel-fsm-handlers fsm) (cons (list 'DONE existing-handler) original-handlers))))

        ;; Now add our termination handler
        (macher--add-termination-handler fsm test-handler)

        ;; Verify both handlers are present in DONE
        (let ((done-handlers (cdr (assq 'DONE (gptel-fsm-handlers fsm)))))
          (expect (member existing-handler done-handlers) :to-be-truthy)
          (expect (member test-handler done-handlers) :to-be-truthy))))

    (it "works correctly with custom transition tables"
      (let* ((custom-transitions '((START . ((t . MIDDLE))) (MIDDLE . ((t . END))) (END . ())))
             (custom-handlers '())
             (custom-fsm (gptel-make-fsm :table custom-transitions :handlers custom-handlers)))

        ;; Add our handler to the custom FSM
        (macher--add-termination-handler custom-fsm test-handler)

        ;; In this transition table:
        ;; START -> MIDDLE, MIDDLE -> END, END -> (nothing)
        ;; So END is the only terminal state

        (let ((handlers (gptel-fsm-handlers custom-fsm)))
          ;; END should have our handler (it's terminal)
          (let ((end-handlers (cdr (assq 'END handlers))))
            (expect (member test-handler end-handlers) :to-be-truthy))

          ;; START and MIDDLE should NOT have our handler (they're not terminal)
          (let ((start-handlers (cdr (assq 'START handlers))))
            (if start-handlers
                (expect (member test-handler start-handlers) :to-be nil)
              (expect start-handlers :to-be nil)))

          (let ((middle-handlers (cdr (assq 'MIDDLE handlers))))
            (if middle-handlers
                (expect (member test-handler middle-handlers) :to-be nil)
              (expect middle-handlers :to-be nil))))))

    (it "handles states that don't appear as keys in transitions table"
      (let* ((custom-transitions '((START . ((t . ORPHAN))) (MIDDLE . ((t . ORPHAN)))))
             (custom-handlers '())
             (custom-fsm (gptel-make-fsm :table custom-transitions :handlers custom-handlers)))

        ;; Add our handler to the custom FSM
        (macher--add-termination-handler custom-fsm test-handler)

        ;; In this transition table:
        ;; START -> ORPHAN, MIDDLE -> ORPHAN
        ;; ORPHAN doesn't appear as a key, so it's terminal

        (let ((handlers (gptel-fsm-handlers custom-fsm)))
          ;; ORPHAN should have our handler (it's terminal - doesn't appear as a key)
          (let ((orphan-handlers (cdr (assq 'ORPHAN handlers))))
            (expect (member test-handler orphan-handlers) :to-be-truthy))

          ;; START and MIDDLE should NOT have our handler (they have transitions)
          (let ((start-handlers (cdr (assq 'START handlers))))
            (if start-handlers
                (expect (member test-handler start-handlers) :to-be nil)
              (expect start-handlers :to-be nil)))

          (let ((middle-handlers (cdr (assq 'MIDDLE handlers))))
            (if middle-handlers
                (expect (member test-handler middle-handlers) :to-be nil)
              (expect middle-handlers :to-be nil))))))

    (it "does not destructively modify the original transitions or handlers"
      (let* ((original-transitions (copy-alist gptel-request--transitions))
             (original-handlers (copy-alist gptel-request--handlers))
             (original-fsm-transitions (gptel-fsm-table fsm))
             (original-fsm-handlers (gptel-fsm-handlers fsm)))

        ;; Add our handler
        (macher--add-termination-handler fsm test-handler)

        ;; Verify that the global constants are unchanged
        (expect gptel-request--transitions :to-equal original-transitions)
        (expect gptel-request--handlers :to-equal original-handlers)

        ;; Verify that the FSM's transitions table is unchanged
        (expect (gptel-fsm-table fsm) :to-equal original-fsm-transitions)

        ;; Verify that our handler was actually added to the FSM in the right places.
        (expect (gptel-fsm-handlers fsm) :not :to-equal original-handlers)
        (let ((found-handler-states nil))
          (dolist (state-handlers (gptel-fsm-handlers fsm))
            (when (member test-handler (cdr state-handlers))
              (push (car state-handlers) found-handler-states)))
          (let ((expected-handler-states '(DONE ERRS)))
            (expect (length found-handler-states) :to-equal (length expected-handler-states))
            (expect found-handler-states :to-have-same-items-as expected-handler-states))))))

  (describe "macher--implement-prompt"
    :var (temp-file)

    (before-each
      (setq temp-file (make-temp-file "macher-test" nil ".js"))
      ;; Write some content to the file.
      (with-temp-buffer
        (insert "// Test JavaScript file\nfunction test() {}\n")
        (write-region (point-min) (point-max) temp-file)))

    (after-each
      (when (file-exists-p temp-file)
        (delete-file temp-file)))

    (it "includes gptel--strip-mode-suffix result for common modes"
      ;; Test with a JavaScript file to verify language detection.
      (with-temp-buffer
        (find-file temp-file)
        ;; Use a mode that's specifically handled by gptel--strip-mode-suffix.
        (sh-mode)
        ;; Set the buffer-local workspace variable.
        (setq-local macher--workspace (cons 'file temp-file))
        (let ((prompt (macher--implement-prompt "Add error handling" nil)))
          ;; The prompt should contain "Javascript" (or similar) as the language.
          ;; This verifies that our condition-case approach works correctly.
          (expect prompt :to-match "Shell")
          (expect prompt :to-match "Add error handling")
          (expect prompt :to-match "IMPLEMENTATION REQUEST"))))

    (it "falls back to mode-name when gptel--strip-mode-suffix fails"
      ;; Temporarily override gptel--strip-mode-suffix to simulate an error.
      (spy-on 'gptel--strip-mode-suffix :and-call-fake (lambda (&rest _) (error "Simulated error")))
      (with-temp-buffer
        (find-file temp-file)
        (js-mode)
        ;; Set the buffer-local workspace variable.
        (setq-local macher--workspace (cons 'file temp-file))
        (let ((prompt (macher--implement-prompt "Add error handling" nil)))
          ;; Should fall back to using mode-name format.
          (expect prompt :to-be-truthy)
          (expect prompt :to-match "Add error handling")
          ;; Should not contain the error itself.
          (expect prompt :not :to-match "Simulated error"))))

    (it "handles case where gptel--strip-mode-suffix doesn't exist"
      ;; Temporarily undefine gptel--strip-mode-suffix.
      (let ((original-function (symbol-function 'gptel--strip-mode-suffix)))
        (unwind-protect
            (progn
              (fmakunbound 'gptel--strip-mode-suffix)
              (with-temp-buffer
                (find-file temp-file)
                (js-mode)
                ;; Set the buffer-local workspace variable.
                (setq-local macher--workspace (cons 'file temp-file))
                (let ((prompt (macher--implement-prompt "Add error handling" nil)))
                  ;; Should still work by falling back to mode-name.
                  (expect prompt :to-be-truthy)
                  (expect prompt :to-match "Add error handling"))))
          ;; Restore the original function.
          (fset 'gptel--strip-mode-suffix original-function)))))

  (describe "macher--resolve-workspace-path"
    :var
    (temp-workspace-root
     temp-file1 temp-file2 temp-subdir temp-subfile project-workspace file-workspace)

    (before-each
      ;; Create temporary workspace structure.
      (setq temp-workspace-root (make-temp-file "macher-workspace-test" t))
      (setq temp-file1 (expand-file-name "file1.txt" temp-workspace-root))
      (setq temp-file2 (expand-file-name "file2.txt" temp-workspace-root))
      (setq temp-subdir (expand-file-name "subdir" temp-workspace-root))
      (setq temp-subfile (expand-file-name "subfile.txt" temp-subdir))

      ;; Create files and directories.
      (write-region "content1" nil temp-file1)
      (write-region "content2" nil temp-file2)
      (make-directory temp-subdir)
      (write-region "subcontent" nil temp-subfile)

      ;; Create project marker.
      (write-region "" nil (expand-file-name ".project" temp-workspace-root))

      ;; Set up workspace objects.
      (setq project-workspace `(project . ,(directory-file-name temp-workspace-root)))
      (setq file-workspace `(file . ,temp-file1)))

    (after-each
      ;; Clean up.
      (when (file-exists-p temp-workspace-root)
        (delete-directory temp-workspace-root t)))

    (it "resolves basic relative paths"
      (let ((result (macher--resolve-workspace-path project-workspace "file1.txt")))
        (expect result :to-equal temp-file1))

      (let ((result (macher--resolve-workspace-path project-workspace "subdir/subfile.txt")))
        (expect result :to-equal temp-subfile)))

    (it "resolves '.' to workspace root"
      (let ((result (macher--resolve-workspace-path project-workspace ".")))
        (expect result :to-equal temp-workspace-root)))

    (it "handles './' prefix correctly"
      (let ((result (macher--resolve-workspace-path project-workspace "./file1.txt")))
        (expect result :to-equal temp-file1)))

    (it "handles .. path components"
      (let ((result (macher--resolve-workspace-path project-workspace "subdir/../file1.txt")))
        (expect result :to-equal temp-file1)))

    (it "returns path for non-existent files without error"
      (let ((result (macher--resolve-workspace-path project-workspace "nonexistent.txt")))
        (expect result :to-equal (expand-file-name "nonexistent.txt" temp-workspace-root))))

    (it "throws error for paths that escape workspace root"
      (expect (macher--resolve-workspace-path project-workspace "../outside.txt") :to-throw)
      (expect (macher--resolve-workspace-path project-workspace "/etc/passwd") :to-throw))

    (it "throws error for existing files not in workspace files list"
      ;; Create an extra file that won't be in the project files list.
      (let ((extra-file (expand-file-name "extra.txt" temp-workspace-root)))
        (write-region "extra content" nil extra-file)

        ;; Spy on macher--workspace-files to exclude extra.txt from the files list.
        (spy-on 'macher--workspace-files
                :and-call-fake
                (lambda (workspace)
                  ;; Get the actual files from the real function but filter out extra.txt.
                  (let ((files (macher--project-files (cdr workspace))))
                    (cl-remove-if (lambda (file) (string-match-p "extra\\.txt$" file)) files))))

        ;; Should succeed for files that are in the project.
        (let ((result (macher--resolve-workspace-path project-workspace "file1.txt")))
          (expect result :to-equal temp-file1))

        ;; Should fail for existing files not in the project.
        (expect (macher--resolve-workspace-path project-workspace "extra.txt") :to-throw)))

    (it "handles special characters in paths"
      ;; Create file with special characters.
      (let ((special-file (expand-file-name "file with spaces & symbols!.txt" temp-workspace-root)))
        (write-region "special content" nil special-file)

        ;; Add it to the project by updating the .project marker (forces project-files to re-scan).
        (write-region "updated" nil (expand-file-name ".project" temp-workspace-root))

        (let ((result
               (macher--resolve-workspace-path
                project-workspace "file with spaces & symbols!.txt")))
          (expect result :to-equal special-file))))

    (it "allows symlinks as final component"
      (let ((symlink-path (expand-file-name "test-symlink" temp-workspace-root)))
        (make-symbolic-link temp-file1 symlink-path)

        ;; Mock workspace files to include the symlink.
        (spy-on 'macher--workspace-files
                :and-call-fake
                (lambda (workspace)
                  (append (macher--project-files (cdr workspace)) (list symlink-path))))

        (let ((result (macher--resolve-workspace-path project-workspace "test-symlink")))
          (expect result :to-equal symlink-path))))

    (it "throws error for symlinks in non-final path components"
      (let ((symlink-dir (expand-file-name "symlink-dir" temp-workspace-root))
            (target-dir (expand-file-name "target" temp-workspace-root))
            (target-file
             (expand-file-name "target.txt" (expand-file-name "target" temp-workspace-root))))

        ;; Create target directory and file.
        (make-directory target-dir)
        (write-region "target content" nil target-file)

        ;; Create symlink to directory.
        (make-symbolic-link target-dir symlink-dir)

        ;; Should throw error when accessing through symlinked directory.
        (expect (macher--resolve-workspace-path project-workspace "symlink-dir/target.txt")
                :to-throw)))

    (it "throws error for files in non-final path components"
      ;; Create a file within the workspace that will be used as a non-final path component.
      (let ((file-in-path (expand-file-name "not-a-dir" temp-workspace-root)))
        (write-region "file content" nil file-in-path)

        ;; Should throw error when trying to access through the file as if it were a directory.
        (expect (macher--resolve-workspace-path project-workspace "not-a-dir/target.txt")
                :to-throw)))

    (it "handles absolute paths within workspace"
      (let ((result (macher--resolve-workspace-path project-workspace temp-file1)))
        (expect result :to-equal temp-file1)))

    (it "handles paths with multiple slashes"
      (let ((result (macher--resolve-workspace-path project-workspace "subdir//subfile.txt")))
        (expect result :to-equal temp-subfile)))

    (it "handles empty string path"
      (let ((result (macher--resolve-workspace-path project-workspace "")))
        (expect result :to-equal temp-workspace-root)))

    (it "allows access to workspace directories even if not in files list"
      ;; Directories should always be accessible, even if not in the files list.
      (let ((result (macher--resolve-workspace-path project-workspace ".")))
        (expect result :to-equal temp-workspace-root))
      ;; Test subdirectories too.
      (let ((result (macher--resolve-workspace-path project-workspace "subdir")))
        (expect result :to-equal temp-subdir)))

    (it "allows access to nested workspace directories even if not in files list"
      ;; Create a nested directory structure that won't be in the project files list.
      (let ((nested-dir (expand-file-name "very/deep/nested/dir" temp-workspace-root)))
        (make-directory nested-dir t)
        (let ((result (macher--resolve-workspace-path project-workspace "very/deep/nested/dir")))
          (expect result :to-equal nested-dir))
        ;; Test intermediate directories too.
        (let ((result (macher--resolve-workspace-path project-workspace "very/deep")))
          (expect result :to-equal (expand-file-name "very/deep" temp-workspace-root)))
        ;; Clean up.
        (delete-directory (expand-file-name "very" temp-workspace-root) t)))

    (it "allows files outside workspace if in workspace files list"
      (let ((outside-file (make-temp-file "macher-test-outside-")))
        ;; Create the outside file with content.
        (write-region "outside content" nil outside-file)

        (unwind-protect
            (progn
              ;; Mock workspace files to include the outside file - this is one case where spy is
              ;; needed since we can't actually add files outside the project to a real project.
              (spy-on 'macher--workspace-files :and-return-value (list outside-file))

              (let ((result (macher--resolve-workspace-path project-workspace outside-file)))
                (expect result :to-equal outside-file)))

          ;; Clean up.
          (when (file-exists-p outside-file)
            (delete-file outside-file)))))

    (it "rejects files and directories outside workspace if not in workspace files list"
      ;; File outside workspace should fail if not in files list.
      (let ((outside-file (make-temp-file "macher-test-outside-file-")))
        (write-region "outside content" nil outside-file)
        (unwind-protect
            ;; Should throw error - file is outside workspace and not in files list.
            (expect (macher--resolve-workspace-path project-workspace outside-file) :to-throw)
          ;; Clean up.
          (when (file-exists-p outside-file)
            (delete-file outside-file))))

      ;; Directory outside workspace should fail even if it exists.
      (let ((outside-dir (make-temp-file "macher-test-outside-dir-" t)))
        (unwind-protect
            ;; Should throw error - directory is outside workspace.
            (expect (macher--resolve-workspace-path project-workspace outside-dir) :to-throw)
          ;; Clean up.
          (when (file-exists-p outside-dir)
            (delete-directory outside-dir))))

      ;; Non-existent directory outside workspace should also fail.
      (let ((non-existent-dir
             (file-name-directory
              (expand-file-name "non-existent-dir/that-should-not-exist"
                                (temporary-file-directory)))))
        (expect (macher--resolve-workspace-path project-workspace non-existent-dir) :to-throw)))

    (it "handles broken symlinks"
      (let ((broken-symlink (expand-file-name "broken-symlink" temp-workspace-root)))
        (make-symbolic-link "/nonexistent/target" broken-symlink)

        ;; Mock workspace files to include the broken symlink.
        (spy-on 'macher--workspace-files
                :and-call-fake
                (lambda (workspace)
                  (append (macher--project-files (cdr workspace)) (list broken-symlink))))

        (let ((result (macher--resolve-workspace-path project-workspace "broken-symlink")))
          (expect result :to-equal broken-symlink))

        ;; Clean up.
        (delete-file broken-symlink)))

    (it "throws error for existing symlinks not in workspace files list"
      ;; Create a symlink that exists but won't be in the project files list
      (let ((untracked-symlink (expand-file-name "untracked-symlink" temp-workspace-root))
            (target-file (expand-file-name "target-for-untracked.txt" temp-workspace-root)))
        ;; Create the target file.
        (write-region "target content" nil target-file)
        ;; Create the symlink.
        (make-symbolic-link target-file untracked-symlink)

        ;; Spy on macher--workspace-files to exclude the symlink from the files list.
        (spy-on 'macher--workspace-files
                :and-call-fake
                (lambda (workspace)
                  ;; Get the actual files from the real function but filter out the symlink.
                  (let ((files (macher--project-files (cdr workspace))))
                    (cl-remove-if
                     (lambda (file) (string-match-p "untracked-symlink$" file)) files))))

        (unwind-protect
            ;; Should throw error - symlink exists but is not in the workspace files list.
            (expect (macher--resolve-workspace-path project-workspace "untracked-symlink")
                    :to-throw)
          ;; Clean up
          (when (file-exists-p untracked-symlink)
            (delete-file untracked-symlink))
          (when (file-exists-p target-file)
            (delete-file target-file)))))

    (it "resolves paths correctly for workspaces under '~'"
      ;; If the workspace root contains an expandable segment like "~", make sure we can still
      ;; resolve files within it.
      (let* ((mock-workspace-root "~/projects/test-project/")
             (mock-file1 (concat mock-workspace-root "src/main.el"))
             (mock-file2 (concat mock-workspace-root "README.md"))
             (home-workspace `(project . ,mock-workspace-root)))

        ;; Mock workspace-files to return our fake file list.
        (spy-on 'macher--workspace-files :and-return-value (list mock-file1 mock-file2))

        ;; Mock workspace-root to return our fake root.
        (spy-on 'macher--workspace-root :and-return-value mock-workspace-root)

        ;; Test resolving "." to workspace root.
        (let ((result (macher--resolve-workspace-path home-workspace ".")))
          (expect result :to-equal (directory-file-name (expand-file-name mock-workspace-root))))

        ;; Test resolving "" to workspace root.
        (let ((result (macher--resolve-workspace-path home-workspace ".")))
          (expect result :to-equal (directory-file-name (expand-file-name mock-workspace-root))))

        ;; Test resolving relative paths to non-existing files (should not throw).
        (let ((result (macher--resolve-workspace-path home-workspace "src/new-file.el")))
          (expect result :to-equal (expand-file-name "src/new-file.el" mock-workspace-root)))

        ;; Test resolving paths with "./" prefix.
        (let ((result (macher--resolve-workspace-path home-workspace "./README.md")))
          (expect result :to-equal (expand-file-name mock-file2)))

        ;; Test resolving paths with ".." components.
        (let ((result (macher--resolve-workspace-path home-workspace "src/../README.md")))
          (expect result :to-equal (expand-file-name mock-file2)))

        ;; Test that paths trying to escape the workspace still fail.
        (expect (macher--resolve-workspace-path home-workspace "../outside.txt") :to-throw)
        (expect (macher--resolve-workspace-path home-workspace "../../etc/passwd") :to-throw)

        ;; Test absolute paths within the mock workspace.
        (let ((result (macher--resolve-workspace-path home-workspace mock-file1)))
          (expect result :to-equal (expand-file-name mock-file1)))

        ;; Test that absolute paths outside the workspace fail (even under home).
        (let ((outside-home-path (expand-file-name "other-project/file.txt" "~")))
          (expect (macher--resolve-workspace-path home-workspace outside-home-path) :to-throw)))))

  (describe "macher--patch-prepare-metadata"
    (it "does not insert empty line between diff and prompt comments"
      (let* ((workspace `(project . "/tmp/test-workspace"))
             (context (macher--make-context :workspace workspace :prompt "test prompt"))
             callback-called)
        (with-temp-buffer
          ;; Insert a mock diff.
          (insert "diff --git a/test.py b/test.py\n")
          (insert "--- a/test.py\n")
          (insert "+++ b/test.py\n")
          (insert "@@ -1 +1 @@\n")
          (insert "-old line\n")
          (insert "+new line\n")
          (insert "\\ No newline at end of file\n")
          ;; Call the metadata function.
          (macher--patch-prepare-metadata context nil (lambda () (setq callback-called t)))
          ;; Verify callback was called.
          (expect callback-called :to-be t)
          ;; Check the buffer content.
          (let* ((content (buffer-string))
                 (all-lines (split-string content "\n"))
                 (lines
                  (if (string-empty-p (car (last all-lines)))
                      (butlast all-lines)
                    all-lines))
                 (after-diff nil)
                 (diff-ended nil))
            ;; Should not have two consecutive newlines before the comment section.
            (expect content :not :to-match "\n\n# ---")
            ;; Should have exactly one newline before the separator.
            (expect content :to-match "\\ No newline at end of file\n# ---")
            ;; Verify all lines after the diff start with #.
            (dolist (line lines)
              (when (string-match-p "^\\\\ No newline at end of file$" line)
                (setq diff-ended t))
              (when diff-ended
                (unless (string-match-p "^\\\\ No newline at end of file$" line)
                  (expect (string-prefix-p "#" line) :to-be t))))))))

    (it "formats patch with prompt correctly"
      (let* ((workspace `(project . "/tmp/test-project"))
             (context (macher--make-context :workspace workspace :prompt "implement hello world"))
             callback-called)
        (with-temp-buffer
          ;; Insert a simple diff.
          (insert "diff --git a/hello.py b/hello.py\n")
          (insert "+print('hello world')\n")
          ;; Call the metadata function.
          (macher--patch-prepare-metadata context nil (lambda () (setq callback-called t)))
          ;; Verify callback was called.
          (expect callback-called :to-be t)
          ;; Check buffer content has the expected structure.
          (let* ((content (buffer-string))
                 (lines (split-string content "\n"))
                 (diff-ended nil))
            ;; Should start with patch metadata.
            (expect content :to-match "^# Patch ID: [a-z0-9]\\{8\\}\n")
            (expect content :to-match "# Project: test-project\n")
            ;; Should contain the diff.
            (expect content :to-match "diff --git a/hello.py")
            ;; Should contain the prompt section.
            (expect content :to-match "# PROMPT for patch ID")
            (expect content :to-match "# implement hello world")
            ;; Should not have empty line between diff and comments.
            (expect content :not :to-match "print('hello world')\n\n# ---")
            ;; Verify all lines after the diff start with # (except for a single trailing newline at
            ;; the very end, which is acceptable).
            (let ((line-num 0))
              (dolist (line lines)
                (when diff-ended
                  ;; Allow empty line only if it's the last line (trailing newline).
                  (unless (and (string-empty-p line) (= line-num (1- (length lines))))
                    (expect (string-prefix-p "#" line) :to-be t)))
                (when (string-equal "+print('hello world')" line)
                  (setq diff-ended t))
                (setq line-num (1+ line-num))))
            (expect diff-ended :to-be t)))))))

(provide 'test-unit)
;;; test-unit.el ends here
