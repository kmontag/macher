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
      (setq file (make-temp-file "macher-test-file" nil ".txt"))))

   ;; Helper to check that directories are absolute and differ only by a trailing slash.
   (expect-dirs-match
    (lambda (dir1 dir2)
      (expect (file-name-absolute-p dir1) :to-be-truthy)
      (expect (file-name-absolute-p dir2) :to-be-truthy)
      (expect (file-directory-p dir1) :to-be-truthy)
      (expect (file-directory-p dir2) :to-be-truthy)

      (expect (expand-file-name "test" dir1) :to-equal (expand-file-name "test" dir2)))))

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
      ;; Write some test content to the file
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
      ;; Both should initially contain the same content
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
        (expect
         (assoc (macher--normalize-path non-existent) (macher-context-contents context))
         :to-be-truthy))))

  (describe "macher-context--set-new-content-for-file"
    :var (context temp-file original-contents)

    (before-each
      (setq context (macher--make-context))
      (setq temp-file (make-temp-file "macher-test"))
      ;; Write some test content to the file
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
    :var (temp-file)

    (before-each
      (setq temp-file (make-temp-file "macher-test"))
      ;; Write some test content to the file.
      (with-temp-buffer
        (insert "original file content")
        (write-region (point-min) (point-max) temp-file)))

    (after-each
      (when (file-exists-p temp-file)
        (delete-file temp-file)))

    (it "sets the dirty-p flag"
      ;; Test that edit tools set the dirty-p flag via the wrapper.
      (let* ((workspace (cons 'file temp-file))
             (context (macher--make-context :workspace workspace))
             (make-tool-function (apply-partially #'macher--make-tool context))
             (edit-tools (macher--edit-tools context make-tool-function)))
        ;; Initially, dirty-p should be nil.
        (expect (macher-context-dirty-p context) :to-be nil)
        ;; Find the write_file_in_workspace tool.
        (let ((write-tool
               (cl-find-if
                (lambda (tool)
                  (string= (gptel-tool-name tool) "write_file_in_workspace"))
                edit-tools)))
          (expect write-tool :to-be-truthy)
          ;; Call the tool function.
          (funcall (gptel-tool-function write-tool)
                   (file-name-nondirectory temp-file)
                   "test content")
          ;; dirty-p should now be t.
          (expect (macher-context-dirty-p context) :to-be t)))))

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
      (macher--process-request context fsm)
      ;; Verify macher--build-patch was called.
      (expect #'macher--build-patch :to-have-been-called))

    (it "does not call macher--build-patch when dirty-p is nil"
      ;; Ensure dirty-p is nil (default).
      (expect (macher-context-dirty-p context) :to-be nil)
      ;; Call macher--process-request.
      (macher--process-request context fsm)
      ;; Verify macher--build-patch was not called.
      (expect #'macher--build-patch :not :to-have-been-called))

    (it "does not call macher--build-patch when context is nil"
      ;; Call with nil context.
      (macher--process-request nil fsm)
      ;; Verify macher--build-patch was not called.
      (expect #'macher--build-patch :not :to-have-been-called)))

  (describe "macher--edit-string"
    (it "replaces single occurrence of old text with new text"
      (let* ((content "Hello world! This is a test.")
             (old-text "world")
             (new-text "universe")
             (result (macher--edit-string content old-text new-text)))
        (expect result :to-equal "Hello universe! This is a test.")))

    (it "handles empty old string in empty content"
      (let* ((content "")
             (old-text "")
             (new-text "new content")
             (result (macher--edit-string content old-text new-text)))
        (expect result :to-equal "new content")))

    (it "signals error for empty old string in non-empty content"
      (let ((content "some content")
            (old-text "")
            (new-text "replacement"))
        (expect (macher--edit-string content old-text new-text) :to-throw)))

    (it "signals error when old text is not found"
      (let ((content "Hello world!")
            (old-text "missing")
            (new-text "replacement"))
        (expect (macher--edit-string content old-text new-text) :to-throw)))

    (it "signals error when old text appears multiple times"
      (let ((content "test test test")
            (old-text "test")
            (new-text "replacement"))
        (expect (macher--edit-string content old-text new-text) :to-throw)))

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

  (describe "macher--project-files"
    :var (temp-dir file1 file2 subdir file3 project-file)

    (it "signals error with nil project root"
      (expect (macher--project-files nil) :to-throw))

    (it "returns project files with relative paths"
      (funcall setup-project)
      (expect project-dir :to-be-truthy)
      (let ((files (macher--project-files project-dir)))
        (expect files :to-be-truthy)
        (expect (listp files) :to-be-truthy)
        ;; Should contain relative paths.
        (expect files :to-contain "file1.txt")
        (expect files :to-contain "file2.el")
        (expect files :to-contain "subdir/file3.md"))))

  (describe "macher--context-string"
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
        ;; Should mark files in context with [*].
        (expect result :to-match "\\[\\*\\] file1\\.txt")
        (expect result :to-match "\\[\\*\\] file2\\.el")
        ;; file3 should not be marked since it's not in contexts.
        (expect result :to-match "    subdir/file3\\.md")
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
        (expect result :not :to-match "file3.md"))))

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
            (funcall expect-dirs-match (cdr result) project-dir))))

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
            (funcall expect-dirs-match (cdr result) project-dir))))

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
                (funcall expect-dirs-match (cdr result) project-dir))))))

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
              (funcall expect-dirs-match (cdr result) project-dir)))))))

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
            (expect
             (cl-find-if (lambda (tool) (string= (gptel-tool-name tool) "regular_tool")) tools)
             :to-be-truthy)
            ;; Should contain the macher tool.
            (expect
             (cl-find-if (lambda (tool) (string= (gptel-tool-name tool) "macher_tool")) tools)
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
            (expect
             (cl-find-if
              (lambda (tool) (string= (gptel-tool-name tool) "existing_macher_tool")) tools)
             :to-be-truthy)
            ;; Should contain the new tool.
            (expect
             (cl-find-if (lambda (tool) (string= (gptel-tool-name tool) "new_macher_tool")) tools)
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
            (expect
             (cl-find-if
              (lambda (tool)
                (and (string= (gptel-tool-name tool) "conflicting_tool")
                     (not (macher--tool-context tool))))
              tools)
             :to-be nil)
            ;; Should contain the new conflicting tool.
            (expect
             (cl-find-if
              (lambda (tool)
                (and (string= (gptel-tool-name tool) "conflicting_tool")
                     (macher--tool-context tool)))
              tools)
             :to-be-truthy)
            ;; Should contain the other tool.
            (expect
             (cl-find-if (lambda (tool) (string= (gptel-tool-name tool) "other_tool")) tools)
             :to-be-truthy)
            ;; Should contain the unique tool.
            (expect
             (cl-find-if (lambda (tool) (string= (gptel-tool-name tool) "unique_tool")) tools)
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
          (spy-on
           'display-warning
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
           (expect
            (member #'macher--prompt-transform-add-context gptel-prompt-transform-functions)
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
           (expect
            (cl-find-if
             (lambda (tool) (string= (gptel-tool-name tool) "existing_tool")) gptel-tools)
            :to-be-truthy)
           ;; Verify that macher tools are also present.
           (expect
            (cl-find-if
             (lambda (tool) (string= (gptel-tool-name tool) "read_file_in_workspace")) gptel-tools)
            :to-be-truthy)))
        ;; Verify that the global tools list is restored after the preset.
        (expect gptel-tools :to-equal (list existing-tool))
        ;; Verify that the existing tool was indeed in the captured tools list.
        (expect
         (cl-find-if
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
        (expect
         (plist-get macher-preset :description)
         :to-equal "Send macher workspace context + tools to read files and propose edits")
        (expect
         (plist-get macher-ro-preset :description)
         :to-equal "Send macher workspace context + tools to read files")
        (expect
         (plist-get macher-notools-preset :description)
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
          ;; Check that the basic hooks were added.
          (expect (member #'macher--before-action macher-before-action-functions) :to-be-truthy)
          (expect (member #'macher--after-action macher-after-action-functions) :to-be-truthy)
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
          ;; Check that hooks were added.
          (expect (member #'macher--before-action macher-before-action-functions) :to-be-truthy)
          (expect (member #'macher--after-action macher-after-action-functions) :to-be-truthy))))

    (it "sets up org UI correctly"
      (let ((macher-action-buffer-ui 'org))
        (with-temp-buffer
          (setq-local macher--workspace '(test . "/tmp/test"))
          (macher--action-buffer-setup)
          ;; Check that org-mode is enabled.
          (expect (derived-mode-p 'org-mode) :to-be-truthy)
          ;; Check that gptel-mode is enabled.
          (expect (bound-and-true-p gptel-mode) :to-be-truthy)
          ;; Check that hooks were added.
          (expect (member #'macher--before-action macher-before-action-functions) :to-be-truthy)
          (expect (member #'macher--after-action macher-after-action-functions) :to-be-truthy))))

    (it "performs no setup when UI is nil"
      (let ((macher-action-buffer-ui nil))
        (with-temp-buffer
          (setq-local macher--workspace '(test . "/tmp/test"))
          (macher--action-buffer-setup)
          ;; Check that no hooks were added.
          (expect (member #'macher--before-action macher-before-action-functions) :to-be nil)
          (expect (member #'macher--after-action macher-after-action-functions) :to-be nil)
          ;; Check that gptel-mode is NOT enabled.
          (expect (bound-and-true-p gptel-mode) :to-be nil)
          ;; Check that major mode is still fundamental-mode.
          (expect major-mode :to-be 'fundamental-mode))))

    (it "raises error for unrecognized UI value"
      (let ((macher-action-buffer-ui 'invalid-value))
        (with-temp-buffer
          (setq-local macher--workspace '(test . "/tmp/test"))
          (expect (macher--action-buffer-setup) :to-throw 'user-error)))))

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
                (gptel-with-preset
                 'test-functional-preset
                 ;; Inside the preset context, values should be applied.
                 (expect gptel-use-tools :to-be t) (expect gptel-tools :to-equal '(tool1)))
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
      (expect
       (seq-find (lambda (b) (and (buffer-file-name b) (buffer-modified-p b))) (buffer-list))
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
            (macher-action 'implement "test prompt")
            (expect dispatched-p :to-be t)))))

    (it "passes original prompt when no before-action functions modify it"
      (with-current-buffer project-file-buffer
        (let* ((original-prompt "original test prompt")
               ;; Make sure the before actions are empty.
               (macher-before-action-functions nil))
          ;; Call macher-action with the original prompt.
          (macher-action 'implement original-prompt)
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
          (macher-action 'implement original-prompt)
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
          (macher-action 'implement original-prompt)
          ;; Verify gptel-request was called
          (expect 'gptel-request :to-have-been-called)
          ;; Get the arguments passed to gptel-request.
          (let* ((call-args (spy-calls-args-for 'gptel-request 0))
                 (call-plist (cdr call-args))
                 (passed-context (plist-get call-plist :context)))
            ;; The :context key should contain our test context.
            (expect passed-context :to-equal test-context)))))))


;; Local variables:
;; elisp-autofmt-load-packages-local: ("./_defs.el")
;; end:

(provide 'test-unit)
;;; test-unit.el ends here
