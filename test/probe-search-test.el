;;; probe-search-test.el --- Tests for probe-search -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Edmond Frank

;; Author: Edmond Frank <edmondfrank@hotmail.com>

;;; Commentary:
;;
;; Test suite for probe-search.el
;;
;;; Code:

(require 'ert)
(require 'probe-search)
(require 'probe-search-enhanced)

(ert-deftest probe-search-test-buffer-name ()
  "Test buffer name generation."
  (should (equal (probe-search--buffer-name "test query")
                 "*probe search test query*")))

(ert-deftest probe-search-test-project-root ()
  "Test project root detection."
  (let ((probe-search-use-project-root t))
    ;; Should return a directory
    (should (file-directory-p (probe-search--project-root))))
  
  (let ((probe-search-use-project-root nil))
    ;; Should return default-directory when disabled
    (should (equal (probe-search--project-root) default-directory))))

(ert-deftest probe-search-test-escape-backslash ()
  "Test backslash escaping."
  (should (equal (probe-search--escape-backslash "test\\path")
                 "test\\\\path"))
  (should (equal (probe-search--escape-backslash "no-backslash")
                 "no-backslash")))

(ert-deftest probe-search-test-highlight-matches ()
  "Test search term highlighting."
  (let ((text "This is a test of the highlighting function")
        (search-term "test highlighting"))
    (let ((result (probe-search--highlight-matches text search-term)))
      ;; Should contain the original text
      (should (string-match-p "test" result))
      (should (string-match-p "highlighting" result)))))

(ert-deftest probe-search-test-build-args ()
  "Test command argument building."
  (let ((probe-search-include-tests nil)
        (probe-search-max-results nil))
    (let ((args (probe-search--build-args "test query" nil)))
      (should (member "search" args))
      (should (member "--format" args))
      (should (member "json" args))
      (should (member "test query" args))
      (should-not (member "--allow-tests" args))))
  
  (let ((probe-search-include-tests t)
        (probe-search-max-results 50))
    (let ((args (probe-search--build-args "test query" t)))
      (should (member "query" args))
      (should (member "--allow-tests" args))
      (should (member "--max-results" args))
      (should (member "50" args)))))

;; Removed test for probe-search--parse-json-result as function no longer exists

(ert-deftest probe-search-test-get-mode-for-file ()
  "Test file mode detection."
  (should (eq (probe-search--get-mode-for-file "test.el") 'emacs-lisp-mode))
  (should (eq (probe-search--get-mode-for-file "test.py") 'python-mode))
  (should (eq (probe-search--get-mode-for-file "test.js") 'js-mode))
  (should (eq (probe-search--get-mode-for-file "unknown.xyz") 'prog-mode)))

(ert-deftest probe-search-test-mode-setup ()
  "Test probe-search-mode setup."
  (with-temp-buffer
    (probe-search-mode)
    (should (eq major-mode 'probe-search-mode))
    (should buffer-read-only)
    (should (keymapp probe-search-mode-map))
    (should (eq (lookup-key probe-search-mode-map (kbd "RET"))
                'probe-search-visit-result))))

(ert-deftest probe-search-test-toggle-tests ()
  "Test toggling test file inclusion."
  (let ((probe-search-include-tests nil))
    (probe-search-toggle-tests)
    (should probe-search-include-tests)
    (probe-search-toggle-tests)
    (should-not probe-search-include-tests)))

(ert-deftest probe-search-test-json-results-handling ()
  "Test JSON results insertion."
  (with-temp-buffer
    (probe-search-mode)
    (let ((json-data '((results . [((file . "/test/file.el")
                                    (lines . [1 2 3])
                                    (code . "test code")
                                    (score . 0.95))])
                      (summary . ((count . 1)
                                 (total_bytes . 100)
                                 (total_tokens . 20))))))
      (probe-search--insert-json-results-basic json-data)
      (should (> (buffer-size) 0))
      (should (string-match-p "file.el" (buffer-string)))
      (should (string-match-p "test code" (buffer-string))))))

(ert-deftest probe-search-test-error-handling ()
  "Test error handling in various functions."
  ;; Test with nil input
  (should-not (probe-search--extract-regexp "test" nil))
  
  ;; Test with empty JSON data
  (with-temp-buffer
    (probe-search-mode)
    (let ((json-data '((results . []))))
      (probe-search--insert-json-results-basic json-data)
      (should (string-match-p "No results found" (buffer-string))))))

(ert-deftest probe-search-test-json-results-enhanced ()
  "Test enhanced JSON results insertion."
  (with-temp-buffer
    (probe-search-mode)
    (let ((json-data '((results . [((file . "/test/file.el")
                                    (lines . [1 2 3])
                                    (code . "test code")
                                    (score . 0.95))])
                      (summary . ((count . 1)
                                 (total_bytes . 100)
                                 (total_tokens . 20))))))
      (probe-search--insert-json-results-enhanced json-data)
      (should (> (buffer-size) 0))
      (should (string-match-p "file.el" (buffer-string)))
      (should (string-match-p "test code" (buffer-string)))
      (should (string-match-p "score" (buffer-string))) ; Enhanced shows score
      (should (string-match-p "Summary" (buffer-string))) ; Enhanced shows summary
      )))

(ert-deftest probe-search-test-file-collapse-expand ()
  "Test file section collapse and expand functionality."
  (with-temp-buffer
    (probe-search-mode)
    (let ((json-data '((results . [((file . "/test/file1.el")
                                    (lines . [1])
                                    (code . "first file content")
                                    (score . 0.95))
                                   ((file . "/test/file2.el")
                                    (lines . [1])
                                    (code . "second file content")
                                    (score . 0.90))])
                      (summary . ((count . 2)
                                 (total_bytes . 200)
                                 (total_tokens . 40))))))
      (probe-search--insert-json-results-enhanced json-data)
      ;; Test initial state - files should be expanded
      (should-not probe-search--collapsed-files)
      ;; Test collapsing a file
      (goto-char (point-min))
      (re-search-forward "file1.el" nil t)
      (probe-search-toggle-file-visibility)
      (should (member "/test/file1.el" probe-search--collapsed-files))
      ;; Test expanding a file
      (probe-search-toggle-file-visibility)
      (should-not (member "/test/file1.el" probe-search--collapsed-files)))))

(ert-deftest probe-search-test-process-handling ()
  "Test process output collection and JSON parsing."
  (with-temp-buffer
    (probe-search-mode)
    ;; Test output collection
    (setq probe-search--collected-output "")
    (let ((mock-process (make-marker))) ; Simple mock
      (probe-search--process-filter mock-process "{\"results\": []}")
      (should (equal probe-search--collected-output "{\"results\": []}")))
    ;; Test successful completion handling
    (probe-search--handle-successful-completion)
    (should (string-match-p "No results found" (buffer-string)))))

(provide 'probe-search-test)
;;; probe-search-test.el ends here
