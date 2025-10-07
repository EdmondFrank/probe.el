;;; probe-search-enhanced.el --- Enhanced UI functions for probe-search -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Edmond Frank

;; Author: Edmond Frank <edmondfrank@hotmail.com>
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; This file provides enhanced UI functions for probe-search with syntax
;; highlighting and improved formatting.
;;
;;; Code:

(require 'cl-lib)

;; Forward declarations to avoid circular dependency
(declare-function probe-search--project-root "probe-search")
(defvar probe-search--search-term)
(defvar probe-search-reranker)
(defvar probe-search-meta-face)
(defvar probe-search-filename-face)
(defvar probe-search-line-number-face)
(defvar probe-search-separator-face)
(defvar probe-search-match-face)

(defun probe-search--get-mode-for-file (filename)
  "Get the appropriate major mode for FILENAME."
  (let ((mode (assoc-default filename auto-mode-alist 'string-match)))
    (if (and mode (symbolp mode))
        mode
      'prog-mode)))

(defun probe-search--apply-syntax-highlighting (code filename search-term)
  "Apply syntax highlighting to CODE based on FILENAME's mode.
  Also highlight SEARCH-TERM matches."
  (with-temp-buffer
    (insert code)
    (let ((mode (probe-search--get-mode-for-file filename)))
      (condition-case err
          (progn
            (when (and mode (not (fboundp mode)))
              (require mode nil t))
            (when (and mode (fboundp mode))
              (funcall mode)
              (font-lock-mode 1)
              (when font-lock-defaults
                (let ((font-lock-use-overlays nil))
                  (font-lock-fontify-buffer)))))
        (error
         (message "Syntax highlighting for %s failed: %s" filename (error-message-string err)))))

    ;; Highlight search matches on top of syntax highlighting.
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (stringp search-term)
        (dolist (word (split-string search-term "[ \t\n]+" t))
          (when (> (length word) 0)
            (goto-char (point-min))
            (while (re-search-forward (concat "\\<" (regexp-quote word)) nil t)
              (put-text-property (match-beginning 0) (match-end 0)
                                 'face 'probe-search-match-face))))))
    (buffer-string)))

(defun probe-search--insert-json-results-enhanced (json-data)
  "Insert JSON results from probe search with enhanced formatting.
JSON-DATA is the parsed JSON response from probe."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq buffer-read-only nil)
    (let* ((results-raw (cdr (assoc 'results json-data)))
           ;; Convert vector to list if necessary
           (results (if (vectorp results-raw)
                       (append results-raw nil)
                     results-raw))
           (summary (cdr (assoc 'summary json-data))))
      (if (or (null results) (eq (length results) 0))
          (progn
            (insert "\n")
            (insert (propertize "  No results found.\n" 
                              'face 'probe-search-meta-face)))
        (let ((current-filename nil)
              (file-count 0))
          ;; Insert header
          (insert (propertize (format "\n  Found %d results in %d files (reranker: %s)\n" 
                                    (length results)
                                    (length (delete-dups (mapcar (lambda (r) 
                                                                  (cdr (assoc 'file r)))
                                                                results)))
                                    (symbol-name probe-search-reranker))
                            'face 'probe-search-meta-face))
          (insert (propertize (make-string 70 ?─) 'face 'probe-search-separator-face))
          (insert "\n\n")
          
          (save-excursion
            (goto-char (point-min))
            (dolist (result results)
              (when (cdr (assoc 'code result))
                (let ((filename (cdr (assoc 'file result)))
                      (lines (cdr (assoc 'lines result)))
                      (content (cdr (assoc 'code result)))
                      (score (cdr (assoc 'score result))))
                  (unless (equal filename current-filename)
                    (when current-filename
                      (insert "\n")
                      (insert (propertize (make-string 70 ?─) 
                                        'face 'probe-search-separator-face))
                      (insert "\n\n"))
                    (setq file-count (1+ file-count))
                    (let* ((relative-filename (file-relative-name filename 
                                                                 (probe-search--project-root)))
                           (pretty-filename
                            (propertize relative-filename
                                      'face 'probe-search-filename-face
                                      'probe-search-filename filename
                                      'read-only t
                                      'front-sticky t)))
                      (insert pretty-filename)
                      ;; Add collapse/expand indicator
                      (insert (propertize " [-] (click to collapse)" 
                                        'face 'probe-search-meta-face))
                      (when score
                        (insert (propertize (format " (score: %.3f)" score)
                                          'face 'probe-search-meta-face)))
                      (insert "\n\n"))
                    (setq current-filename filename))
                  
                  ;; Format and insert code block
                  (let* ((line-start (if (vectorp lines) (aref lines 0) (car lines)))
                         (highlighted-content (probe-search--apply-syntax-highlighting 
                                             content filename probe-search--search-term)))
                    ;; Add line numbers to each line
                    (let ((content-lines (split-string highlighted-content "\n")))
                      (dotimes (i (length content-lines))
                        (let* ((line-num (+ line-start i))
                               (line-text (nth i content-lines))
                               (pretty-line-num
                                (propertize (format "%4d │ " line-num)
                                          'face 'probe-search-line-number-face
                                          'probe-search-filename filename
                                          'probe-search-line-number line-num
                                          'read-only t
                                          'front-sticky t
                                          'rear-nonsticky t)))
                          (insert pretty-line-num)
                          (insert line-text)
                          (unless (= i (1- (length content-lines)))
                            (insert "\n"))))))
                  (insert "\n")))))))
      
      ;; Insert footer
      (goto-char (point-max))
      (insert "\n")
      (insert (propertize (make-string 70 ?─) 'face 'probe-search-separator-face))
      (insert "\n")
      (when summary
        (let ((count (cdr (assoc 'count summary)))
              (bytes (cdr (assoc 'total_bytes summary)))
              (tokens (cdr (assoc 'total_tokens summary))))
          (insert (propertize 
                   (format "  Summary: %d results | %d bytes | %d tokens\n" 
                          (or count 0) (or bytes 0) (or tokens 0))
                   'face 'probe-search-meta-face)))))
    (setq buffer-read-only t)
    (goto-char (point-min))
    (forward-line 1)))

(defun probe-search--test-apply-syntax-highlighting ()
  "Test `probe-search--apply-syntax-highlighting`."
  (interactive)
  (message "Running syntax highlighting tests...")
  (let ((failures 0)
        (successes 0))
    (cl-flet ((expect-face-at (description code filename search-term pos expected-face)
                (let* ((result (probe-search--apply-syntax-highlighting code filename search-term))
                       (face-found (get-text-property pos 'face result)))
                  (if (equal face-found expected-face)
                      (progn (setq successes (1+ successes))
                             (message "PASS: %s" description))
                    (progn (setq failures (1+ failures))
                           (message "FAIL: %s. Expected '%s' at pos %d, found '%s'" description expected-face pos face-found))))))

      ;; Python test
      (expect-face-at "Python keyword"
                      "def my_func():" "test.py" "other"
                      0 'font-lock-keyword-face)

      ;; Elisp test
      (expect-face-at "Elisp keyword"
                      "(defun my-func () \"docstring\")" "test.el" "other"
                      1 'font-lock-keyword-face)

      ;; Elisp string test
      (expect-face-at "Elisp string"
                      "(defun my-func () \"docstring\")" "test.el" "other"
                      19 'font-lock-string-face)

      ;; Javascript test
      (expect-face-at "JavaScript keyword"
                      "function myFunc() { return 1; }" "test.js" "other"
                      0 'font-lock-keyword-face)

      ;; Test fallback for unknown file type (should have no face)
      (expect-face-at "Fallback (no face)"
                      "hello world" "test.unknown" "world"
                      0 nil))
    (message "Tests finished. %d successes, %d failures." successes failures)))

(provide 'probe-search-enhanced)
;;; probe-search-enhanced.el ends here
