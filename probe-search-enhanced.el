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

(defun probe-search--get-mode-for-file (filename)
  "Get the appropriate major mode for FILENAME."
  (let ((mode (assoc-default filename auto-mode-alist 'string-match)))
    (if (and mode (symbolp mode))
        mode
      'prog-mode)))

(defun probe-search--apply-syntax-highlighting (code filename search-term)
  "Apply syntax highlighting to CODE based on FILENAME's mode.
  Also highlight SEARCH-TERM matches."
  (let ((mode (probe-search--get-mode-for-file filename)))
    (with-temp-buffer
      (insert code)
      (delay-mode-hooks (funcall mode))
      (font-lock-ensure)
      ;; Now highlight search matches on top of syntax highlighting
      (goto-char (point-min))
      (let ((case-fold-search t))
        (dolist (word (split-string search-term "[ \t\n]+" t))
          (when (> (length word) 0)
            (goto-char (point-min))
            (while (re-search-forward (regexp-quote word) nil t)
              (let ((match-start (match-beginning 0))
                    (match-end (match-end 0)))
                ;; Get existing face at this position
                (let ((existing-face (get-text-property match-start 'face))
                      (highlight-bg (if (eq (frame-parameter nil 'background-mode) 'dark)
                                        "#3a5d9f"  ; Softer blue for dark mode
                                      "#ffff88"))) ; Softer yellow for light mode
                  ;; Preserve existing syntax highlighting while adding background
                  (if existing-face
                      (progn
                        (put-text-property match-start match-end 
                                           'face 
                                           (if (listp existing-face)
                                               `(,@existing-face (:background ,highlight-bg :weight bold))
                                             `(,existing-face (:background ,highlight-bg :weight bold))))
                        (put-text-property match-start match-end 
                                           'font-lock-face 
                                           (if (listp existing-face)
                                               `(,@existing-face (:background ,highlight-bg :weight bold))
                                             `(,existing-face (:background ,highlight-bg :weight bold)))))
                    ;; No existing face, just apply highlight
                    (put-text-property match-start match-end 
                                       'face 
                                       `(:background ,highlight-bg :weight bold))
                    (put-text-property match-start match-end 
                                       'font-lock-face 
                                       `(:background ,highlight-bg :weight bold))))))))
        (buffer-string)))))

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
          (insert (propertize (format "\n  Found %d results in %d files\n" 
                                    (length results)
                                    (length (delete-dups (mapcar (lambda (r) 
                                                                  (cdr (assoc 'file r)))
                                                                results))))
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
                         (line-end (if (vectorp lines) 
                                     (aref lines (1- (length lines)))
                                   (car (last lines))))
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

(provide 'probe-search-enhanced)
;;; probe-search-enhanced.el ends here
