;;; probe-search.el --- AI-friendly, fully local, semantic code search with probe -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Edmond Frank

;; Author: Edmond Frank <edmondfrank@hotmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (s "1.13.0"))
;; Keywords: tools, search, semantic, ast, code-navigation, convenience
;; URL: https://github.com/edmondfrank/probe.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Usage:
;; (require 'probe-search)
;; M-x probe-search          ; Interactive text search
;; M-x probe-query          ; Interactive AST query
;; M-x probe-query-method   ; Search for methods by name
;; M-x probe-query-class    ; Search for classes by name
;; M-x probe-query-function ; Search for functions by name
;; M-x probe-search-menu    ; Interactive configuration menu

;;; Key bindings in search results:
;; RET          - Visit result at point
;; g            - Restart search
;; q            - Quit search window
;; n/p          - Navigate results
;; s            - Start new search
;; a            - Start AST query
;; t            - Toggle test file inclusion
;; r            - Change reranking algorithm

;;; Commentary:
;; This package provides AI-friendly, fully local semantic code search with AST integration.
;; It supports both text search and structural AST queries for precise code navigation.
;; Features include project-aware searching, configurable reranking algorithms,
;; and intelligent filtering options.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'project)

;; Load enhanced UI if available
(require 'probe-search-enhanced nil t)

(defgroup probe-search nil
  "Customization group for probe search."
  :group 'tools)

(defcustom probe-search-command "probe"
  "Command for probe search (default: probe)."
  :type 'string
  :group 'probe-search)

(defcustom probe-search-display-buffer-function
  'switch-to-buffer-other-window
  "Function used to show the probe search result buffer."
  :type 'function
  :group 'probe-search)

(defcustom probe-search-reranker 'bm25
  "Ranking algorithm for search results.
Available options:
- bm25: BM25 ranking algorithm (default)
- hybrid: Hybrid ranking combining multiple signals
- hybrid2: Alternative hybrid ranking
- tfidf: TF-IDF ranking
- ms-marco-tinybert: Microsoft MARCO TinyBERT model
- ms-marco-minilm-l6: Microsoft MARCO MiniLM L6 model
- ms-marco-minilm-l12: Microsoft MARCO MiniLM L12 model"
  :type '(choice (const :tag "BM25 (default)" bm25)
                 (const :tag "Hybrid" hybrid)
                 (const :tag "Hybrid2" hybrid2)
                 (const :tag "TF-IDF" tfidf)
                 (const :tag "MS MARCO TinyBERT" ms-marco-tinybert)
                 (const :tag "MS MARCO MiniLM L6" ms-marco-minilm-l6)
                 (const :tag "MS MARCO MiniLM L12" ms-marco-minilm-l12))
  :group 'probe-search)

(defcustom probe-search-include-tests nil
  "Whether to include test files in search results."
  :type 'boolean
  :group 'probe-search)

(defcustom probe-search-max-results nil
  "Maximum number of results to return (nil for no limit)."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'probe-search)

(defcustom probe-search-use-project-root t
  "Whether to search from the project root instead of current directory."
  :type 'boolean
  :group 'probe-search)

(defcustom probe-search-default-mode 'text
  "Default search mode to use."
  :type '(choice (const text) (const ast))
  :group 'probe-search)

(defvar-local probe-search--search-term nil)
(put 'probe-search--search-term 'permanent-local t)

(defvar-local probe-search--process nil)
(put 'probe-search--process 'permanent-local t)

(defvar-local probe-search--current-file nil)
(put 'probe-search--current-file 'permanent-local t)

(defvar-local probe-search--remaining-output nil
  "Store incomplete lines from process output.")
(put 'probe-search--remaining-output 'permanent-local t)

(defface probe-search-filename-face
  '((((class color) (background light))
     :foreground "DarkBlue" :weight bold :height 1.1)
    (((class color) (background dark))
     :foreground "LightSkyBlue" :weight bold :height 1.1)
    (t :inherit bold))
  "Face used for filename headings in results buffers."
  :group 'probe-search)

(defface probe-search-match-face
  '((((class color) (background light))
     :background "yellow1" :foreground "black" :weight bold)
    (((class color) (background dark))
     :background "RoyalBlue4" :foreground "white" :weight bold)
    (t :inherit match))
  "Face used for the portion of a line that matches the search term."
  :group 'probe-search)

(defface probe-search-line-number-face
  '((((class color) (background light))
     :foreground "gray50")
    (((class color) (background dark))
     :foreground "gray60")
    (t :inherit font-lock-comment-face))
  "Face used for line numbers."
  :group 'probe-search)

(defface probe-search-separator-face
  '((t :foreground "gray70" :strike-through t :extend t))
  "Face used for separators between files."
  :group 'probe-search)

(defface probe-search-meta-face
  '((t :inherit font-lock-comment-face))
  "Face used for probe search UI text."
  :group 'probe-search)

(defconst probe-search--color-code
  (rx "\x1b[" (+ digit) "m")
  "Regular expression for an ANSI color code.")

(defconst probe-search--filename-regexp
  (rx bos "\x1b[0m\x1b[3" (or "5" "6") "m"
      (? "./")
      (group (+? anything))
      "\x1b[")
  "Extracts the filename from a probe line with ANSI color sequences.")

(defconst probe-search--line-num-regexp
  (rx "\x1b[32m" (group (+ digit)))
  "Extracts the line number from a probe line with ANSI color sequences.")

(defconst probe-search--line-contents-regexp
  (rx "\x1b[32m" (+ digit) "\x1b[0m" (or ":" "-") (group (* anything)))
  "Extract the line contents from a probe line with ANSI color sequences.")

(defconst probe-search--hit-regexp
  (rx-to-string
   `(seq
     ;; A reset color code.
     "\x1b[0m"
     ;; Two color codes, bold and color (any order).
     (regexp ,probe-search--color-code)
     (regexp ,probe-search--color-code)
     ;; The actual text.
     (group (+? anything))
     ;; A reset color code again.
     "\x1b[0m"))
  "Extract the portion of a line found by probe that matches the user's input.")

(defvar probe-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'probe-search-visit-result)
    (define-key map (kbd "g") #'probe-search-restart)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "s") #'probe-search)
    (define-key map (kbd "a") #'probe-query)
    (define-key map (kbd "t") #'probe-search-toggle-tests)
    (define-key map (kbd "r") #'probe-search-rerank)
    (define-key map (kbd "TAB") #'probe-search-toggle-file-visibility)
    map)
  "Keymap for `probe-search-mode'.")

(define-derived-mode probe-search-mode special-mode "Probe-Search"
  "Major mode for probe search results buffers."
  (setq buffer-read-only t))

(defun probe-search--extract-regexp (pattern s)
  "Search for PATTERN in S, and return the content of the first group."
  (string-match pattern s)
  (match-string 1 s))

(defun probe-search--parse-json-result (result)
  "Parse a single JSON result structure."
  (let-alist result
    (list .file .line_start .content)))

(defun probe-search--escape-backslash (s)
  "Escape occurrences of backslashes in S."
  (replace-regexp-in-string "\\\\" "\\\\\\\\" s))

(defun probe-search--propertize-hits (line-contents)
  "Given LINE-CONTENTS from probe, replace ANSI color codes
with a text face property `probe-search-match-face'."
  (replace-regexp-in-string
   probe-search--hit-regexp
   (lambda (s)
     (propertize
      (probe-search--escape-backslash (match-string 1 s))
      'face 'probe-search-match-face))
   line-contents))

(defun probe-search--highlight-matches (text search-term)
  "Highlight SEARCH-TERM matches in TEXT."
  (let ((case-fold-search t)
        (highlighted-text text))
    ;; Split search term into words for highlighting
    (dolist (word (split-string search-term "[ \t\n]+" t))
      (when (> (length word) 0)
        ;; Create a case-insensitive regexp for the word
        (let ((regexp (concat "\\b" (regexp-quote word) "\\b")))
          (setq highlighted-text
                (replace-regexp-in-string
                 regexp
                 (lambda (match)
                   (propertize match 
                              'face 'probe-search-match-face
                              'font-lock-face 'probe-search-match-face))
                 highlighted-text t t)))))
    highlighted-text))

(defun probe-search--insert-json-results (json-data)
  "Insert JSON results from probe search.
JSON-DATA is the parsed JSON response from probe."
  ;; Use enhanced UI if available
  (if (fboundp 'probe-search--insert-json-results-enhanced)
      (probe-search--insert-json-results-enhanced json-data)
    (probe-search--insert-json-results-basic json-data)))

(defun probe-search--insert-json-results-basic (json-data)
  "Basic JSON results insertion.
JSON-DATA is the parsed JSON response from probe."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq buffer-read-only nil)
    (let* ((results-raw (cdr (assoc 'results json-data)))
           ;; Convert vector to list if necessary
           (results (if (vectorp results-raw)
                       (append results-raw nil)
                     results-raw)))
      (if (or (null results) (eq (length results) 0))
          (progn
            (insert "\n  ")
            (insert (propertize "No results found.\n" 'face 'probe-search-meta-face)))
        (let ((current-filename nil))
          ;; Insert search header
          (insert "\n")
          (insert (propertize (format "  Search results for: %s\n" probe-search--search-term)
                            'face 'probe-search-filename-face))
          (insert (propertize (format "  Found %d results (reranker: %s)\n" 
                                    (length results)
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
                      (content (cdr (assoc 'code result))))
                  (unless (equal filename current-filename)
                    (when current-filename
                      (insert "\n")
                      (insert (propertize (make-string 60 ?─) 'face 'probe-search-separator-face))
                      (insert "\n\n"))
                    (let* ((relative-filename (file-relative-name filename 
                                                                  (probe-search--project-root)))
                           (pretty-filename
                            (propertize relative-filename
                                       'face 'probe-search-filename-face
                                       'probe-search-filename filename
                                       'read-only t
                                       'front-sticky t)))
                      (insert pretty-filename "\n"))
                    (setq current-filename filename))

                  (let* ((line-start (if (vectorp lines) (aref lines 0) (car lines)))
                         (pretty-line-num
                          (propertize (format "%4d │ " line-start)
                                      'face 'probe-search-line-number-face
                                      'probe-search-filename filename
                                      'probe-search-line-number line-start
                                      'read-only t
                                      'front-sticky t
                                      'rear-nonsticky t))
                         ;; Highlight matches in content
                         (highlighted-content (probe-search--highlight-matches 
                                             content probe-search--search-term)))
                    (insert pretty-line-num highlighted-content "\n")))))))))
    (setq buffer-read-only t)
    (goto-char (point-min))))

(defvar-local probe-search--collected-output "")
(put 'probe-search--collected-output 'permanent-local t)

(defvar-local probe-search--collapsed-files nil
  "List of filenames that are currently collapsed.")
(put 'probe-search--collapsed-files 'permanent-local t)

(defvar-local probe-search--file-overlays nil
  "Alist of (filename . overlay) for hidden file sections.")
(put 'probe-search--file-overlays 'permanent-local t)

(defun probe-search-toggle-file-visibility ()
  "Toggle visibility of the file section at point."
  (interactive)
  (let* ((pos (point))
         (filename (or (get-text-property pos 'probe-search-filename)
                      ;; Try to find filename from current or previous line
                      (save-excursion
                        (beginning-of-line)
                        (when (re-search-backward "^[^ \t\n]" nil t)
                          (get-text-property (point) 'probe-search-filename))))))
    (if filename
        (if (member filename probe-search--collapsed-files)
            (probe-search--expand-file filename)
          (probe-search--collapse-file filename))
      (message "Not on a file section"))))

(defun probe-search--collapse-file (filename)
  "Collapse the file section for FILENAME."
  (add-to-list 'probe-search--collapsed-files filename)
  (save-excursion
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found) (re-search-forward "^[^ \t\n]" nil t))
        (when (equal (get-text-property (point) 'probe-search-filename) filename)
          (setq found t)
          (let* ((file-start (line-beginning-position))
                 ;; Find the content start (after the filename line)
                 (content-start (save-excursion
                                  (forward-line 1)
                                  (while (and (not (eobp))
                                              (looking-at "^\s-*$\|^\s-*─"))
                                    (forward-line 1))
                                  (point)))
                 ;; Find the end of this file's results
                 (content-end (save-excursion
                                (goto-char content-start)
                                ;; Look for the next file header or separator
                                (if (re-search-forward "^[^ \t\n].*\\|^─\\{60,\\}$" nil t)
                                    (progn
                                      ;; Move back to start of the line we found
                                      (beginning-of-line)
                                      ;; If we're on a separator line, we want to stop before it
                                      (if (looking-at "^─")
                                          ;; Move back one line to not include the separator
                                          (forward-line -1)
                                        ;; We're on a file header, stay at beginning of that line
                                        nil)
                                      (end-of-line)
                                      (point))
                                  (point-max)))))
            (when (> content-end content-start)
              (let ((ov (make-overlay content-start content-end)))
                (overlay-put ov 'invisible 'probe-search-collapsed)
                (overlay-put ov 'probe-search-collapsed t)
                (push (cons filename ov) probe-search--file-overlays)))
            ;; Update the file header to show collapsed state
            (goto-char file-start)
            (when (looking-at "^\\([^[:space:]].*\\)$")
              (let ((ov (make-overlay file-start (line-end-position))))
                (overlay-put ov 'display 
                             (propertize (format "%s [+] (click to expand)" 
                                                 (file-relative-name filename (probe-search--project-root)))
                                         'face (list :foreground (face-attribute 'probe-search-filename-face :foreground)
                                                     :weight 'bold)))
                (push (cons (concat filename "-header") ov) probe-search--file-overlays)))))))
    (message "Collapsed %s" (file-name-nondirectory filename))))

(defun probe-search--expand-file (filename)
  "Expand the file section for FILENAME."
  (setq probe-search--collapsed-files (remove filename probe-search--collapsed-files))
  ;; Remove all overlays for this file
  (let ((remaining-overlays nil))
    (dolist (ov-pair probe-search--file-overlays)
      (if (or (equal (car ov-pair) filename)
              (equal (car ov-pair) (concat filename "-header")))
          (delete-overlay (cdr ov-pair))
        (push ov-pair remaining-overlays)))
    (setq probe-search--file-overlays (nreverse remaining-overlays)))
  (message "Expanded %s" (file-name-nondirectory filename)))

(defun probe-search--process-filter (process output)
  "Handle output from the probe process, collecting JSON data."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t))
        (setq probe-search--collected-output
              (concat probe-search--collected-output output))))))

(defun probe-search--process-sentinel (process event)
  "Handle process termination and process JSON results.
PROCESS is the probe process.
EVENT is the process status change event."
  (let ((buffer (process-buffer process))
        (exit-status (process-exit-status process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (cond
         ;; Process finished successfully
         ((string-match-p "finished" event)
          (probe-search--handle-successful-completion))
         ;; Process exited with error
         ((and (string-match-p "exited" event) (not (zerop exit-status)))
          (probe-search--handle-error-completion exit-status))
         ;; Process killed
         ((string-match-p "killed" event)
          (message "Probe search was cancelled")))))))

(defun probe-search--handle-successful-completion ()
  "Handle successful probe search completion."
  (let ((json-start (string-match "{" probe-search--collected-output)))
    (if json-start
        (let ((json-string (substring probe-search--collected-output json-start)))
          (condition-case err
              (when (and json-string (not (string-blank-p json-string)))
                (let ((json-data (json-read-from-string json-string)))
                  (probe-search--insert-json-results json-data)
                  (message "Probe search completed successfully")))
            (error 
             (probe-search--handle-json-error err json-string))))
      (message "No results found"))))

(defun probe-search--handle-error-completion (exit-status)
  "Handle probe search error.
EXIT-STATUS is the process exit code."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize 
             (format "\n\nError: Probe exited with status %d\n" exit-status)
             'face 'error))
    (when probe-search--collected-output
      (insert (propertize "Output:\n" 'face 'bold))
      (insert probe-search--collected-output))))

(defun probe-search--handle-json-error (err json-string)
  "Handle JSON parsing error.
ERR is the error object.
JSON-STRING is the string that failed to parse."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize "\n\nError parsing probe output:\n" 'face 'error))
    (insert (format "%s\n" err))
    (when (and json-string (> (length json-string) 0))
      (insert (propertize "JSON preview:\n" 'face 'bold))
      (insert (substring json-string 0 (min 200 (length json-string))))
      (insert "...\n"))))

(defun probe-search--buffer-name (search-term)
  "Generate a buffer name for the search results."
  (format "*probe search %s*" search-term))

(defun probe-search--buffer (search-term)
  "Create or get a search results buffer."
  (let* ((buf-name (probe-search--buffer-name search-term))
         (buf (get-buffer buf-name)))
    (unless buf
      (setq buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (probe-search-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq probe-search--search-term search-term)
        (setq probe-search--current-file nil)
        (setq probe-search--collected-output "")))
    buf))

(defun probe-search--project-root ()
  "Get the project root directory for searching."
  (if probe-search-use-project-root
      (or
       ;; Try project.el first (works with various project types)
       (when (fboundp 'project-root)
         (when-let ((project (project-current)))
           (expand-file-name (project-root project))))
       ;; Fallback to git root
       (locate-dominating-file default-directory ".git")
       ;; Fallback to current directory
       default-directory)
    default-directory))

(defun probe-search-visit-result ()
  "Visit the file and line at point."
  (interactive)
  (let* ((pos (line-beginning-position))
         (filename (get-text-property pos 'probe-search-filename))
         (line-num (get-text-property pos 'probe-search-line-number)))
    (cond
     ;; If we're on a result line with filename and line number
     ((and filename line-num)
      (find-file-other-window filename)
      (goto-char (point-min))
      (forward-line (1- line-num)))
     ;; If we're on a file heading (filename but no line number)
     ((and filename (not line-num))
      (message "Point is on file heading, not a specific result"))
     ;; Try to find the filename from previous lines
     (t
      (save-excursion
        (while (and (not filename) (> (point) (point-min)))
          (forward-line -1)
          (setq filename (get-text-property (point) 'probe-search-filename))
          (setq line-num (get-text-property (point) 'probe-search-line-number)))
        (when (and filename line-num)
          (find-file-other-window filename)
          (goto-char (point-min))
          (forward-line (1- line-num))))))))

(defun probe-search-restart ()
  "Restart the current search."
  (interactive)
  (when probe-search--search-term
    (probe-search probe-search--search-term)))

(defun probe-search--build-args (query &optional ast-mode)
  "Build probe command arguments based on current settings."
  (append
   (list (if ast-mode "query" "search"))
   (list "--format" "json")
   (when probe-search-reranker
     (list "--reranker" (symbol-name probe-search-reranker)))
   (when probe-search-include-tests
     (list "--allow-tests"))
   (when probe-search-max-results
     (list "--max-results" (number-to-string probe-search-max-results)))
   (list query)
   (list (probe-search--project-root))))

(defun probe-search--run (query &optional ast-mode)
  "Run probe command with QUERY.
If AST-MODE is non-nil, use AST query mode."
  (unless (executable-find probe-search-command)
    (error "Probe command not found: %s" probe-search-command))
  
  (let* ((buffer (probe-search--buffer query))
         (process-name (format "probe %s%s" (if ast-mode "query " "search ") query))
         (search-path (expand-file-name (probe-search--project-root))))
    
    ;; Kill any existing process
    (when (and probe-search--process (process-live-p probe-search--process))
      (kill-process probe-search--process))
    
    (message "Searching in: %s" search-path)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq probe-search--collected-output "")
        (insert (propertize "Searching...\n" 'face 'probe-search-meta-face)))
      
      (condition-case err
          (progn
            (setq probe-search--process
                  (apply #'start-process process-name buffer probe-search-command
                         (probe-search--build-args query ast-mode)))
            (set-process-filter probe-search--process #'probe-search--process-filter)
            (set-process-sentinel probe-search--process #'probe-search--process-sentinel))
        (error
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (propertize (format "Failed to start probe: %s\n" err) 'face 'error))))))
    (funcall probe-search-display-buffer-function buffer)))

;;;###autoload
(defun probe-search (query)
  "Perform probe search for QUERY using the probe command."
  (interactive "sSearch query: ")
  (probe-search--run query nil))

;;;###autoload
(defun probe-query (query)
  "Perform AST query search for QUERY using the probe command."
  (interactive "sAST query: ")
  (probe-search--run query t))

(defun probe-query-method (method-name)
  "Search for methods matching METHOD-NAME using AST query."
  (interactive "sMethod name: ")
  (probe-query (format "method::*%s*" method-name)))

(defun probe-query-class (class-name)
  "Search for classes matching CLASS-NAME using AST query."
  (interactive "sClass name: ")
  (probe-query (format "class::*%s*" class-name)))

(defun probe-query-function (function-name)
  "Search for functions matching FUNCTION-NAME using AST query."
  (interactive "sFunction name: ")
  (probe-query (format "function::*%s*" function-name)))

(defun probe-search-toggle-tests ()
  "Toggle whether to include test files in search results."
  (interactive)
  (setq probe-search-include-tests (not probe-search-include-tests))
  (message "Test files %s in search results"
           (if probe-search-include-tests "included" "excluded")))

(defun probe-search-rerank ()
  "Change the reranking algorithm for search results."
  (interactive)
  (let ((choices '(("BM25 (default)" . bm25)
                   ("Hybrid" . hybrid)
                   ("Hybrid2" . hybrid2)
                   ("TF-IDF" . tfidf)
                   ("MS MARCO TinyBERT" . ms-marco-tinybert)
                   ("MS MARCO MiniLM L6" . ms-marco-minilm-l6)
                   ("MS MARCO MiniLM L12" . ms-marco-minilm-l12))))
    (let* ((current (symbol-name probe-search-reranker))
           (choice (completing-read 
                    (format "Select reranker (current: %s): " current)
                    (mapcar #'car choices)
                    nil t))
           (new-reranker (cdr (assoc choice choices))))
      (when new-reranker
        (setq probe-search-reranker new-reranker)
        (message "Reranker changed to %s" (symbol-name new-reranker))
        ;; Restart the search with new reranker if we have a current search
        (when probe-search--search-term
          (probe-search-restart))))))

(defun probe-search-menu ()
  "Interactive menu for probe search configuration."
  (interactive)
  (let ((choices '("Text search" . "AST query")))
    (pcase (completing-read "Search mode: " choices)
      ((or "Text search" "1")
       (call-interactively #'probe-search))
      ((or "AST query" "2")
       (call-interactively #'probe-query)))))

(provide 'probe-search)
;;; probe-search.el ends here
