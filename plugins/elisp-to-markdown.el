;;; plugins/elisp-to-markdown.el -*- lexical-binding: t; -*-
(require 'ocodo-handy-functions)

(defun docstring-args-to-markdown-code (args docstring)
  "Using ARGS transform DOCSTRING arguments to inline markdown `code` style."
  (declare (side-effect-free t))
  (let ((case-fold-search nil))
    (let* ((arg-cleaner  (rx  (or "(" ")" "&rest" "&optional")))
           (docstring-replacements (--map
                                    (cons (word-search-regexp it)
                                          (format "`%s`" (downcase it)))
                                    (split-string
                                     (upcase (replace-regexp-in-string
                                              arg-cleaner "" args))
                                     " " t " "))))

      (--reduce-from (replace-regexp-in-string (car it) (cdr it) acc t)

       docstring docstring-replacements))))

(defun docstring-back-quoted-to-markdown-code (docstring)
  "transform back-quoted docstring elements to inline markdown `code` style."
  (declare (side-effect-free t))
  (if (null docstring)
      ""
    (replace-regexp-in-string
      (rx "`" (group (*? not-newline)) "'")
      "`\\1`"
      docstring)))

(defun defun-is-interactive (fn-info)
  "Check FN-INFO is-interactive"
  (and (nth 3 fn-info) "--"))

(defun defun-is-internal (fn-info)
  "Check FN-INFO is-internal"
  (and (nth 4 fn-info) "zz"))

(defun generate-markdown-defun-entry (fn-info)
  "Generate a markdown entry for FN."
  (declare (side-effect-free t))
  (cl-destructuring-bind (name args docstring is-interactive is-internal) fn-info
   (let ((name (format "%s" name))
         (args (if args (format " %s" args) "")))
       (when (string= nil docstring)
         (setq docstring "No docstring available: TODO"))
       (format "### %s%s\n\n%s\n\n<sup>function signature</sup>\n```lisp\n(%s)\n```\n\n- - -\n"
               name
               (or (and is-interactive " [command]")
                   (and is-internal " [internal]")
                   "")
               ;; TODO: Process the docstring as code and text separately.
               ;; (docstring-to-text-and-code docstring)
               (docstring-table-to-markdown
                (docstring-args-to-markdown-code args
                                 (docstring-back-quoted-to-markdown-code
                                   docstring)
                               (format "%s%s" name args)))))))

(defun docstring-options-table-to-markdown (docstring)
  "Convert a table definition in S to markdown."
  (string-match "#TABLE \\(.+?\\) *?#\n\\(\\(.*?\n\\)*?.*?\\)\n#TABLE#" docstring)
  (if (and (match-string 1 docstring) (match-string 2 docstring))
      (let* ((heading-string (match-string 1 docstring))
             (body-string (match-string 2 docstring))
             (heading-row (replace-regexp-in-string
                           "\\([^[:space:]]+?\\) +- +\\(.*\\)"
                           "| \\1 | \\2 |\n|-|-|\n"
                           heading-string))
             (body-rows   (s-join "\n"
                           (--map  (replace-regexp-in-string
                                    "\\([^[:space:]]+\\)[ -]+\\(.*\\)"
                                    "| `\\1' | \\2 |"
                                    it)
                            (s-split "\n" body-string)))))
         (format "%s%s" heading-row body-rows))
    docstring))

(defun get-defun-info (buffer)
  "Get information about all `defun' top-level sexps in a BUFFER.
Returns a list with elements of the form (symbol args docstring)."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let (result)
          ;; keep going while reading succeeds
          (while (condition-case nil
                     (progn
                       (read (current-buffer))
                       (forward-sexp -1)
                       t)
                   (error nil))
            (let ((form (read (current-buffer))))
              (cond
               ((not (listp form)) nil)
               ((eq (nth 0 form) 'defun)
                (let* ((sym (nth 1 form))
                       (name (symbol-name sym))
                       (args (nth 2 form))
                       (doc (when (stringp (nth 3 form)) (nth 3 form)))
                       (is-internal (s-contains? "--" name))
                       (is-interactive (string= "interactive" (car (nth 4 form)))))
                  (push (list sym args doc is-interactive is-internal) result))))))
          result)))))

(defun generate-markdown-list-of-buffer-defuns (buffer)
  "Generate markdown text of all defuns in buffer"
  (s-join "\n"
          (mapcar
           #'generate-markdown-defun-entry
            (-sort (lambda (a b)
                     (let* ((name-a (symbol-name (car a)))
                            (name-b (symbol-name (car b)))
                            (c (format "%s%s"
                                       (or (defun-is-interactive a) (defun-is-internal a) "")
                                       name-a))
                            (d (format "%s%s"
                                       (or (defun-is-interactive b) (defun-is-internal b) "")
                                       name-b)))
                       (string< c d)))
                   (get-defun-info (current-buffer))))))

(defun generate-markdown-page-of-buffer-defuns (&optional buffer)
  "Generate markdown page for all defun in BUFFER.

BUFFER file name and commentary are used as the page heading."
  (concat
   (format-multiline "|# %s
                      |%s
                      | - - -
                      |## Functions
                      |
                      |"
                     (s-capitalized-words (s-replace-regexp "[.]el$" "" (buffer-name buffer)))
                     (docstring-back-quoted-to-markdown-code (lm-commentary (buffer-file-name))))
   (generate-markdown-list-of-buffer-defuns buffer)))

;;;###autoload
(defun current-buffer-defuns-to-markdown (file)
  "Create a markdown FILE of all defuns in the current buffer."
  (interactive "FWrite List of defuns to Markdown File: ")
  (buffer-defuns-to-markdown (current-buffer) file))

;;;###autoload
(defun buffer-defuns-to-markdown (buffer file)
  "Parse all defuns in BUFFER and save to markdown FILE."
  (f-write  (generate-markdown-page-of-buffer-defuns buffer) 'utf-8 file)
  (when (y-or-n-p (format "Open %s?" file))
    (find-file file)))

;;;###autoload
(defun elisp-file-defuns-to-markdown (elisp-file markdown-file)
  "Parse all defuns in ELISP-FILE and save to MARKDOWN-FILE."
  (interactive "fSelect Elisp: \nFWrite to Markdown File: ")
  (find-file elisp-file)
  (buffer-defuns-to-markdown (current-buffer) markdown-file))

(provide 'elisp-to-markdown)
