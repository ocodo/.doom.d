;;; plugins/elisp-to-markdown.el -*- lexical-binding: t; -*-
(require 'ocodo-handy-functions)

(defun docstring-args-to-markdown-code (args docstring)
  "Using ARGS transform DOCSTRING arguments to inline markdown `code` style."
  (declare (side-effect-free t))
  (if (string= args "")
      docstring
    (let ((case-fold-search nil))
      (let* ((docstring-replacements (--map
                                      (cons (word-search-regexp it)
                                            (format "`%s`" (downcase it)))
                                      (split-string
                                        (upcase (clean-args args))
                                        " " t " "))))
        (--reduce-from
         (replace-regexp-in-string (car it) (cdr it) acc t)
         docstring docstring-replacements)))))

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

(defun clean-args (args)
  "Clean args for docstring and page anchor"
  (let ((arg-cleaner  (rx  (or "(" ")" "&rest" "&optional"))))
     (replace-regexp-in-string arg-cleaner "" args)))

(defun fn-to-anchor-name (name args)
  "Make page anchor from NAME and ARGS."
  (string-replace " " "-" (downcase (concat name "-" (clean-args args)))))

(defun add-to-toc (name name-anchor function-type)
  "Add a NAME / NAME-ANCHOR to table of contents.
FUNCTION-TYPE any grouping string, typically `[interactive]' `[internal]'."
  (add-to-list 'elisp-to-markdown-table-of-contents
   (format "- [%s](#%s) %s" name name-anchor function-type)
   t))

(defun generate-markdown-defun-entry (fn-info)
  "Generate a markdown entry for FN."
  (declare (side-effect-free t))
  (cl-destructuring-bind (name args docstring is-interactive is-internal) fn-info
   (let* ((name (format "%s" name))
          (args (if args (format " %s" args) ""))
          (name-anchor (fn-to-anchor-name name args))
          (function-type (or (and is-interactive "command")
                             (and is-internal "internal")
                             "")))
       (add-to-toc name name-anchor function-type)
       (when (string= nil docstring)
         (setq docstring "No docstring available: TODO"))
       (format-multiline "|### <a id=\"%s\" aria-hidden=\"true\"></a>%s %s
                          |
                          |%s
                          |
                          |<sup>function signature</sup>
                          |```lisp
                          |(%s)
                          |```
                          |
                          |- - -
                          |"
               name-anchor
               name
               function-type
               ;; TODO: Process the docstring as code and text separately.
               ;; (docstring-to-text-and-code docstring)
               (docstring-options-table-to-markdown
                 (docstring-args-to-markdown-code args
                  (docstring-back-quoted-to-markdown-code docstring)))
               (format "%s%s" name args)))))

(defun docstring-options-table-to-markdown (docstring)
  "Convert a table definition in S to markdown."
  (set-match-data nil t)
  (let ((table-regexp "#TABLE \\(.+?\\) *?#\n\\(\\(.*?\n\\)*?.*?\\)\n#TABLE#"))
   (string-match table-regexp docstring)
   (if (and (not (s-blank-str? (match-string 0 docstring)))
            (not (s-blank-str? (match-string 1 docstring))))
       (let* ((source (match-string 0 docstring))
              (heading-string (match-string 1 docstring))
              (body-string (match-string 2 docstring))
              (heading-row (replace-regexp-in-string
                            "\\([[:alnum:][:space:]]+?\\) - \\(.*\\)"
                            "| \\1 | \\2 |\n|-|-|\n"
                            heading-string))
              (body-rows (if (not (null body-string))
                             (s-join "\n"
                              (--map  (replace-regexp-in-string
                                       "\\([^[:space:]]+\\) - \\(.*\\)?"
                                       "| `\\1` | \\2 |"
                                       it)
                               (split-string body-string "\n" t)))
                           ""))
              (table (format "%s%s" heading-row body-rows))
              (docstring-converted (string-replace source table docstring)))
         (if (string-match-p table-regexp docstring-converted)
             (docstring-options-table-to-markdown docstring-converted)
           docstring-converted))
     docstring)))

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
  (let* ((page (concat
                (format-multiline "|# %s
                                   |%s
                                   | - - -
                                   |# Function reference
                                   |<!-- [#TOC#] !-->
                                   |"
                                  (s-capitalized-words (s-replace-regexp "[.]el$" "" (buffer-name buffer)))
                                  (docstring-back-quoted-to-markdown-code (lm-commentary (buffer-file-name))))
                (generate-markdown-list-of-buffer-defuns buffer)))
         (page  (string-replace "<!-- [#TOC#] !-->" (elisp-to-markdown-format-table-of-contents) page)))
      page))

(defun elisp-to-markdown-format-table-of-contents ()
  "Return a string of the contents of list `elisp-to-markdown-table-of-contents'"
  (let* ((grouped (-group-by (lambda (entry)
                               (cond
                                ((s-ends-with? "command" entry) "### Commands")
                                ((s-ends-with? "internal" entry) "### Internal Functions")
                                ((s-starts-with? "#" entry) "Heading")
                                (t "### User Functions")))
                             elisp-to-markdown-table-of-contents))
         (toc-heading (cadr (assoc-string "Head" grouped)))
         (commands    (assoc-string "### Commands" grouped))
         (functions   (assoc-string "### User Functions" grouped))
         (internal    (assoc-string "### Internal Functions" grouped)))
    (concat toc-heading
            "\n"
            (s-join "\n" (-flatten (--map (replace-regexp-in-string " command$" "" it) commands)))
            "\n"
            (s-join "\n" (-flatten functions))
            "\n"
            (s-join "\n" (-flatten (--map (replace-regexp-in-string " internal$" "" it) internal))))))

;;;###autoload
(defun current-buffer-defuns-to-markdown (file)
  "Create a markdown FILE of all defuns in the current buffer."
  (interactive "FWrite List of defuns to Markdown File: ")
  (buffer-defuns-to-markdown (current-buffer) file))

;;;###autoload
(defun buffer-defuns-to-markdown (buffer file)
  "Parse all defuns in BUFFER and save to markdown FILE."
  (setq elisp-to-markdown-table-of-contents '("## Table of Contents"))
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
