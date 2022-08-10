;;; plugins/elisp-to-markdown.el -*- lexical-binding: t; -*-

(defun order-substring-matches (raw-substring-matches)
  "Order a set of RAW-SUBSTRING-MATCHES.

Ordered substrings can then be used to perform replacements
on the original source string.

The list is sorted last to first, so that string replacements
don't invalidate replacements using subsequent substring indexes.

Raw substring matches are in the form:

    '((\"s1\" ((10 . 12) (30 . 32) ...))...)
      (\"s2\" ((15 . 17) (20 . 22) ...))...)

This would become:

    '((\"s1\" (30 . 33)))
      (\"s2\" (20 . 22)))
      (\"s2\" (15 . 17)))
      (\"s1\" (10 . 13)))
"
  (--sort
     (> (car (car (cdr it)))
        (car (car (cdr other))))
   (cl-reduce (lambda (acc entry)
                (let* ((indexes (car (cdr entry)))
                       (str (car entry))
                       (mapped (--map
                                `(,str ,it)
                                indexes)))
                 (seq-concatenate 'list acc mapped)))
              raw-substring-matches
              :initial-value '())))

(defun docstring-args-to-markdown-code (args docstring)
  "Using ARGS transform DOCSTRING arguments to inline markdown `code` style."
  (let ((case-fold-search nil))
   (let* ((replacements '(("(" . "")
                          (")" . "")
                          ("&rest" . "")
                          ("&optional" . "")))
          (arg-list     (s-split-words (upcase (s-trim (s-replace-all replacements args)))))
          (doc-matches  (--map
                         `(,it ,(reverse
                                 (s-matched-positions-all
                                  it
                                  docstring)))
                         arg-list))
          (edit-list    (order-substring-matches doc-matches)))
     (cl-reduce (lambda (doc match)
                  (let* ((name (car match))
                         (a (caadr match))
                         (b (cdadr match))
                         (left (substring doc 0 a))
                         (right (substring doc b)))
                     (if (segments-ok-p left right)
                       (format "%s`%s`%s" left (downcase name) right)
                      doc)))
                edit-list
                :initial-value docstring))))

(defun segments-ok-p (left-string right-string)
  "Check the LEFT-STRING and RIGHT-STRING."
  (let ((a  (substring (reverse left-string) 0 1))
        (b  (substring right-string 0 1)))
    (not (s-matches?
          "[A-Za-z]"
          (concat a b)))))

(defun docstring-back-quoted-to-markdown-code (docstring)
  "transform back-quoted docstring elements to inline markdown `code` style."
  (replace-regexp-in-string
   (rx "`" (group (*? not-newline)) "'")
   "`\\1`"
   docstring))

(defun generate-markdown-defun-entry (fn)
  "Generate a markdown entry for FN."
  (cl-destructuring-bind (name args docstring) fn
   (let ((name (format "%s" name))
         (args (if args (format " %s" args) "")))
       (when (string= nil docstring)
         (setq docstring "No docstring available: TODO"))
       (format "### %s\n\n%s\n\n<sup>function signature</sup>\n```lisp\n(%s)\n```\n\n- - -\n"
               name
               (docstring-args-to-markdown-code args
                (docstring-back-quoted-to-markdown-code
                  docstring))
               (format "%s%s" name args)))))

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
               ((not (listp form))      ; if it's not a list, skip it
                nil)
               ((eq (nth 0 form) 'defun) ; if it's a defun, collect info
                (let ((sym (nth 1 form))
                      (args (nth 2 form))
                      (doc (when (stringp (nth 3 form)) (nth 3 form))))
                  (push (list sym args doc) result))))))
          result)))))

(defun generate-markdown-list-of-buffer-defuns (buffer)
  "Generate markdown text of all defuns in buffer"
  (s-join "\n"
          (mapcar
           #'generate-markdown-defun-entry
           (-sort (lambda (a b)
                    (let ((c (symbol-name (first a)))
                          (d (symbol-name (first b))))
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

(defun current-buffer-defuns-to-markdown (file)
  "Create a markdown FILE of all defuns in the current buffer."
  (interactive "FWrite List of defuns to Markdown File: ")
  (f-write (generate-markdown-page-of-buffer-defuns (current-buffer)) 'utf-8 file)
  (when (y-or-n-p (format "Open %s?" file))
    (find-file file)))

(defun buffer-defuns-to-markdown (buffer file)
  "Create markdown for the defuns in BUFFER and save to FILE."
  (interactive "bSelect Buffer: \nFWrite List of defuns to Markdown File: ")
  (f-write (generate-markdown-page-of-buffer-defuns (get-buffer buffer)) 'utf-8 file)
  (when (y-or-n-p (format "Open %s?" file))
    (find-file file)))
