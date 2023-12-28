;;; ocodo-custom-bindings.el -*- lexical-binding: t; -*-

(defvar ocodo-key-binding-groups '(("Markdown Soma" 1 "Markdown soma")
                                   ("Smart Parens" 1 "Sp ")
                                   ("Text Transforms" 0 "C-c t t")
                                   ("Auto Snippet" 1 "Aya ")
                                   ("Ruby" 1 "Ruby ")
                                   ("Swiper" 1 "Swiper ")
                                   ("Git" 0 "C-x v ")
                                   ("Color" 1 "Color ")
                                   ("Dired" 1 "Dired ")
                                   ("ERT Testing" 1 "Ert ")
                                   ("Debugging" 1 "Debug ")
                                   ("Windows" 1 "Window "))
  "Key binding group filters.")

(defvar ocodo-key-bindings-lisp-files
  (-concat
   `(,(expand-file-name "~/.doom.d/key-bindings.el"))
   (f-entries "~/.doom.d/use/"
              (lambda (file)
                 (let1 text
                       (f-read file 'utf-8)
                   (s-contains? "bind-key" text)))))
  "List of emacs-lisp files which have personalised key bindings.")

(defvar ocodo-key-bindings-heading
  "Ocodo's Emacs Key Bindings."
  "Key bindings page heading.")

(defvar ocodo-key-bindings-table-heading (concat
                                          "| Key(s)  | Command | keymap  |\n"
                                          "|:--------|:--------|--------:|")
  "Markdown table heading for key binding documentation.")

(defun ocodo-make-key-binding-table-row (binding)
  "Make a markdown table row from BINDING."
  (cl-destructuring-bind
      (keys command keymap)
      binding
   (format "| <kbd>%s</kbd> | %s | %s |" keys command keymap)))

(defun ocodo-filter-key-bindings (filter index bindings)
  "Filter BINDINGS by FILTER on INDEX."
  (-filter (lambda (it)
             (let ((fn-name (when (and (nth index it) (symbolp (nth index it)))
                                (symbol-name (nth index it)))))
               (when fn-name
                 (s-matches-p filter fn-name))))
           bindings))

(defun ocodo-ungrouped-key-bindings (bindings title groups)
  "Collect BINDINGS and TITLE into GROUPS."
  (let ((predicates (mapcar
                       (lambda (group)
                         (cl-destructuring-bind (_ index filter) group
                           `(lambda (bind)
                              (when (stringp bind) (s-matches-p ,filter (nth ,index bind))))))
                       groups)))
    (list title
      (sort
       (-filter (lambda (b)
                  (--map (funcall it b) predicates))
         bindings)
       (lambda (a b)
         (string> (prin1-to-string a) (prin1-to-string b)))))))

(defun ocodo-make-key-binding-groups (bindings headings groups)
  "Collect BINDINGS and HEADINGS into GROUPS."
  (--map
   (cl-destructuring-bind (title index filter) it
    (list title
      (sort
        (ocodo-filter-key-bindings
          filter index
          bindings)
        #'string>)))
   groups))

(defun ocodo-arrow-key-bindings-use-unicode-arrows (key-binding &optional white-arrows)
  "KEY-BINDING string directions to unicode arrows.
<up> <down> <left> <right> replaced with ↑ ↓ ← →.
<return> replaced with ⮐.

Setting WHITE-ARROWS to t, gives these replacements: ⇧ ⇩ ⇦ ⇨ and ⏎."

 (let* ((key-to-arrow '(("<return>" ("⏎" . "⮐"))
                        ("<up>"     ("⇧" . "↑"))
                        ("<down>"   ("⇩" . "↓"))
                        ("<left>"   ("⇦" . "←"))
                        ("<right>"  ("⇨" . "→"))))
        (fn (or (and white-arrows 'cdar) 'cddr))
        (keys (mapcar (lambda (it) (car it)) key-to-arrow)))
   (if (member key-binding keys)
       (cl-reduce (lambda (text it)
                    (s-replace (car it) (funcall fn it) text))
           key-binding)
       key-binding)))

(defun ocodo-key-bindings-for-documentation ()
  "Cleaned list of key bindings for documentation."
  (ocodo-clean-key-bindings-for-documentation
   (ocodo-collate-key-bindings-for-documentation)))

(defun ocodo-clean-key-bindings-for-documentation (bindings)
  "Prepare collated binding-list for documentation."
  (mapcar (lambda (it)
            (let* ((key-chord (if (car it)
                                  (ocodo-arrow-key-bindings-use-unicode-arrows (car it))
                                ""))
                   (command-fn (nth 1 it))
                   (command-name (if command-fn
                                     (s-capitalized-words (s-replace "#'" "" (prin1-to-string command-fn)))
                                   ""))
                   (key-map (if (and (nth 2 it) (symbolp (nth 2 it))
                                  (s-capitalized-words (format "%s" (symbol-name (nth 2 it)))))
                              "Global")))
              (list key-chord (s-replace "#'" "" (format "%s" command-name)) key-map)))
         (delq nil bindings)))

(defun ocodo-collate-key-bindings-for-documentation ()
  "Collate all key bindings found in ocodo-key-bindings-lisp-files."
   (eval
    (car
     (read-from-string
      (format "'(%s)"
       (s-join
        "\n"
        (--map
         (format
          "( %s )"
          (nth 1 (s-match
                  "[[:space:]]*?(bind-key\\(.*?\\))+$"
                  it)))
         (--filter (s-contains-p "(bind-key " it)
                   (-flatten
                    (--map (s-split "\n" (f-read it 'utf-8))
                     ocodo-key-bindings-lisp-files))))))))))

(defun ocodo-key-binding-groups-to-markdown (binding-groups headings)
  "Convert BINDING-GROUPS to string of markdown tables with HEADINGS."
  (concat
   (format-multiline "|# %s
                      |
                      |%s
                      |"
                     ocodo-key-bindings-heading
                     (lm-commentary "~/.doom.d/key-bindings.el"))

   (s-join "\n"
    (--map
     (cl-destructuring-bind (title bindings) it
       (format-multiline "|
                          |### %s
                          |
                          |%s
                          |%s"
          title
          headings
          (s-join "\n"
           (--map
            (ocodo-make-key-binding-table-row it)
            bindings))))
     (push
      (ocodo-ungrouped-key-bindings (ocodo-key-bindings-for-documentation)
        "General" ocodo-key-binding-groups)
      binding-groups)))))

(defun ocodo-custom-key-bindings-markdown (file)
  "Generate markdown FILE with table of custom bindings."
  (interactive "f[Cusom Bindings] Save to markdown file: ")
  (let* ((table-heading ocodo-key-bindings-table-heading)

         (binding-list (ocodo-key-bindings-for-documentation))

         (custom-key-bindings-markdown (ocodo-key-binding-groups-to-markdown
                                        (ocodo-make-key-binding-groups binding-list table-heading ocodo-key-binding-groups)
                                        table-heading)))

    (f-write custom-key-bindings-markdown 'utf-8 file)
    (message ": %s" file)
    (when (y-or-n-p (format "Generated %s, open it?" file)) (find-file file))))
