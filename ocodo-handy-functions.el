;;; ocodo-handy-functions --- a collection of functions that didn't get organized
;;; Author: Jason Milkins <jasonm23@gmail.com>
;;; Commentary:
;;
;; A collection of miscellaneous functions and macros, which are either
;; candidates to migrate to a minor mode, or will languish here in perpetuity.
;;
;; Peppered in here are a few gems, some redundancies and some stuff I was just
;; playing with. They are auto-documented in this markdown document.
;;
;; Items used often:...
;;
;; - `current-buffer-defuns-to-markdown' (which generated this page.)
;; - `defun-pcase'
;; - `plist-bind'
;; - `*-and-replace'
;; - `screencapture-mac'
;; - `ocodo-custom-key-bindings-to-markdown'
;; - `format-multiline'
;;
;;; License:
;;  GPL3
;;
;;; Code:

(require 'cl-lib)
(require 'cua-base)
(require 'cua-rect)
(require 'dash)
(require 'doom)
(require 'f)
(require 'find-func)
(require 'kurecolor)
(require 'kv)
(require 'lambda-line)
(require 'lisp-mnt)
(require 'magit)
(require 'markdown-soma)
(require 'multiple-cursors)
(require 'pcre2el)
(require 'rx)
(require 'sh-script)
(require 'straight)
(require 'subr-x)
(require 'time-stamp)
(require 'xr)
(require 'yasnippet)

;; Declare used global vars
(defvar recentf-list)
(defvar smie-config)
(defvar sh-styles-alist)

(defvar ocodo-github-repos-cache '()
  "Cache list of github repos.")

(defvar ocodo-github-orgs (list
                           "ocodo"
                           "emacsfodder"
                           "codefodder"
                           "osxfodder"
                           "getclacking"
                           "emacsgifs"
                           "gofodder"
                           "cutbox"
                           "crystal-castles"
                           "nms-shoppinglist")
  "List of my active github orgs.")

(defmacro let1 (var val &rest body)
  "Syntax sugar for LET. A single VAR VAL let over BODY.

Example usage:

```lisp
 (let1 my-var \"Hello World\"
   (message \"%s\" my-var))

 => \"Hello World\"
```"
  `(let ((,var ,val)) ,@body))

(defmacro plist-bind (args expr &rest body)
  "Destructure PLIST, ARGS (keys) of EXPR (a plist) are available in BODY.

For example:

```lisp
 (plist-bind (a c)                ;; <- arg names match key names.
  '(:a \"foo\" :b 13 :c \"bar\") ;; <- plist
  (list a c))                    ;; <- Body

;; => (\"foo\" \"bar\")
```"
  `(cl-destructuring-bind
       (&key ,@args &allow-other-keys)
       ,expr
     ,@body))

(defun -sample (list)
  "Return a random element from the LIST."
  (nth (random (length list)) list))

(defmacro *-and-replace (name evaluator)
 "Create a command NAME which replace region with result of EVALUATOR.

For example:

Using `shell-command-to-string', we can make a replace-region
command with `*-and-replace'

```lisp
 (*-and-replace shell-command-eval-and-replace #'shell-command-to-string)

;; =>
;; (shell-command-eval-and-replace)
```"
 `(defun ,name ()
    (interactive)
    (if (not (region-active-p))
        (replace-thing-at-point-with ,evaluator)

      (replace-region-with ,evaluator))))

(*-and-replace calc-eval-replace-at-region-or-point #'calc-eval)
(*-and-replace decimal-to-hex-at-point-or-region #'decimal-to-hex)
(*-and-replace eval-regexp-to-rx-replace #'xr)
(*-and-replace hex-to-decimal-at-point-or-region #'hex-to-decimal)
(*-and-replace replace-md-code-with-docstring-arg-in-region #'md-code-to-docstring-arg)
(*-and-replace time-to-seconds-at-point-or-region #'time-to-seconds)

(*-and-replace markdown-literate-wrap-exec-code
               #'(lambda (region) (format-multiline "|``` @code
                                                |%s
                                                |```" region)))


(defmacro defun-pcase (name arglist &optional docstring &rest body)
 "Define a pcase function called NAME with ARGLIST.

While `&optional' all `defun-pcase' should have a DOCSTRING.

BODY is the form of the underlying `pcase-lambda'.

These are very useful for certain destructuring / cherry picking
operations on lists / trees.

For example:

```lisp
 (defun-pcase pick-it (`(,_ ,_ (,_ ,it ,_)))
    \"Select it\"
   (format \"%s\" it))

 (my-pfun '(1 2 (1 \"this one\" 3)))
;; => \"this one\"
```"
 (declare (doc-string 3) (indent 2))
 `(progn (defalias
           (quote ,name)
           (pcase-lambda ,arglist ,@body)
           ,docstring)))

(defvar screencapture-mac-default-commandline nil
  "Default command line with options.")

(defvar screencapture-mac-default-file-location
  (expand-file-name "~/Desktop/")
  "Default location to save screen captures.")

(defvar screencapture-mac-default-file-keyword
  "screencapture")

(defun align-number-right (begin end)
  "Align columns of numbers right in the region (BEGIN, END).

For example:

```
10 49 1123
301 213 4111
2134 4151 525235
48912 522  19538

;; =>
;;     10    49     1123
;;    301   213     4111
;;   2134  4151   525235
;;  48912   522    19538
```"
  (interactive "r")
  (align-regexp begin end ".* \\([0-9]+\\).*" -1 1 nil))

(defun buffer-file-name-to-kill-ring ()
  "Save the buffer file name to the kill ring."
  (interactive)
  (when (buffer-file-name)
    (kill-new (buffer-file-name))))

(defun change-number-at-point (func)
  "Change the number at point using FUNC.

It should be wrapped in an interactive function, and func should
take a single numeric argument and return anything.

For example:

```lisp
 (defun round-number-at-point ()
  \"Round the number at point.\"
  (interactive)
  (change-number-at-point #'round))

;; Or...

 (defun number-at-point-to-currency ()
  \"Change the number at point to currency.\"
   (format \"$%.2f\" (number-at-point))))
```"
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (progn
        (forward-word)
        (search-backward (number-to-string number))
        (replace-match (number-to-string (funcall func number)))
        (goto-char point)))))

(defun chmod-this-file (mode)
  "Change the permissions of the current buffer's file to MODE.
MODE is a string representing the new permissions, e.g. \"755\"."
  (interactive "sEnter new permissions (e.g. 755): ")
  (when buffer-file-name
    (let ((filename (buffer-file-name)))
      (shell-command (concat "chmod " mode " " filename))
      (message "Changed permissions of %s to %s." filename mode))))

(defun chmod-executable-this-file ()
  "Change the permissions of the current buffer's file to make it executable."
  (interactive)
  (when buffer-file-name
    (let ((filename (buffer-file-name)))
      (shell-command (concat "chmod +x " filename))
      (message "Changed permissions of %s to executable." filename))))

(defun cleanup-buffer ()
  "Cleanup buffer, tabs to spaces, re-indent, trim whitespace."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun clear-buffer-text-properties ()
  "Clear all text face properties in the buffer.
This is somewhat useful when dealing with text pasted from a
propertied buffer.

Note: this won't turn off face properties in a font-locked buffer."
  (interactive)
  (remove-text-properties 1 (point-max) '(face nil)))

(defun comment-or-uncomment-current-line-or-region ()
  "Comments or uncomments the current line or all the lines in region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (region-active-p)
          (setq min (region-beginning) max (region-end))
        (setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position))))))

(defun copy-region-or-rest-of-line-to-other-window ()
  "Copy the current region to the other window."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (copy-rest-of-line))
  (other-window 1)
  (yank))

(defun copy-rest-of-line ()
  "Copy from cursor to end the current line to the kill ring."
  (interactive)
  (save-mark-and-excursion
    (cua-set-mark)
    (move-end-of-line 1)
    (kill-ring-save nil nil t)))

(defun copy-whole-line ()
  "Copy the current line to the kill ring."
  (interactive)
  (save-mark-and-excursion
    (move-beginning-of-line 1)
    (cua-set-mark)
    (move-end-of-line 1)
    (kill-ring-save nil nil t)))

(defun csv-to-lists (csv)
  "Convert simple (very limited) CSV string to list of lists.

Consider this a basic experiment, which won't be developed.

Use `csv-mode` instead.

For example:

```lisp
 (let1 csv (format-multiline
            \"|1, 2, 3, Words like this, #ffeeff
            |2, 41, 414, 2002, Foo Bar\")
  (csv-to-lists csv))

;; => ((\"1\" \"2\" \"3\" \"Words like this\" \"#ffeeff\")
;;     (\"2\" \"41\" \"414\" \"2002\" \"Foo Bar\"))
```"
 (mapcar (lambda (line) (split-string line ","))
         (split-string (s-chomp csv) "\n")))

(require 'which-key)

(defun cua-rectangle-which-key-help ()
  "Display cua-rectangle-keymap in which-key."
  (interactive)
  (which-key-show-keymap 'cua--rectangle-keymap
           cua--rectangle-keymap))

(defun decimal-to-hex (num)
  "Convert NUM to hex."
  (format "%X" (string-to-number num)))

(defun decrease-default-font-height (m)
  "Adjust the default font :height by 10 (prefix arg M for multiples)."
  (interactive "p")
  (increase-default-font-height -1))

(defun decrement-number-at-point ()
  "Decrement number at point like vim's Ctrl x."
  (interactive)
  (change-number-at-point '1-))

(defun delete-frame-or-window-dwim ()
  "Delete the current frame or buffer.
When there is only one frame, kill the buffer."
  (interactive)
  (if (> 1 (length (frame-list)))
      (delete-frame)
    (kill-buffer)))

(defun delete-this-buffer-and-file (force)
  "Delete kill file and buffer, prefix arg FORCE."
  (interactive "P")
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "'%s' is not a file buffer" name)
      (when (or force (yes-or-no-p (format  "Delete '%s', Are you sure? " filename)))
        (delete-file filename)
        (kill-buffer buffer)
        (message "Deleted '%s'" filename)))))

(defun describe-thing-at-point ()
  "Describe the function or variable at point."
  (interactive)
  (let* ((thing (symbol-at-point)))
    (cond
     ((fboundp thing) (describe-function thing))
     ((boundp thing) (describe-variable thing)))))

(defun describe-char-here-untouched ()
  "Describe the char at point, but first move the point away.

Move back to point after describing the char."
  (interactive)
  (let ((p (point)))
    (if (= (point-min) p)
        (goto-char (point-max))
      (goto-char (point-min)))
    (describe-char p)
    (goto-char p)))

(defun describe-char-next (prefix)
  "Describe the char next to point (+1).

Universal PREFIX can be used to describe char
at N positons from `point'."
  (interactive "p")
  (let ((p (point)))
    (describe-char (+ p prefix))))

(defun describe-char-previous (prefix)
  "Describe the char previous from point (-1).

Universal PREFIX can be used to describe char
at N positons from `point'."
  (interactive "p")
  (let ((p (point)))
    (describe-char (- p prefix))))

(defun dired-find-file-other-window-and-back ()
  "Open file or directory, focus original window."
  (interactive)
  (find-file-other-window (dired-get-file-for-visit))
  (other-window -1))

(defun dired-menu ()
  "Go to one of the currently open Dired buffers (if there is one)."
  (interactive)
  (let* ((dired-buffers (--map (buffer-name it)
                               (--filter
                                (equal 'dired-mode (with-current-buffer it major-mode))
                                (buffer-list)))))
    (if dired-buffers
        (switch-to-buffer (completing-read "Select dired: " dired-buffers))
      (message "There's no dired buffers open right now"))))

(defun dired-osx-open-this-file ()
  "Use the OSX `open' command to launch the current Dired file at point."
  (interactive)
  (shell-command-to-string (format "open %S" (dired-file-name-at-point))))

(defun dired-visit-library (libraryname)
  "Open directory with Dired which contain the given LIBRARYNAME."
  (interactive "M")
  (dired (file-name-as-directory
          (file-name-directory (find-library-name libraryname)))))

(defun duplicate-current-line-or-region (arg &optional up)
  ;; Originally swiped from rejeep's emacs.d rejeep-defuns.el.
  "Duplicates the current line or region ARG times.

If UP is non-nil, duplicate and move point to the top."
  (interactive "p")
  (let (beg
        end
        (origin (point))
        (saved-region
         (when (use-region-p) (list (region-beginning) (region-end)))))
    (if (and (use-region-p) (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if (use-region-p)
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (if up
          (goto-char origin)
        (goto-char (+ origin (* (length region) arg) arg)))
      (when saved-region
        (if up
            (progn (message "setting region (up)")
                   (push-mark-command nil)
                   (goto-char (nth 1 saved-region))
                   (exchange-point-and-mark))
          (progn (message "setting region")
                 (push-mark-command nil)
                 (goto-char (- (point) (length region)))))
        (setq deactivate-mark nil)))))

(defun duplicate-current-line-or-region-up (arg)
  "Duplicates the current line or region up ARG times."
  (interactive "p")
  (duplicate-current-line-or-region arg t))

(defun eval-and-replace ()
  "Replace the preceding sexp with its result."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (insert (format "%s" (eval (read (current-kill 0)))))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun eval-and-replace-prin1 ()
  "Replace the preceding sexp with its value using prin1."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun filter-recentf (pattern)
  "Remove entries matching PATTERN from recent files.
This is operating on the `recentf-list', in memory.
Use `recentf-save-list' to persist."
  (interactive "MRemove recentf entries mathing pattern: ")
  (let ((temporary-recentf-list
         (--reject (s-matches-p pattern it) recentf-list)))
    (setq recentf-list temporary-recentf-list)))

(defun flush-blank-lines ()
  "Flush blank lines."
  (interactive)
  (flush-lines "^\s*$" nil nil t))

(defun format-binary (val &optional width)
  "Convert VAL of WIDTH to a binary string.
&optional WIDTH will default to 8."
  (let* ((w (or width 8))
         (binary (int-to-binary-string val)))
    (message "Width: %d" w)
    (s-pad-left w "0" binary)))

(defun format-multiline (format-string &rest args)
  "Format a multiline indented FORMAT-STRING with ARGS.

A multiline string can use leading `|` (pipe) characters to line
up indentation.

ARGS passed will populate format template tokens in the
FORMAT-STRING. Tokens are as defined in `(format ...)`

For example:

```
 (fomat-multiline \"|- List...
                   |  - Item %s
                   |  - Item %#x
                   |  - Item %x
                   |
                   |... %s\"
   \"one\" 2 #xf \"the end\")

 =>
 \"- List...
   - Item one
   - Item 0x2
   - Item f

 ... the end\"
```"
  (apply 'format
         (s-join "\n" (--map (s-replace-regexp "^\s*|" "" it) (s-lines format-string)))
         args))

(defun duplicate-sexp (arg)
  "Duplicate sexp, follows the ARG rules of `kill-sexp'."
  (interactive "p")
  (kill-sexp arg)
  (yank)
  (yank))

(defun kill-save-sexp (arg)
  "Save sexp to `kill-ring', follows the ARG rules of `kill-sexp'."
  (interactive "p")
  (kill-sexp arg)
  (yank))

(defun format-thousands-separators (n)
  "Format N to have thousand separators.

For example:

```lisp
 (format-thousands-separators 3032498000)
 ;; => \"3,032,498,000\"
```"
  (let* ((parts (split-string (number-to-string n) "[.]"))
         (characteristic (car parts))
         (separated
          (reverse
           (string-join (--reject (string= it "")
                                  (split-string (replace-regexp-in-string
                                                 "[0-9]\\{3\\}"
                                                 "\\& "
                                                 (reverse characteristic))
                                                " "))
                        ","))))
    (if (eq (length parts) 1)
        separated
      (format "%s.%s" separated (nth 1 parts)))))

(defun fraction-radian (denominator)
  "Fraction DENOMINATOR of circle to radians."
  (interactive "nDenomiator:")
  (insert (format "%s" (/ (* float-pi 2) denominator))))

(defun generate-untitled-name ()
  "Generate a name with pattern untitled-n."
  (let ((n 1))
    (while
        (member (format "untitled-%i" n)
                (mapcar
                 (lambda (it) (buffer-name it))
                 (buffer-list)))

      (setq n (+ n 1)))
    (format "untitled-%i" n)))

(defun get-osx-display-resolution ()
  "Get the current display resolution in OSX.

Uses the mac system_profiler `SPDisplaysDataType' to lookup the
current display resolution. This is then filtered out (using grep
& perl) and formattted to a list of `(w h)'.

For example:

```lisp
 (get-osx-display-resolution)
 ;; => (\"3840\" \"2160\")
```"
  (s-split "x"
   (s-chomp
    (shell-command-to-string
        "system_profiler SPDisplaysDataType |\
                            grep Resolution |\
                            perl -pe \
                            's/^ *Resolution: ([0-9]+? x [0-9]+?) .*$/\\1/' |\
                            tr -d ' '"))))

(defun get-position-of-nearest-matching (s &optional arg)
  "Get the position of nearest S.

optional ARG when less than zero, default to the before match
when matches are equidistant from the current point."
  (let* ((after        (- (save-excursion (search-forward s)) (length s)))
         (before       (save-excursion (search-backward s)))
         (dist-after   (- after  (point)))
         (dist-before  (- (point) before)))
    (if (eq dist-after dist-before)
        (if (and arg (>= 0 arg)) after before)
      (if (< dist-after dist-before)
          after
        before))))

(defun get-position-of-nearest-regexp-match (regexp &optional arg)
  "Get the position of nearest REGEXP match.

optional ARG when less than zero, default to the before match
when matches are equidistant from the current point."
  (let* ((after        (save-excursion (search-forward-regexp regexp)))
         (before       (save-excursion (search-backward-regexp regexp)))
         (dist-after   (- after  (point)))
         (dist-before  (- (point) before)))
    (if (eq dist-after dist-before)
        (if (and arg (>= 0 arg)) after before)
      (if (< dist-after dist-before)
          after
        before))))

(defun git-open-changed-and-new-files ()
  "Use git ls-files to open changed files."
  (interactive)
  (let ((git-modified-files (shell-command-to-string "git ls-files -m --others --exclude-standard")))
    (if (not (string-match "^fatal: Not a git repo" git-modified-files))
        (let ((file-list (split-string git-modified-files "\n" t "[\r\n\t ]")))
          (mapc (lambda (file) (find-file file)) file-list))
      (user-error "Not in a git repository"))))

(defun git-open-changed-files ()
  "Use git ls-files to open changed files."
  (interactive)
  (let ((git-modified-files (shell-command-to-string "git ls-files -m")))
    (if (not (string-match "^fatal: Not a git repo" git-modified-files))
        (let ((file-list (split-string git-modified-files "\n" t "[\r\n\t ]")))
          (mapc (lambda (file) (find-file file)) file-list))
      (user-error "Not in a git repository"))))

(defun git-open-from-ls-files (git-ls-options)
  "Use GIT-LS-OPTIONS to open changed files."
  (interactive "sGit ls-files options: ")
  (let ((git-modified-files (shell-command-to-string (format "git ls-files %s" git-ls-options))))
    (if (not (string-match "^fatal: Not a git repo" git-modified-files))
        (let ((file-list (split-string git-modified-files "\n" t "[\r\n\t ]")))
          (mapc (lambda (file) (find-file file)) file-list))
      (user-error "Not in a git repository"))))

(defun git-open-ls-files (git-ls-options)
  "Use GIT-LS-OPTIONS to open changed files."
  (interactive (list
                (completing-read "Open changed files: " '("--modified"
                                                          "--other --exclude-standard"
                                                          "--other --exclude-standard --modified"
                                                          "--unmerged"
                                                          "--ignored"))))
  (let ((git-modified-files (shell-command-to-string (format "git ls-files %s" git-ls-options))))
    (if (not (string-match "^fatal: Not a git repo" git-modified-files))
        (let ((file-list (split-string git-modified-files "\n" t "[\r\n\t ]")))
          (if (> 0 (length file-list))
              (mapc (lambda (file) (find-file file)) file-list)
            (message "No files to open")))
      (user-error "Not in a git repository"))))

(defun git-open-untracked-files ()
  "Use git ls-files to open untracked files.

    Open any untracked file in the repo (unless it's been .gitignored)"
  (interactive)
  (let ((git-untracked-files (shell-command-to-string "git ls-files --others --exclude-standard")))
    (if (not (string-match "^fatal: Not a git repo" git-untracked-files))
        (let ((file-list (split-string git-untracked-files "\n" t "[\r\n\t ]")))
          (mapc (lambda (file) (find-file file)) file-list))
      (user-error "Not in a git repository"))))

(defun git-in-repo-p (dir)
  "True if DIR is in a git repo."
  (not (string-match "^fatal: Not a git repo " (shell-command-to-string "git ls-files"))))

(defun git-delete-file-from-cache (filename)
  "Git rm --cache FILENAME."
  (interactive "f")
  (shell-command (format "git rm --cache %s" filename)))

(defun ocodo-github-repos ()
  "List github repos using gh cli and `ocodo-github-orgs'."
  (unless ocodo-github-repos-cache
    (setq ocodo-github-repos-cache
     (s-split "\n"
      (s-join ""
       (--map
        (shell-command-to-string
         (format "gh repo list %s --json nameWithOwner -t '{{range .}}{{tablerow .nameWithOwner}}{{end}}'" it))
        ocodo-github-orgs))
      t)))
  ocodo-github-repos-cache)

(defun ocodo-github-repos-refresh ()
  "Refresh repos cache."
  (interactive)
  (setq ocodo-github-repos-cache nil)
  (ocodo-github-repos))

(defun ocodo/gist-create ()
  "Comments or uncomments the current line or all the lines in region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (region-active-p)
          (setq min (region-beginning) max (region-end))
        (setq min (point-min) max (point-max)))
      (shell-command-on-region min max
                               (format "gh gist create --desc '%s' --filename '%s' %s"
                                       (string-replace "'" "\\'" (read-string "Gist Description: " nil nil "TODO Description"))
                                       (read-string "Gist Filename: " (file-name-nondirectory (buffer-file-name)) nil "untitled")
                                       (when (y-or-n-p "Public Gist ?") "--public"))))))

(defun ocodo/goals-create-github-issues (goals-markdown-file)
  "Read GOALS-MARKDOWN-FILE and create issues from its top level tasks.

Sub-tasks are added to the issue body."
  (interactive "fSelect goals.md: ")
  (shell-command "gh "))

(defun ocodo-open-project (project)
  "Open a PROJECT in workspace or from github."
  (interactive (list (completing-read "Select a project: " (ocodo-project-list))))
  (if (string-match-p "git@github\\.com:" project)
      (ocodo-open-project-repo-locally project)
    (ocodo-open-local-project project)))

(defun ocodo-open-project-repo-locally (project)
  "Open a PROJECT repo locally.
Clone if not already in workspace."
  (let ((path (ocodo-find-local-repo project)))
    (if path
        (ocodo-open-local-project path)
      (progn
        (let ((path (format "~/workspace/%s" (f-filename project))))
         (if (f-exists-p path)
             (error "%s already exists" path)
          (progn
            (ocodo-clone-project project path)
            (ocodo-open-local-project path))))))))

(defun ocodo-clone-project (project path)
  "Clone PROJECT (git ssh url) to PATH."
  (message
   (shell-command-to-string (format "git clone %s %s" project path))))

(defun ocodo/bump-version-patch ()
  "Search for version = and increment the patch number of the string

Editor macro."
  (interactive)
  (goto-char 0)
  (search-forward "version =")
  (end-of-line)
  (search-backward ".")
  (insert " ")
  (forward-char 1)
  (increment-number-at-point)
  (search-backward ".")
  (delete-horizontal-space))

(defun ocodo/get-filename-size ()
  "Get the size of filename at the current line.
Only works with lines which contain a filename."
  (interactive)
  (let* ((name (s-chomp (thing-at-point 'line)))
         (size (f-size name)))
     (message "%s"
              (file-size-human-readable (f-size name)))))

(defun ocodo/mac-open-filename ()
  "Open the filename at the current line.
Only works with lines which contain a filename."
  (interactive)
  (let* ((name (s-chomp (thing-at-point 'line))))
    (message (shell-command-to-string (format "open \"%s\"" name)))))

(defun ocodo/git-remote-url (&optional directory remote)
  "Return the url of the git REMOTE in DIRECTORY.
Return nil if no remote or not a git repo.

DIRECTORY and REMOTE are optional.
Current dir and origin will be used by default."
  (let ((directory (if directory (format "-C %s" directory) ""))
        (remote (or remote "origin")))
    (s-chomp (shell-command-to-string (format "git %s remote get-url %s" directory remote)))))

(defun ocodo-find-local-repo (project)
  "Return the path of PROJECT with repo url.
Look for name matches in workspace, Check git remote for a match."
  (let ((workspace-folders (cl-copy-list (ocodo-local-project-list)))
        (found nil))
    (while (and (null found) workspace-folders)
     (let* ((d (car workspace-folders))
            (url (ocodo/git-remote-url d)))
       (when (string= url project) d (setq found d))
       (setq workspace-folders (cdr workspace-folders))))
    found))

(defun ocodo-project-list ()
  "List of project names from workspace and github repos."
  (append
   (mapcar
    (lambda (p) (format "git@github.com:%s.git" p))
    (ocodo-github-repos))
   (ocodo-local-project-list)))

(defun ocodo-local-project-list ()
  "List of project names from workspace."
  (f-directories "~/workspace"))

(defun ocodo-open-local-project (project)
  "Open a local PROJECT."
  (if (f-exists-p project)
      (find-file project)
    (error "%s was not found locally" project)))

(defun github-browse-current-repo (repo)
  "Browse the current github REPO.
If the user is not in a repo, Select from `ocodo-github-repos'."
  (interactive (list
                (if (git-in-repo-p (pwd))
                    (s-chomp (shell-command-to-string "gh repo view --json nameWithOwner -t '{{tablerow .nameWithOwner}}'"))
                  (completing-read "Github Repo [format: user/repo]: " (ocodo-github-repos) nil nil))))
  (browse-url (format "https://github.com/%s" repo)))

(defun github-browse-repo (repo)
  "Browse the github REPO. Select from `ocodo-github-repos'."
  (interactive (list (completing-read "Github Repo [format: user/repo]: " (ocodo-github-repos) nil nil)))
  (browse-url (format "https://github.com/%s" repo)))

(defun google-en-to-thai (text)
 "Translate TEXT from English to Thai."
  (let* ((response-json
                 (shell-command-to-string
                  (format "curl -s \"https://translate.googleapis.com/translate_a/single?client=gtx&sl=en&tl=th&dt=t&q=%s\""
                   (url-hexify-string text))))
         (translation (replace-regexp-in-string "\\[+\"\\(.*?\\)\".*$" "\\1" response-json)))
   translation))

(defun google-en-to-thai-on-region (begin end)
 "Translate english in region (BEGIN END) to Thai."
   (interactive "r")
   (let* ((text (buffer-substring begin end))
          (translated (google-en-to-thai text)))
    (message translated)))
  
(defun hex-to-decimal (num)
  "Convert hex NUM to decimal."
  (format "%i" (string-to-number num 16)))

(defun increase-default-font-height (prefix)
  "Adjust the default font :height by 10, PREFIX (to set by multiples)."
  (interactive "p")
  (let ((new-height (+ (* prefix 10) (face-attribute 'default :height))))
    (set-face-attribute 'default nil :height new-height)
    (message "Default font height set to %i" new-height)))

(defun increment-number-at-point ()
  "Increment number at point like vim's Ctrl a."
  (interactive)
  (change-number-at-point '1+))

(defun increment-number-binary (&optional arg)
  "Increment the number forward from point by ARG."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "01")
        (when (re-search-forward "[0-1]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 2) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 2 field-width) answer)))
          (replace-match (format-binary answer field-width)))))))

(defun indent-buffer ()
  "Indent the current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun insert-buffer-base-filename ()
  "Insert the base filename for the current buffer.

If your're in the minibuffer it will use the other buffer file name."
  (interactive)
  (let ((filename (buffer-file-name (if (window-minibuffer-p)
                                        (window-buffer (previous-window))
                                      (current-buffer)))))
    (when filename (insert-kill (file-name-base filename)))))

(defun insert-buffer-filename ()
  "Insert the filename for the current buffer.

If your're in the minibuffer it will use the other buffer file name."
  (interactive)
  (let ((filename (buffer-file-name (if (window-minibuffer-p)
                                        (window-buffer (previous-window))
                                      (current-buffer)))))
    (when filename (insert-kill filename))))

(defun insert-iso8601-date (&optional date)
  "Insert DATE."
  (interactive)
  (insert (format-time-string "%Y-%m-%d" date)))

(defun insert-kill (string)
  "Insert STRING and copy to the kill ring."
  (interactive)
  (kill-new string)
  (insert string))

(defun insert-random-in-range (start end)
  "Insert a random number within the range of START and END."
  (interactive "nRange start: \nnRange end: ")
  (insert (format "%i" (random-in-range start end))))

(defun insert-random-radian ()
  "Insert a radian value from 0 to 6.28318 (2PI : 360 deg)."
  (interactive)
  (insert (format "%s" (* (/ float-pi 180) (random 361)))))

(defun insert-sample (strings)
  "Insert a random item from a list of STRINGS."
  (interactive "sList of strings separated by spaces: ")
  (insert (-sample (s-split " " strings))))

(defun insert-time-now ()
  "Insert current time."
  (interactive)
  (insert (return-time-now)))

(defun return-time-now ()
  "Return now as a format-time-string."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun int-to-binary-string (int)
  "Convert an INT into it's binary representation in string format."
  (let ((res ""))
    (while (not (= int 0))
      (setq res (concat (if (= 1 (logand int 1)) "1" "0") res))
      (setq int (ash int -1)))
    (if (string= res "")
        (setq res "0"))
    res))

(defun is-markdown-filename-p (filename)
  "Is the FILENAME markdown."
  (s-matches-p "^.*[.]\\(md\\|markdown\\)$" filename))

(defun join-lines-in-sexp ()
  "Join all line of the sexp infront of point."
  (interactive)
  (mark-sexp)
  (join-line-or-lines-in-region))

(defun join-line-from-below ()
  "Join line from below."
  (interactive)
  (forward-line 1)
  (delete-indentation))

(defun join-line-or-lines-in-region ()
  "Join this line or the lines in the selected region."
  (interactive)
  (cond ((region-active-p)
         (let ((min (line-number-at-pos (region-beginning))))
           (goto-char (region-end))
           (while (> (line-number-at-pos) min)
             (join-line))))
        (t (call-interactively 'join-line))))

(defun jump-to-message-buffer ()
  "Jump to the *Messages* buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))


(defun kill-untitled-buffers ()
  "Kill untitled buffers."
  (interactive)
  (mapcar (lambda (n)
            (message "Buffer: %s" (buffer-name n))
            (when (string-prefix-p "untitled-" (buffer-name n))
              (if (buffer-modified-p n)
                  (when
                      (y-or-n-p
                       (format "Kill buffer: '%s' (modified)? "
                               (buffer-name n)))
                    (kill-buffer n)
                    (message "  > Kill %s (modified)" (buffer-name n)))
                (message "  > Kill %s" (buffer-name n))
                (kill-buffer n))))
          (buffer-list)))

(defun kill-whole-word ()
  "Kill the current word at point."
  (interactive)
  (unless (looking-back "[\n\t. {\[(\"]" 1)
    (backward-word))
  (kill-word 1))

(defun macos-get-list-of-windowids ()
  "Get a list of macOS windowids."
  (--map
   (cl-destructuring-bind (&optional windowid layer app window-name)
       (eval (car (read-from-string (format "'(%s)" it))))
     `(,windowid ,app ,window-name))
   (--reject (string-blank-p it)
             (s-split "\n"
                      (shell-command-to-string  "~/.doom.d/bin/wlist")))))

(defun macos-get-window-id-of (app)
  "Get the windowid of APP."
  (shell-command-to-string
   (format "~/.doom.d/bin/wlist | grep '%s' | grep -E -o '^[0-9]*' " app)))

(defun macos-get-window-id-of-app (app)
  "Get the windowid of APP."
  (interactive "MApp name: ")
  (message "%s"
           (shell-command-to-string
            (format "~/.doom.d/bin/wlist | grep '%s' | grep -E -o '^[0-9]*' " app))))

(defun magit-just-amend ()
  "Just git commit --amend."
  (interactive)
  (save-window-excursion
    (shell-command "git --no-pager commit --amend --reuse-message=HEAD")
    (magit-refresh)))

(defun make-kurecolor-24bit-hue-table (color)
  "Make a 24bit COLOR table using Kurecolor."
  (interactive)
  (cl-loop for
           (a b) in

           (cl-loop
            for i from 1 to 30
            for a = (- (* i 12) 11)
            for b = (* i 12)
            collect (list a b))

           do

           (cl-loop for i from a upto b
                    do
                    (insert (format "%s:" (kurecolor-hex-set-hue color (/ i 360.0)))))
           (newline-and-indent)))

(defun make-kurecolor-hue-table ()
  "Make a hue table from hex color at top of kill ring, no error checking."
  (interactive)
  (let ((color (car kill-ring-yank-pointer)))
    (cl-loop for (a b) in '((1 12) (13 24) (25 36))
             do
             (insert ";; ")
             (cl-loop for i from a upto b do
                      (insert (format "%-4s    " (format "%s°" (* i 10)))))
             (newline-and-indent)
             (insert ";;")
             (cl-loop for i from a upto b do
                      (insert (format " %s" (kurecolor-hex-set-hue color (/ (* i 10) 360.0)))))
             (newline-and-indent))))

;; Generated from: #FF0000 (use Rainbow-mode for niceness)
;; 10°     20°     30°     40°     50°     60°     70°     80°     90°     100°    110°    120°
;; #FF2A00 #FF5400 #FF7F00 #FFAA00 #FFD400 #FFFF00 #D4FF00 #AAFF00 #7FFF00 #54FF00 #2AFF00 #00FF00
;; 130°    140°    150°    160°    170°    180°    190°    200°    210°    220°    230°    240°
;; #00FF2A #00FF55 #00FF7F #00FFA9 #00FFD4 #00FFFF #00D4FF #00A9FF #007FFF #0054FF #002AFF #0000FF
;; 250°    260°    270°    280°    290°    300°    310°    320°    330°    340°    350°    360°
;; #2A00FF #5400FF #7F00FF #AA00FF #D400FF #FF00FF #FF00D4 #FF00AA #FF007F #FF0055 #FF002A #FF0000

(defun make-yas-from-region (begin end)
  "Make a yasnippet from the current region BEGIN END.

You should use standard snippet formatting in place, e.g. $1,
${1:default value} and so on.  See the yasnippet docs for more info.

You'll be prompted for a name, trigger key and when `prefix-arg' is
specified, a snippet group."
  (interactive "r")
  (if (region-active-p)
      (progn
        ;; TODO make a new buffer with yas headers
        ;; ask for a name
        (let* ((name (read-from-minibuffer "Name: "))
               (group (if current-prefix-arg
                          (format "\n# group: %s\n" (read-from-minibuffer "Group: "))
                        ""))
               (key (read-from-minibuffer "Key: "))
               (filename (format "%ssnippets/%s/%s" user-emacs-directory major-mode name))
               (snippet (buffer-substring begin end))
               (template (format-multiline "# -*- mode: snippet -*-
                                           |# name: %s%s
                                           |# key: %s
                                           |# --
                                           |%s
                                           |"
                                 name
                                 group
                                 key
                                 snippet)))
          (with-temp-buffer
            (insert template)
            (write-file filename)))
        (yas-reload-all))
      (error "An active region is needed to make a snippet")))

(defun markdown-soma-window-arrangement-start ()
  "Arrange windows for `markdown-soma-start'.

Internally uses the script `~/.doom.d/bin/emacs-markdown-preview-layout.osa'."
  (interactive)
  (when (not markdown-soma-mode)
    (shell-command "~/.doom.d/bin/emacs-markdown-preview-layout.osa" nil nil)))

(defun ocodo/gh-workflow-names ()
  "List gh workflow names for current project.

Project is defined by git repo."
  (s-lines (shell-command-to-string "gh workflow list | cut -f1")))

;; (defun ocodo/gh-run-list-data (&optional workflow-name)
;;   "Return workflow runs for the current project as a lisp data structure.

;; Filter by WORKFLOW-NAME.

;; Project is defined by git repo."
;;   (let* ((workflow-filter (if workflow-name (format " --workflow '%s' " workflow-name) ""))
;;          (command-string (ocodo/gh-run-list-json-shell-command-string workflow-filter))
;;          (entries-json-string (shell-command-to-string command-string)))
;;        (json-parse-string entries-lisp-string)))

(defun ocodo/gh-run-list-json-shell-command-string (&optional workflow-filter)
  "Return a gh run list command to generate json.
WORKFLOW-FILTER can be a --workflow filter or empty string."
  (format "gh run -R 'cutbox/cutbox' list %s --json number,status,workflowName,headBranch,event,startedAt,url" (or workflow-filter "")))

(defun ocodo/gh-run-list-hash-to-tblui-vector-list (data)
  "Convert DATA to a tblui ready vector list"
  (let* ((result '())
         (key-names '("status"
                      "url"
                      "event"
                      "workflowName"
                      "startedAt")))
    (dotimes (i (length data))
      (let* ((hash (aref data i))
             (values (mapcar
                      (lambda (key)
                        (gethash key hash))
                      key-names)))
        (push (list i (apply 'vector values)) result)))
    result))

(tblui-define ocodo/gh-run-list
              ocodo/gh-run-list-entries-provider
              [("status" 10 nil)
               ("url" 45 nil)
               ("event" 8 nil)
               ("workflowName" 16 nil)
               ("startedAt" 6 nil)] ())

(defun ocodo/gh-run-list-entries-provider (&optional workflow-name)
  "List workflow runs for the current project as a tblui view.
Filter by WORKFLOW-NAME.

Project is defined by pwd/git repo."
  (let* ((workflow-filter (if workflow-name
                              (format " --workflow '%s' " workflow-name)
                              "")))
    (ocodo/gh-run-list-hash-to-tblui-vector-list
     (json-parse-string
      (shell-command-to-string
       (ocodo/gh-run-list-json-shell-command-string))))))

(defun ocodo/gh-run-list ()
  "Show the current repo's gh workflow run list."
  (interactive)
  (ocodo/gh-run-list-goto-ui))

(defun ocodo/shell-command-to-insert (command)
  "Execute shell COMMAND and insert the result."
  (interactive (list (read-shell-command "Shell Command (output insert at point): ")))
  (insert (shell-command-to-string command)))

;;; Zoom
;;;
(defun ocodo/default-face-size-decrease ()
  "Decrease the default face size."
  (interactive)
  (ocodo/default-face-size-adjust -10))

(defun ocodo/default-face-size-increase ()
  "Increase the default face size."
  (interactive)
  (ocodo/default-face-size-adjust 10))

(defun ocodo/default-face-size-adjust (amount)
  "Adjust the default face size by AMOUNT."
  (let* ((current (face-attribute 'default :height))
         (size (+ amount current)))
    (message "Resize default face to: %i (delta: %i, current: %i)"
             size amount current)
    (set-face-attribute 'default nil :height size)))

(defun ocodo/default-face-size-reset ()
  "Reset the default face size to 230."
  (interactive)
  (set-face-attribute 'default nil :height 230))


(defun ocodo/kill-ring-save-buffer ()
  "Copy the whole buffer to the kill ring."
  (interactive)
  (kill-ring-save (point-min) (point-max)))

(defun ocodo/yank-replace-buffer ()
  "Yank replace the visible buffer.
The existing buffer text is saved to the kill-ring."
  (interactive)
  (goto-char (point-min))
  (yank)
  (kill-region (point) (point-max))
  (goto-char (point-min)))

(defun ocodo/kill-buffer-text ()
  "Kill the visible buffer text, and save to the kill ring."
  (interactive)
  (kill-region (point-min) (point-max)))

(defvar ocodo/favorite-theme-times ()
  "Recording of theme change times for the session")

(defvar ocodo/favorite-themes
  '("creamsody" "creamsody-dark" "creamsody-darker"
    "darktooth" "darktooth-dark" "darktooth-darker"
    "soothe" "orangey-bits" "cyanometric"))

(defun ocodo/choose-favorite-theme ()
  "Choose from a list of favorite themes."
  (interactive)
  (let ((timestamp (return-time-now))
        (chosen-theme (completing-read "Choose theme:" ocodo/favorite-themes)))
   (add-to-list 'ocodo/favorite-theme-times (list :theme chosen-theme :time timestamp))
   (ocodo/load-theme chosen-theme)))

(defun ocodo/favorite-theme-times-save ()
  "Write the favorite theme times to log."
  (dolist (item ocodo/favorite-theme-times)
    (plist-bind (time theme) item
      (shell-command (format "echo '[%s] %s' >> ~/.emacs-favorite-themes.log" time theme)))))

(add-hook 'kill-emacs-hook 'ocodo/favorite-theme-times-save)

(defun ocodo/match-indent-above ()
  "Indent to match line above, regardless of mode."
  (interactive)
  (let (col (current-column))
   (save-excursion
     (forward-line -1)
     (beginning-of-line-text)
     (setq col (current-column))
     (forward-line 1)
     (beginning-of-line)
     (delete-horizontal-space))
   (indent-to-column col)))

(defun ocodo/maximize-mac-window-aka-frame-via-phoenix ()
  "Maximize the frame using a keyboard shortcut on Phoenix.

Internally uses the script `~/.doom.d/bin/phoenix-maximize-toggle.osa'."
  (interactive)
  (shell-command "~/.doom.d/bin/phoenix-maximize-toggle.osa" nil nil))

(defun mc/cua-rectangle-to-multiple-cursors ()
  "Switch from cua rectangle to multiple cursors."
  (interactive)
  (let ((right (cua--rectangle-right-side))
        rows)
    (cua--rectangle-operation
     'clear nil t nil nil
     (lambda (s e _l _r)
       (setq rows
             (append rows
                     (list (cons (+ 0 s) (+ 0 e)))))))
    (cua--cancel-rectangle)
    (if rows
        (let ((mark-row `(lambda (row)
                           ,@(if right
                                 '((push-mark (car row))
                                   (goto-char (cdr row)))
                               '((push-mark (cdr row))
                                 (goto-char (car row))))
                           (setq transient-mark-mode (cons 'only transient-mark-mode))
                           (activate-mark)
                           (setq deactivate-mark nil)))
              (top (car rows))
              (rest (cdr rows)))
          (cl-loop for row in rest do
                   (mc/save-excursion
                    (funcall mark-row row)
                    (mc/create-fake-cursor-at-point)))
          (funcall mark-row top)
          (mc/maybe-multiple-cursors-mode)))))

(define-key cua--rectangle-keymap (kbd "C-. C-,") 'mc/cua-rectangle-to-multiple-cursors)

(defun md-code-to-docstring-arg (string)
  "Replace markdown inline code with docstring arg style in STRING.

For example:

```lisp
 (md-code-to-docstring-arg \"`code`\")
 ;;  => CODE
```"
  (s-replace-regexp
   (rx "`" (group (>= 1 (any alnum "_" "-"))) "`")
   (lambda (match) (upcase (format "%s" (s-replace "`" "" match))))
   string t))

(defun my-isearch-buffers ()
  "Incremental search through open buffers."
  (interactive)
  (multi-isearch-buffers
   (delq nil (mapcar (lambda (buf)
                       (set-buffer buf)
                       (and (not (equal major-mode 'dired-mode))
                            (not (string-match "^[ *]" (buffer-name buf)))
                            buf))
                     (buffer-list)))))

(defun my-multi-occur-in-matching-buffers (regexp &optional allbufs)
  "Show all lines matching REGEXP in all buffers.
Optionally check ALLBUFS."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp))

(defun new-untitled-buffer ()
  "Open a new buffer called untitled-n."
  (interactive)
  (switch-to-buffer (generate-untitled-name)))

(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapc
   (lambda (buffer)
     (kill-buffer buffer))
   (buffer-list))
  (delete-other-windows))

(defun nuke-all-buffers-execept-current ()
  "Kill all the open buffers except the current one.
Leave *scratch* and *Messages* alone too."
  (interactive)
  (mapc
   (lambda (buffer)
     (unless (or
              (eq (current-buffer) buffer)
              (string= (buffer-name buffer) "*scratch*")
              (string= (buffer-name buffer) "*Messages*"))
       (kill-buffer buffer)))
   (buffer-list))
  (delete-other-windows))

(defun ocodo/doom-upgrade-packages (&optional packages no-confirm)
  "Upgrade PACKAGES, (unattended NO-CONFIRM = t)."
  (interactive)
  (when (ocodo/straight-remove-packages packages no-confirm)
    (doom/reload)))

(defun ocodo/preview-mjml ()
  "Compile the current MJML buffer to HTML and open it in the default browser."
  (interactive)
  (let ((html-file (make-temp-file "mjml-preview-" nil ".html"))
        (temp-file (make-temp-file "mjml-temp-" nil ".mjml")))
    (write-file temp-file)
    (shell-command (format "mjml -r \"%s\" -o \"%s\"" temp-file html-file))
    (browse-url (concat "file://" html-file))))

(defun ocodo/straight-shut-up ()
  "Recompile straigt to shut it up."
  (interactive)
  (byte-compile-file "~/.emacs.d/.local/straight/build-28.1/straight/straight.el")
  (byte-compile-file "~/.emacs.d/.local/straight/build-28.1/straight/straight-x.el")
  (byte-compile-file "~/.emacs.d/.local/straight/build-28.1/straight/straight-autoloads.el")
  (byte-compile-file "~/.emacs.d/.local/straight/build-28.1/straight/straight-ert-print-hack.el"))

(defun ocodo/straight-remove-packages (&optional packages no-confirm)
  "Remove PACKAGES, (unattended NO-CONFIRM = t)."
  (interactive)
  (let* ((straight-absolute-build-dir (format "%s%s%s"
                                              straight-base-dir
                                              "straight/"
                                              straight-build-dir))
         (straight-packages (cddr (directory-files straight-absolute-build-dir nil)))
         (packages (or packages (completing-read-multiple "Select package: " straight-packages)))
         (library-elcs (--map (locate-library it) packages))
         (build-dirs (--map (file-name-directory it) library-elcs))
         (source-els (--map (file-truename (s-replace ".elc" ".el" it)) library-elcs))
         (source-dirs (--map (file-name-directory it) source-els))
         (deletions (-zip packages source-dirs build-dirs))
         (confirmed (if no-confirm
                        t
                      (y-or-n-p
                       (format "%s\nDelete?"
                               (s-join
                                "\n"
                                (--map
                                 (format
                                  "%s [%s]"
                                  (car it)
                                  (s-replace
                                   straight-base-dir ""
                                   (cadr it)))
                                 deletions)))))))
    (if confirmed
        (ocodo/straight--removed-packages deletions)
      nil)))

(defun ocodo/reload-config ()
  "Reload config.el."
  (interactive)
  (load-file (concat doom-user-dir "config.el")))

(defun ocodo/reload-keys ()
  "Reload key-bindings.el."
  (interactive)
  (load-file (concat doom-user-dir "key-bindings.el")))

(defun ocodo/straight--removed-packages (deletions)
  "Internal func perform DELETIONS and display status."
  (let ((buffer (get-buffer-create
                  (generate-new-buffer-name
                   "*ocodo/straight-remove-packages*"))))
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (insert "Removing packages:\n")
      (--each deletions
       (cl-destructuring-bind (package source-dir build-dir) it
         (delete-directory build-dir t t)
         (delete-directory source-dir t t)
         (insert
          (format-multiline
           "%1$s
           |  [%4$s] %5$s: %2$s
           |             : %3$s
           |"
           package build-dir source-dir
           (propertize "x" 'face 'warning)
           (propertize "deleted" 'face 'warning)))))
      (insert (propertize "Done" 'face 'success))
      (read-only-mode)
      deletions)))

(defun ocodo-sh-indent-rules ()
  "Set up shell script indenation rules engines."
  (setq smie-config
        '((sh-mode
           (2 :after "then" 2)
           (0 :before "then" 0)
           (2 :after "then" nil)
           (2 :after "{" 2)
           (2 :after "do" 2)
           (2 :after "else" 2))))

  (setq sh-styles-alist
        '(("ocodo"
           (sh-basic-offset . 4)
           (sh-first-lines-indent . 0)
           (sh-indent-after-case . +)
           (sh-indent-after-do . +)
           (sh-indent-after-done . 0)
           (sh-indent-after-else . +)
           (sh-indent-after-if . +)
           (sh-indent-after-loop-construct . +)
           (sh-indent-after-open . +)
           (sh-indent-comment . t)
           (sh-indent-for-case-alt . +)
           (sh-indent-for-case-label . +)
           (sh-indent-for-continuation . +)
           (sh-indent-for-do . 0)
           (sh-indent-for-done . 0)
           (sh-indent-for-else . 0)
           (sh-indent-for-fi . 0)
           (sh-indent-for-then . 0))))
  (sh-load-style "ocodo"))

(defun open-line-above ()
  "Open a newline above the current point, without moving."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (newline)
    (forward-line -1)))

(defun open-line-below ()
  "Open a newline below the current point, without moving."
  (interactive)
  (end-of-line)
  (newline)
  (back-to-indentation))

(defun open-this-in-intellij-idea-osx ()
  "Open the current file in intellij IDEA (OS X specific)."
  (interactive)
  (when (file-exists-p (buffer-file-name))
    (start-process-shell-command "intellij-idea" nil
                                 (format "idea --line %s %s"
                                         (line-number-at-pos)
                                         (buffer-file-name)))
    (start-process-shell-command "switch-to-intellij" nil
                                 "osascript -e 'activate application \"IntelliJ IDEA\"'")))

(defun open-this-in-xcode ()
  "Open the current file in XCode."
  (interactive)
  (when (file-exists-p (buffer-file-name))
    (start-process-shell-command "open-in-xcode" nil
                                 (format "open -a XCode %s"
                                         (buffer-file-name)))
    (start-process-shell-command "switch-to-xcode" nil
                                 "osascript -e 'activate application \"XCode\"'")))

(defun package-insert-ends-here ()
  "Insert the ELPA package file ending string.

\(When it's missing\)"
  (interactive)
  (if (and buffer-file-name (string-match "emacs-lisp" (format "%s" major-mode)))
      (let* ((filename (file-name-base buffer-file-name))
             (end-file-message (format  ";;; %s.el ends here" filename)))
        (goto-char (point-max))
        (unless (looking-back end-file-message nil)
          (insert end-file-message)))
    (message "Not a lisp file.")))

(defun package-commentary-to-markdown (markdown-file &optional emacslisp-file)
  "Convert commentary EMACSLISP-FILE and write it to MARKDOWN-FILE.

Conversion is minimal and expects that most of the docstring is
already formatted as markdown. Quoted `items' will be converted
to backquoted `items`.

If EMACSLISP-FILE is nil the current buffer file will be used."
  (interactive "FMarkdown file: \ni")
  (if (null emacslisp-file)
      (when (string-match "emacs-lisp" (format "%s" major-mode))
        (if (y-or-n-p (format "Use commentary in %s?" (f-base (buffer-file-name))))
            (package-commentary-to-markdown markdown-file (buffer-file-name))
          (let ((emacslisp-file (read-file-name "Emacslisp file: " nil nil t)))
            (package-commentary-to-markdown markdown-file emacslisp-file))))
    (let ((working-buffer (current-buffer)))
      (save-excursion
        (find-file emacslisp-file)
        (f-write-text
         (replace-regexp-in-string "`\\(.*?\\)'" "`\\1`" (lm-commentary))
         'utf-8 markdown-file))
      (if (y-or-n-p (format "Review changes to %s?" markdown-file))
          (find-file markdown-file)
        (switch-to-buffer (get-buffer working-buffer))))))

(defun package-markdown-to-commentary (markdown-file &optional emacslisp-file)
  "Read MARKDOWN-FILE and insert it into the EMACSLISP-FILE commentary.

Conversion is minimal and assumes the the markdown is suitable for insertion as
commentary.  Backquoted `code` will be converted to Emacs quoted `items'."
  (interactive "fMarkdown file: \ni")
  (if (null emacslisp-file)
      (when (string-match "emacs-lisp" (format "%s" major-mode))
        (if (y-or-n-p
             (format "Insert %s into %s?"
                     (f-base markdown-file)
                     (f-base (buffer-file-name))))
            (package-markdown-to-commentary markdown-file (buffer-file-name))
          (let ((emacslisp-file (read-file-name "Emacslisp file: " nil nil t)))
            (package-markdown-to-commentary markdown-file emacslisp-file))))
    (let ((working-buffer (current-buffer)))
      (save-excursion
        (find-file emacslisp-file)
        (let ((start (lm-commentary-start))
              (end (lm-commentary-end))
              (markdown-text (f-read-text markdown-file)))
            (delete-region start end)
            (goto-char start)
            (insert
              ";;; Commentary:\n"
              (replace-regexp-in-string "`\\(.*?\\)`" "`\\1'"
                (string-join
                   (mapcar
                    (lambda (line)
                      (format ";; %s \n" line))
                    (split-string markdown-text "\n")))))
            (if (y-or-n-p (format "Review changes to %s?" emacslisp-file))
                (message "Note: Changes not saved yet.")
              (save-buffer t)
              (switch-to-buffer working-buffer)))))))

(defun pcre-regexp-from-list-of-words (words)
  "Insert a pcre regexp to match a list of WORDS."
  (interactive "sList of words for regexp: ")
  (insert
   (pcre-to-elisp
    (regexp-opt (split-string words)))))

(defun random-in-range (start end)
  "Return a random number in range START to END."
  (random t)
  (+ start (random (+ 1 (- end start)))))

(defun reload-current-firefox-tab-osx ()
  "Run a simple applescript to reload the current Google Chrome tab.

OSX specific."
  (interactive)
  (shell-command "~/.doom.d/bin/reload-firefox.osa" nil nil))

(defun reload-current-chrome-tab-osx ()
  "Run a simple applescript to reload the current Google Chrome tab.

OSX specific."
  (interactive)
  (shell-command "echo 'tell application \"Google Chrome\"
                             reload active tab of window 1
                        end tell' | osascript" nil nil)
  (message "refreshed active Google Chrome tab"))

(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name))
        (read-file-name-function 'read-file-name-default))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun replace-pretty-quotes ()
  "Replace pretty quotes with standard quotes."
  (interactive)
  (replace-regexp-and-return "[”“]" "\""))

(defun replace-regexp-and-return (from to)
  "Replace regexp FROM to TO and return cursor to point."
  (save-excursion
    (while (re-search-forward from nil t)
      (replace-match to))))

(defun replace-region-with (fn)
  "Replace current region using FN."
  (let* ((input (buffer-substring-no-properties (region-beginning) (region-end)))
         (output (funcall fn input)))
    (delete-region (region-beginning) (region-end))
    (insert (if (stringp output) output
              (format "%S" output)))))

(defun replace-thing-at-point-with (fn)
  "Get the current thing at point.
Replace with the return value of the function FN"
  (let* ((pos1 (car (bounds-of-thing-at-point 'symbol)))
         (pos2 (cdr (bounds-of-thing-at-point 'symbol)))
         replacement
         excerpt)
    (when (> pos1 0)
      (setq pos1 (- pos1 1)))
    (setq excerpt (buffer-substring-no-properties pos1 pos2))
    (setq replacement (funcall fn excerpt))
    (delete-region pos1 pos2)
    (insert replacement)))

(defun revert-buffer-instant ()
  "Revert buffer without prompting."
  (interactive)
  (revert-buffer t t t))

(defun s-squeeze (char string)
  "Squeeze the occurences of CHAR in STRING.
This works the same as `tr -s CHAR`."
  (replace-regexp-in-string (format "%s+" (regexp-quote char)) char string))

(defun sass-hex-color-to-var ()
  "Find a hex color, and replace it with a newly created variable name.
Place the created variable at the top of the file.  Name it based
on the property being set, and its CSS selector, and set its
css-value to the hex color found."
  (interactive)
  (let
      (css-value
       css-property
       css-value-position
       variable-name
       variable-definition
       indent-level
       (css-selector ""))
    (save-excursion
      ;; search for a hex color
      (re-search-forward
       (rx bol (0+ blank)
           ;; CSS Property name
           (group (? "-") (regex "[_A-z]") (1+ (regex "[_0-9A-z-]")))
           (* blank) ":" (* blank) (* (regex "[A-z,0-9.% ]"))
           ;; Hex color
           (group "#" (** 3 6 (any hex-digit))) ";" eol))

      (setq css-value-position (match-beginning 2))
      (setq css-property (match-string-no-properties 1))
      (setq css-value  (match-string-no-properties 2))

      (move-end-of-line 1)
      (back-to-indentation)
      (setq indent-level (current-column))
      (while (< 0 indent-level)
        (re-search-backward
         (rx bol (* blank) (? "&") (? (any "." "#"))
             (group (any "_" alpha) (* (any "_" "-" "," " " ":" alphanumeric)))
             (* blank) "{"))
        (move-end-of-line 1)
        (back-to-indentation)
        (when (> indent-level (current-column))
          (setq indent-level (current-column))
          (setq css-selector
                (format "%s_%s" (match-string-no-properties 1) css-selector))))

      (setq variable-name
            (replace-regexp-in-string
             (rx (>= 2 "_")) "_"
             (replace-regexp-in-string
              (rx (any "&" ":" "-" "," " "))
              "_"
              (format "$%s%s" css-selector css-property))))

      (setq variable-definition (format "%s: %s;" variable-name css-value)))
    (goto-char css-value-position)

    (re-search-forward
     (rx "#" (** 3 6 (any hex-digit)) (0+ blank) ";"))
    (replace-match (format "%s;" variable-name) t)

    (goto-char 0) (newline) (goto-char 0)
    (insert variable-definition)))

(defun screencapture-mac (&optional commandline file-keyword)
  "Screencapture on macOS, interactive or supply COMMANDLINE and FILE-KEYWORD."
  (interactive)
  (if (or screencapture-mac-default-commandline commandline)
      (if commandline
          (screencapture-mac--run commandline
                                  (screencapture-mac--filename-generator
                                   screencapture-mac-default-file-location nil file-keyword))
        (screencapture-mac--run screencapture-mac-default-commandline
                                (screencapture-mac--filename-generator
                                 screencapture-mac-default-file-location)))
    (let* ((command (s-squeeze " "
                               (s-join " " (-concat '("screencapture")
                                                    (mapcar 'screencapture-mac--complete-arguments-for-option
                                                            (screencapture-mac--entry-from-summaries
                                                             (completing-read-multiple
                                                              "Options: "
                                                              (screencapture-mac--summary-list))))))))
           (filename (screencapture-mac--filename-generator screencapture-mac-default-file-location nil file-keyword)))

      (when (y-or-n-p (format "Make default (%s)?" command))
        (setq screencapture-mac-default-commandline command))
      (screencapture-mac--run command filename))))

(defun screencapture-mac--complete-arguments-for-option (plist)
  "Do completeing read for arguments of option, using PLIST."
  (plist-bind (flag arg description helper helper-prompt) plist
              (if arg
                  (format " %s %s " flag
                          (if helper
                              (funcall helper)
                            (read-string
                             (format "%s %s %s ?: " description flag arg))))
                (format " %s " flag))))

(defun screencapture-mac--entry-from-summaries (summaries)
  "Get an option from SUMMARIES."
  (mapcar 'screencapture-mac--get-option summaries))

(defun screencapture-mac--filename-generator (path &optional ext file-keyword)
  "Create filename for the screenshot at PATH with optional EXT & FILE-KEYWORD."
  (let ((path (if (s-ends-with? "/" path) path (format "%s/" path)))
        (file-keyword
         (or file-keyword
             screencapture-mac-default-file-keyword)))
    (s-squeeze
     "-"
     (s-replace
      " " "-"
      (format "%s%s%s.%s"
              path
              file-keyword
              (s-replace ":" "." (time-stamp-string))
              (or ext "png"))))))

(defun screencapture-mac--get-option (summary)
  "Fetch option from SUMMARY."
  (let ((flag (substring summary 0 2)))
    (kvplist2get
     (screencapture-mac--options) :flag flag)))

(defun screencapture-mac--options ()
  "Command line options for screencapture (macOS)."
  `((:flag "-B" :arg "<bundleid>" :description "screen capture output will open in app with bundleid")
    (:flag "-C" :description "capture the cursor as well as the screen. only in non-interactive modes")
    (:flag "-D" :arg "<display>"  :description "screen capture or record from the display specified. -D 1 is main display -D 2 second")
    (:flag "-G" :arg "<id>"       :description "captures audio during a video recording using audio id specified.")
    (:flag "-I" :description "screen capture output will open in Messages")
    (:flag "-J" :arg "<style>"    :description "sets the starting of interfactive capture \n selection       - captures screen in selection mode \n window          - captures screen in window mode \n video           - records screen in selection mode")
    (:flag "-M" :description "screen capture output will go to a new Mail message")
    (:flag "-P" :description "screen capture output will open in Preview or QuickTime Player if video")
    (:flag "-R" :arg "<x,y,w,h>"     :description "capture screen rect")
    (:flag "-S" :description "in window capture mode capture the screen not the window")
    (:flag "-T" :arg "<seconds>"  :description "take the picture after a delay of <seconds> default is 5")
    (:flag "-U" :description "Show interactive toolbar in interactive mode")
    (:flag "-V" :arg "<seconds>"  :description "limits video capture to specified seconds")
    (:flag "-W" :description "start interaction in window selection mode")
    (:flag "-a" :description "do not include windows attached to selected windows")
    (:flag "-b" :description "capture Touch Bar - non-interactive modes only")
    (:flag "-c" :description "force screen capture to go to the clipboard")
    (:flag "-d" :description "display errors to the user graphically")
    (:flag "-g" :description "captures audio during a video recording using default input.")
    (:flag "-i" :description "capture screen interactively by selection or window \n control key - causes screen shot to go to clipboard \n space key   - toggle between mouse selection and \n window selection modes \n escape key  - cancels interactive screen shot")
    (:flag "-k" :description "show clicks in video recording mode")
    (:flag "-l" :arg "<windowid>" :description "capture this windowsid" :helper ,#'screencapture-mac--windowid-helper)
    (:flag "-m" :description "only capture the main monitor undefined if -i is set")
    (:flag "-o" :description "in window capture mode do not capture the shadow of the window")
    (:flag "-p" :description "screen capture will use the default settings for capture. The files argument will be ig")
    (:flag "-r" :description "do not add dpi meta data to image")
    (:flag "-s" :description "only allow mouse selection mode")
    (:flag "-t" :arg "<format>"   :description "image format to create default is png (other options include pdf jpg tiff and other")
    (:flag "-u" :description "present UI after screencapture is complete. files passed to command line will be ignored")
    (:flag "-v" :description "capture video recording of the screen")
    (:flag "-w" :description "only allow window selection mode")
    (:flag "-x" :description "do not play sounds")))

(defun screencapture-mac--options-summary (plist)
  "Get the options summary from PLIST."
  (plist-bind (flag description) plist
              (format "%2s %s." flag description)))

(defun screencapture-mac--run (command filename)
  "Execute the shell COMMAND with FILENAME."
  (message "%s \"%s\"" command filename)
  (shell-command
   (format "%s \"%s\"" command filename)))

(defun screencapture-mac--summary-list ()
  "Summarized list of screencapture mac options."
  (mapcar
   'screencapture-mac--options-summary
   (screencapture-mac--options)))

(defun screencapture-mac--windowid-helper ()
  "Get the windowid from a `completing-read' list."
  (car (last
        (s-match "^\\([0-9]*\\) -"
          (completing-read "Select window: "
             (--map
               (cl-destructuring-bind (windowid app name) it
                                                    (format "%s - [%s] %s" windowid app name))
               (macos-get-list-of-windowids)))))))

(defun screencapture-mac-reset-default-commandline ()
  "Reset the default commandline."
  (interactive)
  (setq screencapture-mac-default-commandline nil))


(defmacro ocodo/cmdalias (name command)
  "Create an alias NAME of an interactive COMMAND."
  `(defun ,name ()
     ,(format "%s is an alias of %s." name command)
     (interactive)
     (call-interactively (function ,command))))

(ocodo/cmdalias ocodo/reload-fonts set-doom-lambda-line-fonts)

(defun ocodo/load-theme (&optional theme-name)
  "Load a theme without confirmation or enabling."
  (interactive)
  (let ((theme-name (or theme-name (completing-read "Load Theme:" (custom-available-themes)))))
    (load-theme  (intern  theme-name ) t)
    (ocodo/reload-fonts)))

(defun set-doom-lambda-line-fonts ()
  "Sort out font / unicode / fontset stuff."
  (interactive)
  (doom/increase-font-size 1)
  (doom/decrease-font-size 1)
  (lambda-line--clockface-update-fontset "ClockFaceRect"))

(defun search-backward-wrapped-string (wrap_start wrap_end)
  "Search for a string backwards from the current point.

Use the strings WRAP_START and WRAP_END, to match the start and
end of the string.

if WRAP_END and WRAP_START are equal, we first position the point
at the beginning of the first WRAP_END match, before the initial
point.

The string found between the two wrappers is returned.

This is useful for naive finding of symbols previously defined in
the buffer."
  (save-excursion
    (when (equal wrap_start wrap_end)
      (search-backward wrap_end))
    (let* ((start_match
            (+ (search-backward wrap_start)
               (length wrap_start)))
           (end_match 0))
      (goto-char start_match)
      (setq end_match (- (search-forward wrap_end) 1))
      (buffer-substring-no-properties start_match end_match))))

(defun search-for-nearest-hex-color (p)
  "Search to the nearest hex color.
Use negative prefix P to go backward."
  (interactive "p")
  (let ((regexp "#[0-9a-fA-F]\\{3,6\\}"))
    (if (> p 0)
        (search-forward-regexp regexp)
      (search-backward-regexp regexp))))

(defun set-default-font-height (p)
  "Set the default font :height P (prefix arg) or enter in minibuffer."
  (interactive "P")
  (unless p
    (setq p (string-to-number (read-from-minibuffer
                               (format "Set default font height (currently %s): "
                                       (face-attribute 'default :height))))))
  (set-face-attribute 'default nil :height  p)
  (message "Default font height set to %s" p))

(defun set-internal-border (n)
  "Set or reset the internal border width N of the selected frame."
  (interactive (list (string-to-number (read-from-minibuffer "Border width: "))))
  (let ((w (or n 0)))
    (set-frame-parameter
     (selected-frame)
     'internal-border-width
     w)))

(defun shell-command-on-buffer-file ()
  "Run a shell command, using the file of current buffer as input.
Return an error if no buffer file."
  (interactive)
  (or (buffer-file-name) (error "There is no file associated with this buffer"))
  (let* ((my-cmd (read-shell-command "Command to run: "))
         (cmd-to-run (concat my-cmd " " (buffer-file-name))))
    (shell-command cmd-to-run)))

(defun shell-command-on-region-replace (start end command)
  "Run `shell-command-on-region' replacing the selected region.  START END COMMAND."
  (interactive (let (string)
                 (unless (mark)
                   (error "The mark is not set now, so there is no region"))
                 (setq string (read-from-minibuffer "Shell region | replace: "
                                                    nil nil nil
                                                    'shell-command-history))
                 (list (region-beginning) (region-end)
                       string)))
  (shell-command-on-region start end command t t))

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or `beginning-of-line'."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun snippy-comment ()
  "Insert a snip line `- - 8< - - -' comment."
  (interactive)
  (end-of-line)
  (newline)
  (insert "- - 8<")
  (cl-loop repeat 60 do (insert " -"))
  (beginning-of-line)
  (comment-region (point-at-bol) (point-at-eol)))

(defun sort-sexps (beg end)
  "Sort sexps in region BEG / END.
Comments stay with the code below."
  (interactive "r")
  (cl-flet ((skip-whitespace () (while (looking-at (rx (1+ (or space "\n"))))
                                  (goto-char (match-end 0))))
            (skip-both () (while (cond ((or (nth 4 (syntax-ppss))
                                            (ignore-errors
                                              (save-excursion
                                                (forward-char 1)
                                                (nth 4 (syntax-ppss)))))
                                        (forward-line 1))
                                       ((looking-at (rx (1+ (or space "\n"))))
                                        (goto-char (match-end 0)))))))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char beg)
        (skip-both)
        (cl-destructuring-bind (sexps markers)
            (cl-loop do (skip-whitespace)
                     for start = (point-marker)
                     for sexp = (ignore-errors
                                  (read (current-buffer)))
                     for end = (point-marker)
                     while sexp
                     ;; Collect the real string, then one used for sorting.
                     collect (cons (buffer-substring (marker-position start) (marker-position end))
                                   (save-excursion
                                     (goto-char (marker-position start))
                                     (skip-both)
                                     (buffer-substring (point) (marker-position end))))
                     into sexps
                     collect (cons start end)
                     into markers
                     finally return (list sexps markers))
          (setq sexps (sort sexps (lambda (a b)
                                    (string< (cdr a) (cdr b)))))
          (cl-loop for (real . sort) in sexps
                   for (start . end) in markers
                   do (progn
                        (goto-char (marker-position start))
                        (insert-before-markers real)
                        (delete-region (point) (marker-position end)))))))))

(defun ssh-agent--set-env-sock (sock)
  "Set the env var SSH_AUTH_SOCK to SOCK."
  (setenv "SSH_AUTH_SOCK"
          (shell-command-to-string
           (format "echo '%s' | awk '{printf($8)}'" sock))))

(defun ssh-agent--fix ()
  "Fix the env to use the ssh-agent."
  (let ((private-sock (shell-command-to-string "lsof -c ssh-agent | grep /private"))
        (agent-sock (shell-command-to-string "lsof -c ssh-agent | grep /agent")))
    (unless (string= private-sock "")
      (ssh-agent--set-env-sock private-sock))
    (unless (string= agent-sock "")
      (ssh-agent--set-env-sock agent-sock))))

(defun ssh-agent--close (ssh-agent-info)
  "Extract PID from SSH-AGENT-INFO and close the process."
  (let ((pid (cadr (split-string ssh-agent-info " " t))))
    (shell-command (format "kill %s" pid))))

(defun ssh-agent--close-rejected-agents (selected-agent ssh-agents)
  "Close SSH-AGENTS that are not SELECTED-AGENT."
    (let ((redundant-agents (--remove (string= it selected-agent) ssh-agents)))
      (--each redundant-agents (ssh-agent--close it))))

(defun ssh-agent-env-fix ()
  "Ensure $SSH_AUTH_SOCK is set correctly in the environment."
  (interactive)
  (if (= (string-to-number (shell-command-to-string "pgrep ssh-agent | wc -l")) 0)
    (message "No ssh-agents are running!")
    
   (if (= (string-to-number (shell-command-to-string "pgrep ssh-agent | wc -l")) 1)
       (ssh-agent--fix)
     (let* ((ssh-agents (split-string
                         (shell-command-to-string
                          "lsof -c ssh-agent | grep -E '/var/|/private' | awk '{print $1, $2, $8}'")
                         "\n" t))
            (selected-agent (completing-read
                             "Select the ssh-agent (There are more than 1): "
                             ssh-agents)))
         (ssh-agent--close-rejected-agents selected-agent ssh-agents)
         (ssh-agent--fix)))))

(defun setup-ssh-agent ()
  "On macOS, check if launchd ssh-agent is running.
Otherwise start it, adding ssh keys from the macOS keychain."
  (interactive)
  (when (eq system-type 'darwin) ;; check if macOS
    (let ((ssh-auth-sock (getenv "SSH_AUTH_SOCK"))
          (ssh-agent-info (shell-command-to-string "launchctl getenv SSH_AUTH_SOCK")))
      (if (and (string-prefix-p "/private/tmp/com.apple.launchd." ssh-auth-sock)
               (string-prefix-p "/private/tmp/com.apple.launchd." ssh-agent-info))
          ;; if launchd ssh-agent is running and ssh-auth-sock points to it
          (progn
            ;; terminate any other ssh-agents running
            (dolist (pid (split-string (shell-command-to-string "pgrep ssh-agent") "\n" t))
              (unless (string-prefix-p (concat "SSH_AUTH_SOCK=" ssh-auth-sock) (shell-command-to-string (concat "ps -o command= " pid)))
                (shell-command (concat "kill " pid))))
            ;; set SSH environment variables
            (setenv "SSH_AUTH_SOCK" ssh-auth-sock)
            (setenv "SSH_AGENT_PID" (substring ssh-auth-sock (length "/private/tmp/com.apple.launchd.")))
            (setenv "SSH_ASKPASS" "git-gui--askpass")
            (setenv "GIT_ASKPASS" "git-gui--askpass")
            ;; add ssh keys from macOS keychain
            (shell-command "ssh-add -A"))
        ;; if launchd ssh-agent is not running
        (progn
          ;; start launchd ssh-agent
          (shell-command "eval $(ssh-agent -s | tee /dev/stderr)")
          ;; set SSH environment variables
          (setenv "SSH_ASKPASS" "git-gui--askpass")
          (setenv "GIT_ASKPASS" "git-gui--askpass")
          ;; add ssh keys from macOS keychain
          (shell-command "ssh-add -A"))))))

(defun switch-to-message-buffer ()
  "Switch to the message buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun switch-to-minibuffer-window ()
  "Switch to minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))

(defun switch-to-scratch ()
  "Switch to scratch, grab the region if it's active."
  (interactive)
  (let ((contents
         (and (region-active-p)
              (buffer-substring (region-beginning)
                                (region-end)))))
    (switch-to-buffer "*scratch*")
    (if contents
        (progn
          (goto-char (buffer-end 1))
          (insert contents)))))

(defun time-now ()
 "Current time."
 (interactive)
 (message (format-time-string "%l:%M%P(%z) %Y-%m-%d")))

(defun time-to-seconds (time)
 "Convert TIME `hh:mm:ss' into seconds."
 (cl-destructuring-bind (hh mm ss)
     (mapcar 'string-to-number
             (cdr (car
                   (s-match-strings-all
                    "\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)"
                    time))))
   (+ (* 3600 hh) (* 60 mm) ss)))

(defun toggle-window-split ()
  "Toggle the current window split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun untabify-buffer ()
  "Untabify the current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun utc-seconds ()
  "Insert UTC seconds."
  (interactive)
  (insert (format-time-string "%s")))

(provide 'ocodo-handy-functions)

;;; ocodo-handy-functions.el ends here
