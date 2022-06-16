;;; ocodo-handy-functions --- a collection of functions that didn't get organized
;;; Author: Jason Milkins <jasonm23@gmail.com>
;;; Commentary:
;;
;;  A collection of miscellaneous functions, which are either
;;  candidates to migrate to a minor mode, or will languish here in
;;  perpetuity.
;;
;;; License:
;;  GPL3
;;
;;; Code:

(require 's)
(require 'cl)
(require 'dash)
(require 'find-func)
(require 'kurecolor)
(require 'cua-base)
(require 'magit)

(defmacro *-and-replace (function-name evaluator)
  "Build FUNCTION-NAME to use EVALUATOR on the current region, and replace it with the result."
  `(defun ,function-name ()
     (interactive)
     (if (not (region-active-p))
         (replace-thing-at-point-with ,evaluator)
       ;; - else -
       (replace-region-with ,evaluator))))

(*-and-replace calc-eval-replace-at-region-or-point #'calc-eval)

(*-and-replace decimal-to-hex-at-point-or-region #'decimal-to-hex)

(*-and-replace hex-to-decimal-at-point-or-region #'hex-to-decimal)

(*-and-replace video-time-to-seconds-at-point-or-region #'video-time-to-seconds)

(defmacro defun-pcase (name arglist &optional docstring &rest body)
  "Define a pcase function called NAME with ARGLIST.

All `defun-pcase' should have a DOCSTRING.

BODY is the form of the underlying `pcase-lambda'."
  (declare (doc-string 3) (indent 2))
  `(progn (defalias
            (quote ,name)
            (pcase-lambda ,arglist ,@body)
            ,docstring)))

(defmacro plist-bind (args expr &rest body)
  ;; http://emacs.stackexchange.com/questions/22542
  "A `destructuring-bind' without the boilerplate for plists."
  `(cl-destructuring-bind
       (&key ,@args &allow-other-keys)
       ,expr
     ,@body))

(defun -sample (list)
  "Return a random element from the LIST."
  (nth (random (length list)) list))

(defun align-number-right (begin end)
  "Align region to equal signs from BEGIN to END."
  (interactive "r")
  (align-regexp begin end ".* \\([0-9]+\\).*" -1 1 nil))

(defun buffer-file-name-to-kill-ring ()
  "Save the buffer file name to the kill ring."
  (interactive)
  (when (buffer-file-name)
    (kill-new (buffer-file-name))))

(defun change-number-at-point (func)
  "Change the number at point using FUNC."
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (progn
        (forward-word)
        (search-backward (number-to-string number))
        (replace-match (number-to-string (funcall func number)))
        (goto-char point)))))

(defun cleanup-buffer ()
  "Perform a cleanup operations on a buffer, tabs to spaces, re-indent, trim whitespace."
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
  "Comments or uncomments current current line or whole lines in region."
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

(defun csv--to-lists (csv)
  "Convert CSV to lists."
  (mapcar (lambda (line) (split-string line ","))
          (split-string (s-chomp csv) "\n")))

(defun current-buffer-defuns-to-markdown (file)
  "Create a markdown FILE of all defuns in the current buffer."
  (interactive "FWrite List of defuns to Markdown File: ")
  (f-write-text (generate-markdown-list-of-buffer-defuns (current-buffer)) 'utf-8 file)
  (when (y-or-n-p (format "Open %s?" file))
    (find-file file)))

(defun decimal-to-hex (num)
  "Convert NUM to hex."
  (format "%X" (string-to-number num)))

(defun decrease-default-font-height (m)
  "Adjust the default font :height by 10, universal argument is M (to set by multiples)."
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
  "Delete the file connected to this buffer and kill it, FORCE is universal argument."
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
  (interactive)
  (let* ((thing (symbol-at-point)))
    (cond
     ((fboundp thing) (describe-function thing))
     ((boundp thing) (describe-variable thing)))))

(defun dired-find-file-other-window-and-back ()
  "In Dired, visit this file or directory in another window and remain in first window."
  (interactive)
  (find-file-other-window (dired-get-file-for-visit))
  (switch-window))

(defun dired-menu ()
  "Go to one of the currently open dired buffers (if there is one)."
  (interactive)
  (let* ((dired-buffers (--map (buffer-name it)
                               (--filter
                                (equal 'dired-mode (with-current-buffer it major-mode))
                                (buffer-list)))))
    (if dired-buffers
        (switch-to-buffer (completing-read "Select dired: " dired-buffers))
      (message "There's no dired buffers open right now"))))

(defun dired-osx-open-this-file ()
  "Use the OSX `open' command to launch the current dired file at point."
  (interactive)
  (shell-command-to-string (format "open %S" (dired-file-name-at-point))))

(defun dired-visit-library (libraryname)
  "Open directory with dired which contain the given LIBRARYNAME."
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
                   (goto-char (second saved-region))
                   (exchange-point-and-mark))
          (progn (message "setting region")
                 (push-mark-command nil)
                 (goto-char (- (point) (length region)))))
        (setq deactivate-mark nil)))))

(defun duplicate-current-line-or-region-up (arg)
  "Duplicates the current line or region up ARG times."
  (interactive "p")
  (duplicate-current-line-or-region arg t))

(defun elpa-package-insert-ends-here ()
  "Insert the ELPA package file ending string.

\(When it's missing\)"
  (interactive)
  (if (and buffer-file-name (string-match "emacs-lisp" (format "%s" major-mode)))
      (let* ((filename (file-name-base))
             (end-file-message (format  ";;; %s.el ends here" filename)))
        (goto-char (point-max))
        (unless (looking-back end-file-message nil)
          (insert end-file-message)))
    (message "Not a lisp file.")))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
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

(defun flush-blank-lines ()
  "Flush blank lines."
  (interactive)
  (flush-lines "^\s*$" nil nil t))

(defun format-bin (val width)
  "Convert VAL of WIDTH to a binary string."
  (let (result)
    (while (> width 0)
      (if (equal (mod val 2) 1)
          (setq result (concat "1" result))
        (setq result (concat "0" result)))
      (setq val (/ val 2))
      (setq width (1- width)))
    result))

(defun format-thousands-separators (n)
  "Format N to have thousand separators."
  (let* ((parts (split-string (number-to-string n) "[.]"))
         (characteristic (first parts))
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
      (format "%s.%s" separated (second parts)))))

(defun fraction-radian (denominator)
  "Fraction DENOMINATOR of circle to radians."
  (interactive "nDenomiator:")
  (insert (format "%s" (/ (* float-pi 2) denominator))))

(defun generate-markdown-list-of-buffer-defuns (buffer)
  "Generate markdown text of all defuns in buffer"
  (s-join "\n"
          (mapcar
           (lambda (entry)
             (cl-destructuring-bind (name args docstring) entry
               (setq
                name (format "%s" name)
                args (format "%s" args))
               (when (string= nil docstring)
                 (setq docstring "No docstring available: TODO"))
               (format "### %s\n\n```lisp\n(%s)\n```\n\n%s\n"
                       name
                       (format "%s %s" name (or args ""))
                       (docstring-args-to-markdown-code  docstring))))

           (-sort (lambda (a b)
                    (let ((c (symbol-name (first a)))
                          (d (symbol-name (first b))))
                      (string< c d)) )
                  (get-defun-info (current-buffer))))))

(defun docstring-args-to-markdown-code (docstring)
  "transform DOCSTRING I arguments to inline markdown `code` style."
  (format "%s" (s-replace-regexp "\([A-Z]\{2,\}\)" "@@\1@@" docstring)))

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

(defun get-defun-info (buffer)
  "Get information about all `defun' top-level sexps in a buffer
BUFFER. Returns a list with elements of the form (symbol args docstring)."
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

(defun get-osx-display-resolution ()
  "Get the current display resolution in OSX."
  (--map (s-split "x" it)
         (--filter (not (string= it ""))
                   (s-split "\n" (shell-command-to-string
                                  "system_profiler SPDisplaysDataType |\
                                   grep Resolution |\
                                   sed -e 's/Resolution: //' -e 's/ //g'")))))

(defun get-position-of-nearest-matching (s &optional arg)
  "Get the position of nearest S.

optional ARG when less than zero, default to the before match
when matches are equidistant from the current point."
  (let* ((after      (- (save-excursion (search-forward s)) (length s)))
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
      (error "Not in a git repository"))))

(defun git-open-changed-files ()
  "Use git ls-files to open changed files."
  (interactive)
  (let ((git-modified-files (shell-command-to-string "git ls-files -m")))
    (if (not (string-match "^fatal: Not a git repo" git-modified-files))
        (let ((file-list (split-string git-modified-files "\n" t "[\r\n\t ]")))
          (mapc (lambda (file) (find-file file)) file-list))
      (error "Not in a git repository"))))

(defun git-open-from-ls-files (git-ls-options)
  "Use GIT-LS-OPTIONS to open changed files."
  (interactive "sGit ls-files options: ")
  (let ((git-modified-files (shell-command-to-string (format "git ls-files %s" git-ls-options))))
    (if (not (string-match "^fatal: Not a git repo" git-modified-files))
        (let ((file-list (split-string git-modified-files "\n" t "[\r\n\t ]")))
          (mapc (lambda (file) (find-file file)) file-list))
      (error "Not in a git repository"))))

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
      (error "Not in a git repository"))))

(defun git-open-untracked-files ()
  "Use git ls-files to open untracked files.

    Open any untracked file in the repo (unless it's been .gitignored)"
  (interactive)
  (let ((git-untracked-files (shell-command-to-string "git ls-files --others --exclude-standard")))
    (if (not (string-match "^fatal: Not a git repo" git-untracked-files))
        (let ((file-list (split-string git-untracked-files "\n" t "[\r\n\t ]")))
          (mapc (lambda (file) (find-file file)) file-list))
      (error "Not in a git repository"))))

(defun github-browse-repo (repo)
  "Browse a github REPO by supplying the user/reponame."
  (interactive "sGithub Repo [format: user/repo]: ")
  (browse-url (format "https://github.com/%s" repo)))

(defun hex-to-decimal (num)
  "Convert hex NUM to decimal."
  (format "%i" (string-to-number num 16)))

(defun increase-default-font-height (m)
  "Adjust the default font :height by 10, universal argument is M (to set by multiples)."
  (interactive "p")
  (let ((new-height (+ (* m 10) (face-attribute 'default :height))))
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
          (replace-match (format-bin answer field-width)))))))

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
  (insert (format-time-string "%l:%M%P(%z) %Y-%m-%d")))

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

(defun magit-just-amend ()
  "Just git commit --amend."
  (interactive)
  (save-window-excursion
    (shell-command "git --no-pager commit --amend --reuse-message=HEAD")
    (magit-refresh)))

(defun make-kurecolor-24bit-hue-table (color)
  "Make a 24bit color table using Kurecolor."
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

;; Generated from: #A30905 (use Rainbow-mode for niceness)
;; ;; 10°     20°     30°     40°     50°     60°     70°     80°     90°     100°    110°    120°
;; ;; #A31F05 #A33905 #A35405 #A36E05 #A38805 #A3A305 #88A305 #6EA305 #54A305 #39A305 #1FA305 #05A305
;; ;; 130°    140°    150°    160°    170°    180°    190°    200°    210°    220°    230°    240°
;; ;; #05A31F #05A339 #05A354 #05A36E #05A388 #05A3A3 #0588A3 #056EA3 #0554A3 #0539A3 #051FA3 #0505A3
;; ;; 250°    260°    270°    280°    290°    300°    310°    320°    330°    340°    350°    360°
;; ;; #00030B #3905A3 #5405A3 #6E05A3 #8805A3 #A305A3 #A30588 #A3056E #A30554 #A30539 #A3051F #A30505

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

(defun open-line-above ()
  "Open a newline above the current point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (newline)
    (forward-line -1)))

(defun open-line-below ()
  "Open a newline below the current point."
  (interactive)
  (end-of-line)
  (newline)
  (back-to-indentation))

(defun open-this-in-intellij-idea-15-osx ()
  "Open the current file in intellij IDEA 15 (OS X specific)."
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
     (rx "#" (** 3 6 (any hex-digit)) (0+ blank) ";" ))
    (replace-match (format "%s;" variable-name) t)

    (goto-char 0) (newline) (goto-char 0)
    (insert variable-definition)))

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
  "Sort sexps in region.
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
  "current time."
  (interactive)
  (message (format-time-string "%l:%M%P(%z) %Y-%m-%d")))

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

(defun video-time-to-seconds (video-time)
  "Convert a VIDEO-TIME formar hh:mm:ss into seconds."
  (cl-destructuring-bind (hh mm ss)
      (mapcar 'string-to-number
              (cdr (car
                    (s-match-strings-all
                     "\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)"
                     video-time))))
    (+ (* 3600 hh) (* 60 mm) ss)))

(defun yank-repeat (&optional arg)
  "Repeat yank n times ARG."
  (interactive "*p")
  (dotimes (string-to-int arg) (yank)))

(defun s-squeeze (char string)
  "Squeeze the occurences of CHAR in STRING.
This works the same as `tr -s CHAR`."
  (s-replace-regexp (format "%s+" char) char string))


;; usage: screencapture [-icMPmwsWxSCUtoa] [files]
;;   -c         force screen capture to go to the clipboard
;;   -b         capture Touch Bar - non-interactive modes only
;;   -C         capture the cursor as well as the screen. only in non-interactive modes
;;   -d         display errors to the user graphically
;;   -i         capture screen interactively, by selection or window
;;                control key - causes screen shot to go to clipboard
;;                space key   - toggle between mouse selection and
;;                              window selection modes
;;                escape key  - cancels interactive screen shot
;;   -m         only capture the main monitor, undefined if -i is set
;;   -D<display> screen capture or record from the display specified. -D 1 is main display, -D 2 secondary, etc.
;;   -o         in window capture mode, do not capture the shadow of the window
;;   -p         screen capture will use the default settings for capture. The files argument will be ignored
;;   -M         screen capture output will go to a new Mail message
;;   -P         screen capture output will open in Preview or QuickTime Player if video
;;   -I         screen capture output will open in Messages
;;   -B<bundleid> screen capture output will open in app with bundleid
;;   -s         only allow mouse selection mode
;;   -S         in window capture mode, capture the screen not the window
;;   -J<style>  sets the starting of interfactive capture
;;                selection       - captures screen in selection mode
;;                window          - captures screen in window mode
;;                video           - records screen in selection mode
;;   -t<format> image format to create, default is png (other options include pdf, jpg, tiff and other formats)
;;   -T<seconds> take the picture after a delay of <seconds>, default is 5
;;   -w         only allow window selection mode
;;   -W         start interaction in window selection mode
;;   -x         do not play sounds
;;   -a         do not include windows attached to selected windows
;;   -r         do not add dpi meta data to image
;;   -l<windowid> capture this windowsid
;;   -R<x,y,w,h> capture screen rect
;;   -v        capture video recording of the screen
;;   -V<seconds> limits video capture to specified seconds
;;   -g        captures audio during a video recording using default input.
;;   -G<id>    captures audio during a video recording using audio id specified.
;;   -k        show clicks in video recording mode
;;   -U        Show interactive toolbar in interactive mode
;;   -u        present UI after screencapture is complete. files passed to command line will be ignored
;;   files   where to save the screen capture, 1 file per screen


(require 'kv)

(defvar screencapture-mac-default-file-location
  (expand-file-name "~/Desktop")
  "Default location to save screen captures.") 

(defun screencapture-mac ()
  "Screencapture on macOS."
  (interactive)
  (if screencapture-mac-default-commandline
      (screencapture-mac--run screencapture-mac-default-commandline)
    (let* ((command (s-squeeze
                     " "
                     (s-join " " (-concat '("screencapture")
                                          (mapcar 'screencapture-mac--complete-arguments-for-option
                                                  (screencapture-mac--entry-from-summaries
                                                   (completing-read-multiple
                                                    "Options: "                                                 
                                                    (screencapture-mac--summary-list))))))))
           (filename screencapture-mac-default-file-location))
      
      (when (y-or-n-p (format "Make default (%s) :" command))
        (setq screencapture-mac-default-commandline command))    
      (screencapture-mac--run command filename))))

(defun screencapture-mac--run (command filename)
  "Execute the shell COMMAND with FILENAME."
  (shell-command
   (format "%s \"%s\"" command filename )))

;; (screencapture-mac)

(defun screencapture-mac--summary-list ()
  "Summarized list of screencapture mac options"
  (mapcar
   'screencapture-mac--options-summary
   (screencapture-mac--options)))

(defun screencapture-mac--options-summary (plist)
  (cl-destructuring-bind (&key flag arg description) plist
    (format "%2s %s." flag description)))

(defun screencapture-mac--get-option (summary)
  "Fetch the option from SUMMARY"
  (let ((flag (substring summary 0 2)))
    (kvplist2get
     (screencapture-mac--options) :flag flag)))

(defun screencapture-mac--entry-from-summaries (summaries)
  (mapcar 'screencapture-mac--get-option summaries))

(defun screencapture-mac--complete-arguments-for-option (plist)
  "Do completeing read for arguments of option."
  (plist-bind (flag arg description) plist
              (if arg
                  (format " %s %s " flag
                          (read-string
                           (format "%s %s %s ?: " description flag arg)))
                (format " %s " flag))))

(defun screencapture-mac--options ()
  "Command line options for screencapture (macOS)."
  '(
    ( :flag "-B" :arg "<bundleid>" :description "screen capture output will open in app with bundleid")
    ( :flag "-C" :description "capture the cursor as well as the screen. only in non-interactive modes")
    ( :flag "-D" :arg "<display>"  :description "screen capture or record from the display specified. -D 1 is main display -D 2 second")
    ( :flag "-G" :arg "<id>"       :description "captures audio during a video recording using audio id specified.")
    ( :flag "-I" :description "screen capture output will open in Messages")
    ( :flag "-J" :arg "<style>"    :description "sets the starting of interfactive capture \n selection       - captures screen in selection mode \n window          - captures screen in window mode \n video           - records screen in selection mode")
    ( :flag "-M" :description "screen capture output will go to a new Mail message")
    ( :flag "-P" :description "screen capture output will open in Preview or QuickTime Player if video")
    ( :flag "-R" :arg "<x,y,w,h>"     :description "capture screen rect")
    ( :flag "-S" :description "in window capture mode capture the screen not the window")
    ( :flag "-T" :arg "<seconds>"  :description "take the picture after a delay of <seconds> default is 5")
    ( :flag "-U" :description "Show interactive toolbar in interactive mode")
    ( :flag "-V" :arg "<seconds>"  :description "limits video capture to specified seconds")
    ( :flag "-W" :description "start interaction in window selection mode")
    ( :flag "-a" :description "do not include windows attached to selected windows")
    ( :flag "-b" :description "capture Touch Bar - non-interactive modes only")
    ( :flag "-c" :description "force screen capture to go to the clipboard")
    ( :flag "-d" :description "display errors to the user graphically")
    ( :flag "-g" :description "captures audio during a video recording using default input.")
    ( :flag "-i" :description "capture screen interactively by selection or window \n control key - causes screen shot to go to clipboard \n space key   - toggle between mouse selection and \n window selection modes \n escape key  - cancels interactive screen shot")
    ( :flag "-k" :description "show clicks in video recording mode")
    ( :flag "-l" :arg "<windowid>" :description "capture this windowsid")
    ( :flag "-m" :description "only capture the main monitor undefined if -i is set")
    ( :flag "-o" :description "in window capture mode do not capture the shadow of the window")
    ( :flag "-p" :description "screen capture will use the default settings for capture. The files argument will be ig")
    ( :flag "-r" :description "do not add dpi meta data to image")
    ( :flag "-s" :description "only allow mouse selection mode")
    ( :flag "-t" :arg "<format>"   :description "image format to create default is png (other options include pdf jpg tiff and other")
    ( :flag "-u" :description "present UI after screencapture is complete. files passed to command line will be ignored")
    ( :flag "-v" :description "capture video recording of the screen")
    ( :flag "-w" :description "only allow window selection mode")
    ( :flag "-x" :description "do not play sounds")
    ))

(provide 'ocodo/handy-functions)

;;; ocodo/handy-functions.el ends here
