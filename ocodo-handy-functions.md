### -sample

```lisp
(-sample (list))
```

Return a random element from the LIST.

### align-number-right

```lisp
(align-number-right (begin end))
```

Align region to equal signs from BEGIN to END.

### buffer-file-name-to-kill-ring

```lisp
(buffer-file-name-to-kill-ring nil)
```

Save the buffer file name to the kill ring.

### change-number-at-point

```lisp
(change-number-at-point (func))
```

Change the number at point using FUNC.

### cleanup-buffer

```lisp
(cleanup-buffer nil)
```

Perform a cleanup operations on a buffer, tabs to spaces, re-indent, trim whitespace.

### clear-buffer-text-properties

```lisp
(clear-buffer-text-properties nil)
```

Clear all text face properties in the buffer.
This is somewhat useful when dealing with text pasted from a
propertied buffer.

Note: this won't turn off face properties in a font-locked buffer.

### comment-or-uncomment-current-line-or-region

```lisp
(comment-or-uncomment-current-line-or-region nil)
```

Comments or uncomments current current line or whole lines in region.

### copy-region-or-rest-of-line-to-other-window

```lisp
(copy-region-or-rest-of-line-to-other-window nil)
```

Copy the current region to the other window.

### copy-rest-of-line

```lisp
(copy-rest-of-line nil)
```

Copy from cursor to end the current line to the kill ring.

### copy-whole-line

```lisp
(copy-whole-line nil)
```

Copy the current line to the kill ring.

### csv--to-lists

```lisp
(csv--to-lists (csv))
```

Convert CSV to lists.

### current-buffer-defuns-to-markdown

```lisp
(current-buffer-defuns-to-markdown (file))
```

Create a markdown FILE of all defuns in the current buffer.

### decimal-to-hex

```lisp
(decimal-to-hex (num))
```

Convert NUM to hex.

### decrease-default-font-height

```lisp
(decrease-default-font-height (m))
```

Adjust the default font :height by 10, universal argument is M (to set by multiples).

### decrement-number-at-point

```lisp
(decrement-number-at-point nil)
```

Decrement number at point like vim's Ctrl x.

### delete-frame-or-window-dwim

```lisp
(delete-frame-or-window-dwim nil)
```

Delete the current frame or buffer.
When there is only one frame, kill the buffer.

### delete-this-buffer-and-file

```lisp
(delete-this-buffer-and-file (force))
```

Delete the file connected to this buffer and kill it, FORCE is universal argument.

### describe-thing-at-point

```lisp
(describe-thing-at-point nil)
```

No docstring available: TODO

### dired-find-file-other-window-and-back

```lisp
(dired-find-file-other-window-and-back nil)
```

In Dired, visit this file or directory in another window and remain in first window.

### dired-menu

```lisp
(dired-menu nil)
```

Go to one of the currently open dired buffers (if there is one).

### dired-osx-open-this-file

```lisp
(dired-osx-open-this-file nil)
```

Use the OSX `open' command to launch the current dired file at point.

### dired-visit-library

```lisp
(dired-visit-library (libraryname))
```

Open directory with dired which contain the given LIBRARYNAME.

### docstring-args-to-markdown-code

```lisp
(docstring-args-to-markdown-code (docstring))
```

transform DOCSTRING I arguments to inline markdown `code` style.

### duplicate-current-line-or-region

```lisp
(duplicate-current-line-or-region (arg &optional up))
```

Duplicates the current line or region ARG times.

If UP is non-nil, duplicate and move point to the top.

### duplicate-current-line-or-region-up

```lisp
(duplicate-current-line-or-region-up (arg))
```

Duplicates the current line or region up ARG times.

### elpa-package-insert-ends-here

```lisp
(elpa-package-insert-ends-here nil)
```

Insert the ELPA package file ending string.

(When it's missing)

### eval-and-replace

```lisp
(eval-and-replace nil)
```

Replace the preceding sexp with its value.

### eval-and-replace-prin1

```lisp
(eval-and-replace-prin1 nil)
```

Replace the preceding sexp with its value using prin1.

### flush-blank-lines

```lisp
(flush-blank-lines nil)
```

Flush blank lines.

### format-bin

```lisp
(format-bin (val width))
```

Convert VAL of WIDTH to a binary string.

### format-thousands-separators

```lisp
(format-thousands-separators (n))
```

Format N to have thousand separators.

### fraction-radian

```lisp
(fraction-radian (denominator))
```

Fraction DENOMINATOR of circle to radians.

### generate-markdown-list-of-buffer-defuns

```lisp
(generate-markdown-list-of-buffer-defuns (buffer))
```

Generate markdown text of all defuns in buffer

### generate-untitled-name

```lisp
(generate-untitled-name nil)
```

Generate a name with pattern untitled-n.

### get-defun-info

```lisp
(get-defun-info (buffer))
```

Get information about all `defun' top-level sexps in a buffer
BUFFER. Returns a list with elements of the form (symbol args docstring).

### get-osx-display-resolution

```lisp
(get-osx-display-resolution nil)
```

Get the current display resolution in OSX.

### get-position-of-nearest-matching

```lisp
(get-position-of-nearest-matching (s &optional arg))
```

Get the position of nearest S.

optional ARG when less than zero, default to the before match
when matches are equidistant from the current point.

### git-open-changed-and-new-files

```lisp
(git-open-changed-and-new-files nil)
```

Use git ls-files to open changed files.

### git-open-changed-files

```lisp
(git-open-changed-files nil)
```

Use git ls-files to open changed files.

### git-open-from-ls-files

```lisp
(git-open-from-ls-files (git-ls-options))
```

Use GIT-LS-OPTIONS to open changed files.

### git-open-ls-files

```lisp
(git-open-ls-files (git-ls-options))
```

Use GIT-LS-OPTIONS to open changed files.

### git-open-untracked-files

```lisp
(git-open-untracked-files nil)
```

Use git ls-files to open untracked files.

    Open any untracked file in the repo (unless it's been .gitignored)

### github-browse-repo

```lisp
(github-browse-repo (repo))
```

Browse a github REPO by supplying the user/reponame.

### hex-to-decimal

```lisp
(hex-to-decimal (num))
```

Convert hex NUM to decimal.

### increase-default-font-height

```lisp
(increase-default-font-height (m))
```

Adjust the default font :height by 10, universal argument is M (to set by multiples).

### increment-number-at-point

```lisp
(increment-number-at-point nil)
```

Increment number at point like vim's Ctrl a.

### increment-number-binary

```lisp
(increment-number-binary (&optional arg))
```

Increment the number forward from point by ARG.

### indent-buffer

```lisp
(indent-buffer nil)
```

Indent the current buffer.

### insert-buffer-base-filename

```lisp
(insert-buffer-base-filename nil)
```

Insert the base filename for the current buffer.

If your're in the minibuffer it will use the other buffer file name.

### insert-buffer-filename

```lisp
(insert-buffer-filename nil)
```

Insert the filename for the current buffer.

If your're in the minibuffer it will use the other buffer file name.

### insert-iso8601-date

```lisp
(insert-iso8601-date (&optional date))
```

Insert DATE.

### insert-kill

```lisp
(insert-kill (string))
```

Insert STRING and copy to the kill ring.

### insert-random-in-range

```lisp
(insert-random-in-range (start end))
```

Insert a random number within the range of START and END.

### insert-random-radian

```lisp
(insert-random-radian nil)
```

Insert a radian value from 0 to 6.28318 (2PI : 360 deg).

### insert-sample

```lisp
(insert-sample (strings))
```

Insert a random item from a list of STRINGS.

### insert-time-now

```lisp
(insert-time-now nil)
```

Insert current time.

### join-line-from-below

```lisp
(join-line-from-below nil)
```

Join line from below.

### join-line-or-lines-in-region

```lisp
(join-line-or-lines-in-region nil)
```

Join this line or the lines in the selected region.

### kill-untitled-buffers

```lisp
(kill-untitled-buffers nil)
```

Kill untitled buffers.

### kill-whole-word

```lisp
(kill-whole-word nil)
```

Kill the current word at point.

### magit-just-amend

```lisp
(magit-just-amend nil)
```

Just git commit --amend.

### make-kurecolor-24bit-hue-table

```lisp
(make-kurecolor-24bit-hue-table (color))
```

Make a 24bit color table using Kurecolor.

### make-kurecolor-hue-table

```lisp
(make-kurecolor-hue-table nil)
```

Make a hue table from hex color at top of kill ring, no error checking.

### my-isearch-buffers

```lisp
(my-isearch-buffers nil)
```

Incremental search through open buffers.

### my-multi-occur-in-matching-buffers

```lisp
(my-multi-occur-in-matching-buffers (regexp &optional allbufs))
```

Show all lines matching REGEXP in all buffers.
Optionally check ALLBUFS.

### new-untitled-buffer

```lisp
(new-untitled-buffer nil)
```

Open a new buffer called untitled-n.

### nuke-all-buffers

```lisp
(nuke-all-buffers nil)
```

Kill all buffers, leaving *scratch* only.

### nuke-all-buffers-execept-current

```lisp
(nuke-all-buffers-execept-current nil)
```

Kill all the open buffers except the current one.
Leave *scratch* and *Messages* alone too.

### open-line-above

```lisp
(open-line-above nil)
```

Open a newline above the current point.

### open-line-below

```lisp
(open-line-below nil)
```

Open a newline below the current point.

### open-this-in-intellij-idea-15-osx

```lisp
(open-this-in-intellij-idea-15-osx nil)
```

Open the current file in intellij IDEA 15 (OS X specific).

### open-this-in-xcode

```lisp
(open-this-in-xcode nil)
```

Open the current file in XCode.

### pcre-regexp-from-list-of-words

```lisp
(pcre-regexp-from-list-of-words (words))
```

Insert a pcre regexp to match a list of WORDS.

### random-in-range

```lisp
(random-in-range (start end))
```

Return a random number in range START to END.

### reload-current-chrome-tab-osx

```lisp
(reload-current-chrome-tab-osx nil)
```

Run a simple applescript to reload the current Google Chrome tab.

OSX specific.

### rename-this-buffer-and-file

```lisp
(rename-this-buffer-and-file nil)
```

Renames current buffer and file it is visiting.

### replace-pretty-quotes

```lisp
(replace-pretty-quotes nil)
```

Replace pretty quotes with standard quotes.

### replace-regexp-and-return

```lisp
(replace-regexp-and-return (from to))
```

Replace regexp FROM to TO and return cursor to point.

### replace-region-with

```lisp
(replace-region-with (fn))
```

No docstring available: TODO

### replace-thing-at-point-with

```lisp
(replace-thing-at-point-with (fn))
```

Get the current thing at point.
Replace with the return value of the function FN

### sass-hex-color-to-var

```lisp
(sass-hex-color-to-var nil)
```

Find a hex color, and replace it with a newly created variable name.
Place the created variable at the top of the file.  Name it based
on the property being set, and its CSS selector, and set its
css-value to the hex color found.

### search-backward-wrapped-string

```lisp
(search-backward-wrapped-string (wrap_start wrap_end))
```

Search for a string backwards from the current point.

Use the strings WRAP_START and WRAP_END, to match the start and
end of the string.

if WRAP_END and WRAP_START are equal, we first position the point
at the beginning of the first WRAP_END match, before the initial
point.

The string found between the two wrappers is returned.

This is useful for naive finding of symbols previously defined in
the buffer.

### search-for-nearest-hex-color

```lisp
(search-for-nearest-hex-color (p))
```

Search to the nearest hex color.
Use negative prefix P to go backward.

### set-default-font-height

```lisp
(set-default-font-height (p))
```

Set the default font :height P (prefix arg) or enter in minibuffer.

### set-internal-border

```lisp
(set-internal-border (n))
```

Set or reset the internal border width N of the selected frame.

### shell-command-on-buffer-file

```lisp
(shell-command-on-buffer-file nil)
```

Run a shell command, using the file of current buffer as input.
Return an error if no buffer file.

### shell-command-on-region-replace

```lisp
(shell-command-on-region-replace (start end command))
```

Run `shell-command-on-region' replacing the selected region.  START END COMMAND.

### smart-beginning-of-line

```lisp
(smart-beginning-of-line nil)
```

Move point to first non-whitespace character or `beginning-of-line'.

### snippy-comment

```lisp
(snippy-comment nil)
```

Insert a snip line `- - 8< - - -' comment.

### sort-sexps

```lisp
(sort-sexps (beg end))
```

Sort sexps in region.
Comments stay with the code below.

### switch-to-message-buffer

```lisp
(switch-to-message-buffer nil)
```

Switch to the message buffer.

### switch-to-minibuffer-window

```lisp
(switch-to-minibuffer-window nil)
```

Switch to minibuffer window (if active).

### switch-to-scratch

```lisp
(switch-to-scratch nil)
```

Switch to scratch, grab the region if it's active.

### time-now

```lisp
(time-now nil)
```

current time.

### toggle-window-split

```lisp
(toggle-window-split nil)
```

Toggle the current window split.

### untabify-buffer

```lisp
(untabify-buffer nil)
```

Untabify the current buffer.

### utc-seconds

```lisp
(utc-seconds nil)
```

Insert UTC seconds.

### video-time-to-seconds

```lisp
(video-time-to-seconds (video-time))
```

Convert a VIDEO-TIME formar hh:mm:ss into seconds.

### yank-repeat

```lisp
(yank-repeat (&optional arg))
```

Repeat yank n times ARG.
