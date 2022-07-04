### -sample

Return a random element from the LIST.

```lisp
(-sample (list))
```

### align-number-right

Align region to equal signs from BEGIN to END.

```lisp
(align-number-right (begin end))
```

### buffer-file-name-to-kill-ring

Save the buffer file name to the kill ring.

```lisp
(buffer-file-name-to-kill-ring )
```

### change-number-at-point

Change the number at point using FUNC.

```lisp
(change-number-at-point (func))
```

### cleanup-buffer

Perform a cleanup operations on a buffer, tabs to spaces, re-indent, trim whitespace.

```lisp
(cleanup-buffer )
```

### clear-buffer-text-properties

Clear all text face properties in the buffer.
This is somewhat useful when dealing with text pasted from a
propertied buffer.

Note: this won't turn off face properties in a font-locked buffer.

```lisp
(clear-buffer-text-properties )
```

### comment-or-uncomment-current-line-or-region

Comments or uncomments the current line or all the lines in region.

```lisp
(comment-or-uncomment-current-line-or-region )
```

### copy-region-or-rest-of-line-to-other-window

Copy the current region to the other window.

```lisp
(copy-region-or-rest-of-line-to-other-window )
```

### copy-rest-of-line

Copy from cursor to end the current line to the kill ring.

```lisp
(copy-rest-of-line )
```

### copy-whole-line

Copy the current line to the kill ring.

```lisp
(copy-whole-line )
```

### csv--to-lists

Convert CSV to lists.

```lisp
(csv--to-lists (csv))
```

### current-buffer-defuns-to-markdown

Create a markdown FILE of all defuns in the current buffer.

```lisp
(current-buffer-defuns-to-markdown (file))
```

### decimal-to-hex

Convert NUM to hex.

```lisp
(decimal-to-hex (num))
```

### decrease-default-font-height

Adjust the default font :height by 10, universal argument is M (to set by multiples).

```lisp
(decrease-default-font-height (m))
```

### decrement-number-at-point

Decrement number at point like vim's Ctrl x.

```lisp
(decrement-number-at-point )
```

### delete-frame-or-window-dwim

Delete the current frame or buffer.
When there is only one frame, kill the buffer.

```lisp
(delete-frame-or-window-dwim )
```

### delete-this-buffer-and-file

Delete the file connected to this buffer and kill it, FORCE is universal argument.

```lisp
(delete-this-buffer-and-file (force))
```

### describe-thing-at-point

No docstring available: TODO

```lisp
(describe-thing-at-point )
```

### dired-find-file-other-window-and-back

In Dired, visit this file or directory in another window and remain in first window.

```lisp
(dired-find-file-other-window-and-back )
```

### dired-menu

Go to one of the currently open dired buffers (if there is one).

```lisp
(dired-menu )
```

### dired-osx-open-this-file

Use the OSX `open' command to launch the current dired file at point.

```lisp
(dired-osx-open-this-file )
```

### dired-visit-library

Open directory with dired which contain the given LIBRARYNAME.

```lisp
(dired-visit-library (libraryname))
```

### docstring-args-to-markdown-code

transform DOCSTRING I arguments to inline markdown `code` style.

```lisp
(docstring-args-to-markdown-code (docstring))
```

### docstring-back-quoted-to-markdown-code

transform back-quoted docstring elements to inline markdown `code` style.

```lisp
(docstring-back-quoted-to-markdown-code (docstring))
```

### duplicate-current-line-or-region

Duplicates the current line or region ARG times.

If UP is non-nil, duplicate and move point to the top.

```lisp
(duplicate-current-line-or-region (arg &optional up))
```

### duplicate-current-line-or-region-up

Duplicates the current line or region up ARG times.

```lisp
(duplicate-current-line-or-region-up (arg))
```

### elpa-package-insert-ends-here

Insert the ELPA package file ending string.

(When it's missing)

```lisp
(elpa-package-insert-ends-here )
```

### eval-and-replace

Replace the preceding sexp with its value.

```lisp
(eval-and-replace )
```

### eval-and-replace-prin1

Replace the preceding sexp with its value using prin1.

```lisp
(eval-and-replace-prin1 )
```

### filter-recentf

Remove entries matching PATTERN from recent files.
This is operating on the recentf-list, in memory.
Use recentf-save-list to persist.

```lisp
(filter-recentf (pattern))
```

### flush-blank-lines

Flush blank lines.

```lisp
(flush-blank-lines )
```

### format-binary

Convert VAL of WIDTH to a binary string.
&optional WIDTH will default to 8.

```lisp
(format-binary (val &optional width))
```

### format-thousands-separators

Format N to have thousand separators.

```lisp
(format-thousands-separators (n))
```

### fraction-radian

Fraction DENOMINATOR of circle to radians.

```lisp
(fraction-radian (denominator))
```

### generate-markdown-list-of-buffer-defuns

Generate markdown text of all defuns in buffer

```lisp
(generate-markdown-list-of-buffer-defuns (buffer))
```

### generate-untitled-name

Generate a name with pattern untitled-n.

```lisp
(generate-untitled-name )
```

### get-defun-info

Get information about all `defun' top-level sexps in a buffer
BUFFER. Returns a list with elements of the form (symbol args docstring).

```lisp
(get-defun-info (buffer))
```

### get-osx-display-resolution

Get the current display resolution in OSX.

```lisp
(get-osx-display-resolution )
```

### get-position-of-nearest-matching

Get the position of nearest S.

optional ARG when less than zero, default to the before match
when matches are equidistant from the current point.

```lisp
(get-position-of-nearest-matching (s &optional arg))
```

### get-position-of-nearest-regexp-match

Get the position of nearest REGEXP match.

optional ARG when less than zero, default to the before match
when matches are equidistant from the current point.

```lisp
(get-position-of-nearest-regexp-match (regexp &optional arg))
```

### git-open-changed-and-new-files

Use git ls-files to open changed files.

```lisp
(git-open-changed-and-new-files )
```

### git-open-changed-files

Use git ls-files to open changed files.

```lisp
(git-open-changed-files )
```

### git-open-from-ls-files

Use GIT-LS-OPTIONS to open changed files.

```lisp
(git-open-from-ls-files (git-ls-options))
```

### git-open-ls-files

Use GIT-LS-OPTIONS to open changed files.

```lisp
(git-open-ls-files (git-ls-options))
```

### git-open-untracked-files

Use git ls-files to open untracked files.

    Open any untracked file in the repo (unless it's been .gitignored)

```lisp
(git-open-untracked-files )
```

### github-browse-repo

Browse a github REPO by supplying the user/reponame.

```lisp
(github-browse-repo (repo))
```

### hex-to-decimal

Convert hex NUM to decimal.

```lisp
(hex-to-decimal (num))
```

### increase-default-font-height

Adjust the default font :height by 10, universal argument is M (to set by multiples).

```lisp
(increase-default-font-height (m))
```

### increment-number-at-point

Increment number at point like vim's Ctrl a.

```lisp
(increment-number-at-point )
```

### increment-number-binary

Increment the number forward from point by ARG.

```lisp
(increment-number-binary (&optional arg))
```

### indent-buffer

Indent the current buffer.

```lisp
(indent-buffer )
```

### insert-buffer-base-filename

Insert the base filename for the current buffer.

If your're in the minibuffer it will use the other buffer file name.

```lisp
(insert-buffer-base-filename )
```

### insert-buffer-filename

Insert the filename for the current buffer.

If your're in the minibuffer it will use the other buffer file name.

```lisp
(insert-buffer-filename )
```

### insert-iso8601-date

Insert DATE.

```lisp
(insert-iso8601-date (&optional date))
```

### insert-kill

Insert STRING and copy to the kill ring.

```lisp
(insert-kill (string))
```

### insert-random-in-range

Insert a random number within the range of START and END.

```lisp
(insert-random-in-range (start end))
```

### insert-random-radian

Insert a radian value from 0 to 6.28318 (2PI : 360 deg).

```lisp
(insert-random-radian )
```

### insert-sample

Insert a random item from a list of STRINGS.

```lisp
(insert-sample (strings))
```

### insert-time-now

Insert current time.

```lisp
(insert-time-now )
```

### int-to-binary-string

convert an integer into it's binary representation in string format

```lisp
(int-to-binary-string (i))
```

### join-line-from-below

Join line from below.

```lisp
(join-line-from-below )
```

### join-line-or-lines-in-region

Join this line or the lines in the selected region.

```lisp
(join-line-or-lines-in-region )
```

### kill-untitled-buffers

Kill untitled buffers.

```lisp
(kill-untitled-buffers )
```

### kill-whole-word

Kill the current word at point.

```lisp
(kill-whole-word )
```

### macos-get-list-of-windowids

Get a list of macOS windowids.

```lisp
(macos-get-list-of-windowids )
```

### macos-get-window-id-of

Get the windowid of APP.

```lisp
(macos-get-window-id-of (app))
```

### macos-get-window-id-of-app

Get the windowid of APP.

```lisp
(macos-get-window-id-of-app (app))
```

### magit-just-amend

Just git commit --amend.

```lisp
(magit-just-amend )
```

### make-kurecolor-24bit-hue-table

Make a 24bit color table using Kurecolor.

```lisp
(make-kurecolor-24bit-hue-table (color))
```

### make-kurecolor-hue-table

Make a hue table from hex color at top of kill ring, no error checking.

```lisp
(make-kurecolor-hue-table )
```

### mc/cua-rectangle-to-multiple-cursors

No docstring available: TODO

```lisp
(mc/cua-rectangle-to-multiple-cursors )
```

### my-isearch-buffers

Incremental search through open buffers.

```lisp
(my-isearch-buffers )
```

### my-multi-occur-in-matching-buffers

Show all lines matching REGEXP in all buffers.
Optionally check ALLBUFS.

```lisp
(my-multi-occur-in-matching-buffers (regexp &optional allbufs))
```

### new-untitled-buffer

Open a new buffer called untitled-n.

```lisp
(new-untitled-buffer )
```

### nuke-all-buffers

Kill all buffers, leaving *scratch* only.

```lisp
(nuke-all-buffers )
```

### nuke-all-buffers-execept-current

Kill all the open buffers except the current one.
Leave *scratch* and *Messages* alone too.

```lisp
(nuke-all-buffers-execept-current )
```

### open-line-above

Open a newline above the current point.

```lisp
(open-line-above )
```

### open-line-below

Open a newline below the current point.

```lisp
(open-line-below )
```

### open-this-in-intellij-idea-15-osx

Open the current file in intellij IDEA 15 (OS X specific).

```lisp
(open-this-in-intellij-idea-15-osx )
```

### open-this-in-xcode

Open the current file in XCode.

```lisp
(open-this-in-xcode )
```

### pcre-regexp-from-list-of-words

Insert a pcre regexp to match a list of WORDS.

```lisp
(pcre-regexp-from-list-of-words (words))
```

### random-in-range

Return a random number in range START to END.

```lisp
(random-in-range (start end))
```

### reload-current-chrome-tab-osx

Run a simple applescript to reload the current Google Chrome tab.

OSX specific.

```lisp
(reload-current-chrome-tab-osx )
```

### rename-this-buffer-and-file

Renames current buffer and file it is visiting.

```lisp
(rename-this-buffer-and-file )
```

### replace-pretty-quotes

Replace pretty quotes with standard quotes.

```lisp
(replace-pretty-quotes )
```

### replace-regexp-and-return

Replace regexp FROM to TO and return cursor to point.

```lisp
(replace-regexp-and-return (from to))
```

### replace-region-with

No docstring available: TODO

```lisp
(replace-region-with (fn))
```

### replace-thing-at-point-with

Get the current thing at point.
Replace with the return value of the function FN

```lisp
(replace-thing-at-point-with (fn))
```

### s-squeeze

Squeeze the occurences of CHAR in STRING.
This works the same as `tr -s CHAR`.

```lisp
(s-squeeze (char string))
```

### sass-hex-color-to-var

Find a hex color, and replace it with a newly created variable name.
Place the created variable at the top of the file.  Name it based
on the property being set, and its CSS selector, and set its
css-value to the hex color found.

```lisp
(sass-hex-color-to-var )
```

### screencapture-mac

Screencapture on macOS, interactive or supply COMMANDLINE and FILE_KEYWORD.

```lisp
(screencapture-mac (&optional commandline file-keyword))
```

### screencapture-mac--complete-arguments-for-option

Do completeing read for arguments of option.

```lisp
(screencapture-mac--complete-arguments-for-option (plist))
```

### screencapture-mac--entry-from-summaries

No docstring available: TODO

```lisp
(screencapture-mac--entry-from-summaries (summaries))
```

### screencapture-mac--filename-generator

Generate a filename for the screenshot at PATH with optional EXT and FILE_KEYWORD.

```lisp
(screencapture-mac--filename-generator (path &optional ext file-keyword))
```

### screencapture-mac--get-option

Fetch the option from SUMMARY

```lisp
(screencapture-mac--get-option (summary))
```

### screencapture-mac--options

Command line options for screencapture (macOS).

```lisp
(screencapture-mac--options )
```

### screencapture-mac--options-summary

No docstring available: TODO

```lisp
(screencapture-mac--options-summary (plist))
```

### screencapture-mac--run

Execute the shell COMMAND with FILENAME.

```lisp
(screencapture-mac--run (command filename))
```

### screencapture-mac--summary-list

Summarized list of screencapture mac options

```lisp
(screencapture-mac--summary-list )
```

### screencapture-mac--windowid-helper

Get the windowid from a completing-read list.

```lisp
(screencapture-mac--windowid-helper )
```

### screencapture-mac-reset-default-commandline

Reset the default commandline

```lisp
(screencapture-mac-reset-default-commandline )
```

### search-backward-wrapped-string

Search for a string backwards from the current point.

Use the strings WRAP_START and WRAP_END, to match the start and
end of the string.

if WRAP_END and WRAP_START are equal, we first position the point
at the beginning of the first WRAP_END match, before the initial
point.

The string found between the two wrappers is returned.

This is useful for naive finding of symbols previously defined in
the buffer.

```lisp
(search-backward-wrapped-string (wrap_start wrap_end))
```

### search-for-nearest-hex-color

Search to the nearest hex color.
Use negative prefix P to go backward.

```lisp
(search-for-nearest-hex-color (p))
```

### set-default-font-height

Set the default font :height P (prefix arg) or enter in minibuffer.

```lisp
(set-default-font-height (p))
```

### set-internal-border

Set or reset the internal border width N of the selected frame.

```lisp
(set-internal-border (n))
```

### shell-command-on-buffer-file

Run a shell command, using the file of current buffer as input.
Return an error if no buffer file.

```lisp
(shell-command-on-buffer-file )
```

### shell-command-on-region-replace

Run `shell-command-on-region' replacing the selected region.  START END COMMAND.

```lisp
(shell-command-on-region-replace (start end command))
```

### smart-beginning-of-line

Move point to first non-whitespace character or `beginning-of-line'.

```lisp
(smart-beginning-of-line )
```

### snippy-comment

Insert a snip line `- - 8< - - -' comment.

```lisp
(snippy-comment )
```

### sort-sexps

Sort sexps in region.
Comments stay with the code below.

```lisp
(sort-sexps (beg end))
```

### switch-to-message-buffer

Switch to the message buffer.

```lisp
(switch-to-message-buffer )
```

### switch-to-minibuffer-window

Switch to minibuffer window (if active).

```lisp
(switch-to-minibuffer-window )
```

### switch-to-scratch

Switch to scratch, grab the region if it's active.

```lisp
(switch-to-scratch )
```

### time-now

current time.

```lisp
(time-now )
```

### time-to-seconds

Convert TIME `hh:mm:ss' into seconds.

```lisp
(time-to-seconds (time))
```

### toggle-window-split

Toggle the current window split.

```lisp
(toggle-window-split )
```

### untabify-buffer

Untabify the current buffer.

```lisp
(untabify-buffer )
```

### utc-seconds

Insert UTC seconds.

```lisp
(utc-seconds )
```

### yank-repeat

Repeat yank n times ARG.

```lisp
(yank-repeat (&optional arg))
```
