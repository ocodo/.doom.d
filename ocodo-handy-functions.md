# Ocodo handy functions

A collection of miscellaneous functions and macros, which are either
candidates to migrate to a minor mode, or will languish here in perpetuity.

Peppered in here are a few gems, some redundancies and some stuff I was just
playing with. They are auto-documented in this markdown document.

Items used often:...

- document-current-elisp-buffer-to-markdown (which generated this page.)
- defun-pcase
- plist-bind
- *-and-replace
- screencapture-mac
- ocodo-custom-key-bindings-to-markdown
- format-multiline

 - - -
## Functions

### -sample

Return a random element from the `list`.

```lisp
(-sample (list))
```
<sup>function signature</sup>
- - -

### align-number-right

Align columns of numbers right in the region (BEGIN, `end`).

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
```


```lisp
(align-number-right (begin end))
```
<sup>function signature</sup>
- - -

### buffer-file-name-to-kill-ring

Save the buffer file name to the kill ring.

```lisp
(buffer-file-name-to-kill-ring)
```
<sup>function signature</sup>
- - -

### change-number-at-point

Change the number at point using `func`.

```lisp
(change-number-at-point (func))
```
<sup>function signature</sup>
- - -

### cleanup-buffer

Perform a cleanup operations on a buffer, tabs to spaces, re-indent, trim whitespace.

```lisp
(cleanup-buffer)
```
<sup>function signature</sup>
- - -

### clear-buffer-text-properties

Clear all text face properties in the buffer.
This is somewhat useful when dealing with text pasted from a
propertied buffer.

Note: this won't turn off face properties in a font-locked buffer.

```lisp
(clear-buffer-text-properties)
```
<sup>function signature</sup>
- - -

### comment-or-uncomment-current-line-or-region

Comments or uncomments the current line or all the lines in region.

```lisp
(comment-or-uncomment-current-line-or-region)
```
<sup>function signature</sup>
- - -

### copy-region-or-rest-of-line-to-other-window

Copy the current region to the other window.

```lisp
(copy-region-or-rest-of-line-to-other-window)
```
<sup>function signature</sup>
- - -

### copy-rest-of-line

Copy from cursor to end the current line to the kill ring.

```lisp
(copy-rest-of-line)
```
<sup>function signature</sup>
- - -

### copy-whole-line

Copy the current line to the kill ring.

```lisp
(copy-whole-line)
```
<sup>function signature</sup>
- - -

### csv-to-lists

Convert simple (very limited) `csv` string to list of lists.

Consider this a basic experiment, which won't be developed.

Use `csv-mode` instead.

For example:

```lisp
(let1 csv (format-multiline
            "|1, 2, 3, Words like this, #ffeeff
            |2, 41, 414, 2002, Foo Bar")
  (csv-to-lists csv))

;; => (("1" "2" "3" "Words like this" "#ffeeff")
;;     ("2" "41" "414" "2002" "Foo Bar"))
```


```lisp
(csv-to-lists (csv))
```
<sup>function signature</sup>
- - -

### cua-rectangle-which-key-help

Display cua-rectangle-keymap in which-key.

```lisp
(cua-rectangle-which-key-help)
```
<sup>function signature</sup>
- - -

### current-buffer-defuns-to-markdown

Create a markdown `file` of all defuns in the current buffer.

```lisp
(current-buffer-defuns-to-markdown (file))
```
<sup>function signature</sup>
- - -

### decimal-to-hex

Convert `num` to hex.

```lisp
(decimal-to-hex (num))
```
<sup>function signature</sup>
- - -

### decrease-default-font-height

Adjust the default font :height by 10, universal argument is M (to set by multiples).

```lisp
(decrease-default-font-height (m))
```
<sup>function signature</sup>
- - -

### decrement-number-at-point

Decrement number at point like vim's Ctrl x.

```lisp
(decrement-number-at-point)
```
<sup>function signature</sup>
- - -

### delete-frame-or-window-dwim

Delete the current frame or buffer.
When there is only one frame, kill the buffer.

```lisp
(delete-frame-or-window-dwim)
```
<sup>function signature</sup>
- - -

### delete-this-buffer-and-file

Delete the file connected to this buffer and kill it, `force` is universal argument.

```lisp
(delete-this-buffer-and-file (force))
```
<sup>function signature</sup>
- - -

### describe-thing-at-point

No docstring available: `todo`

```lisp
(describe-thing-at-point)
```
<sup>function signature</sup>
- - -

### dired-find-file-other-window-and-back

In Dired, visit this file or directory in another window and remain in first window.

```lisp
(dired-find-file-other-window-and-back)
```
<sup>function signature</sup>
- - -

### dired-menu

Go to one of the currently open dired buffers (if there is one).

```lisp
(dired-menu)
```
<sup>function signature</sup>
- - -

### dired-osx-open-this-file

Use the `osx` `open` command to launch the current dired file at point.

```lisp
(dired-osx-open-this-file)
```
<sup>function signature</sup>
- - -

### dired-visit-library

Open directory with dired which contain the given `libraryname`.

```lisp
(dired-visit-library (libraryname))
```
<sup>function signature</sup>
- - -

### docstring-args-to-markdown-code

transform `docstring` arguments to inline markdown `code` style.

```lisp
(docstring-args-to-markdown-code (docstring))
```
<sup>function signature</sup>
- - -

### docstring-back-quoted-to-markdown-code

transform back-quoted docstring elements to inline markdown `code` style.

```lisp
(docstring-back-quoted-to-markdown-code (docstring))
```
<sup>function signature</sup>
- - -

### duplicate-current-line-or-region

Duplicates the current line or region `arg` times.

If `up` is non-nil, duplicate and move point to the top.

```lisp
(duplicate-current-line-or-region (arg &optional up))
```
<sup>function signature</sup>
- - -

### duplicate-current-line-or-region-up

Duplicates the current line or region up `arg` times.

```lisp
(duplicate-current-line-or-region-up (arg))
```
<sup>function signature</sup>
- - -

### elpa-package-insert-ends-here

Insert the `elpa` package file ending string.

(When it's missing)

```lisp
(elpa-package-insert-ends-here)
```
<sup>function signature</sup>
- - -

### eval-and-replace

Replace the preceding sexp with its result.

```lisp
(eval-and-replace)
```
<sup>function signature</sup>
- - -

### eval-and-replace-prin1

Replace the preceding sexp with its value using prin1.

```lisp
(eval-and-replace-prin1)
```
<sup>function signature</sup>
- - -

### filter-recentf

Remove entries matching `pattern` from recent files.
This is operating on the `recentf-list`, in memory.
Use `recentf-save-list` to persist.

```lisp
(filter-recentf (pattern))
```
<sup>function signature</sup>
- - -

### flush-blank-lines

Flush blank lines.

```lisp
(flush-blank-lines)
```
<sup>function signature</sup>
- - -

### format-binary

Convert `val` of `width` to a binary string.
&optional `width` will default to 8.

```lisp
(format-binary (val &optional width))
```
<sup>function signature</sup>
- - -

### format-multiline

Format a  multiline indented `format-string` with `args`.

A multiline string can use leading `|` (pipe) characters to line
up indentation.

ARGS passed will populate format template tokens in the
FORMAT-STRING. Tokens are as defined in `(format ...)`

For example:

```
(fomat-multiline "|- List...
                  |  - Item %s
                  |  - Item %#x
                  |  - Item %x
                  |
                  |... %s"
  "one" 2 #xf "the end")

=>
"- List...
  - Item one
  - Item 0x2
  - Item f

... the end"
```


```lisp
(format-multiline (format-string &rest args))
```
<sup>function signature</sup>
- - -

### format-thousands-separators

Format N to have thousand separators.

For example:

```lisp
(format-thousands-separators 3032498000)
;; => "3,032,498,000"
```


```lisp
(format-thousands-separators (n))
```
<sup>function signature</sup>
- - -

### fraction-radian

Fraction `denominator` of circle to radians.

```lisp
(fraction-radian (denominator))
```
<sup>function signature</sup>
- - -

### generate-markdown-defun-entry

Generate a markdown entry for `fn`.

```lisp
(generate-markdown-defun-entry (fn))
```
<sup>function signature</sup>
- - -

### generate-markdown-list-of-buffer-defuns

Generate markdown text of all defuns in buffer

```lisp
(generate-markdown-list-of-buffer-defuns (buffer))
```
<sup>function signature</sup>
- - -

### generate-markdown-page-of-buffer-defuns

Generate markdown page for all defun in `buffer`.

BUFFER file name and commentary are used as the page heading.

```lisp
(generate-markdown-page-of-buffer-defuns (&optional buffer))
```
<sup>function signature</sup>
- - -

### generate-untitled-name

Generate a name with pattern untitled-n.

```lisp
(generate-untitled-name)
```
<sup>function signature</sup>
- - -

### get-defun-info

Get information about all `defun` top-level sexps in a `buffer`.
Returns a list with elements of the form (symbol args docstring).

```lisp
(get-defun-info (buffer))
```
<sup>function signature</sup>
- - -

### get-osx-display-resolution

Get the current display resolution in `osx`.

Uses the mac system_profiler `SPDisplaysDataType` to lookup the
current display resolution. This is then filtered out (using grep
& perl) and formattted to a list of `(w h)`.

For example:

```lisp
(get-osx-display-resolution)
;; => ("3840" "2160")
```


```lisp
(get-osx-display-resolution)
```
<sup>function signature</sup>
- - -

### get-position-of-nearest-matching

Get the position of nearest S.

optional `arg` when less than zero, default to the before match
when matches are equidistant from the current point.

```lisp
(get-position-of-nearest-matching (s &optional arg))
```
<sup>function signature</sup>
- - -

### get-position-of-nearest-regexp-match

Get the position of nearest `regexp` match.

optional `arg` when less than zero, default to the before match
when matches are equidistant from the current point.

```lisp
(get-position-of-nearest-regexp-match (regexp &optional arg))
```
<sup>function signature</sup>
- - -

### git-open-changed-and-new-files

Use git ls-files to open changed files.

```lisp
(git-open-changed-and-new-files)
```
<sup>function signature</sup>
- - -

### git-open-changed-files

Use git ls-files to open changed files.

```lisp
(git-open-changed-files)
```
<sup>function signature</sup>
- - -

### git-open-from-ls-files

Use `git-ls-options` to open changed files.

```lisp
(git-open-from-ls-files (git-ls-options))
```
<sup>function signature</sup>
- - -

### git-open-ls-files

Use `git-ls-options` to open changed files.

```lisp
(git-open-ls-files (git-ls-options))
```
<sup>function signature</sup>
- - -

### git-open-untracked-files

Use git ls-files to open untracked files.

    Open any untracked file in the repo (unless it's been .gitignored)

```lisp
(git-open-untracked-files)
```
<sup>function signature</sup>
- - -

### github-browse-repo

Browse a github `repo` by supplying the user/reponame.

```lisp
(github-browse-repo (repo))
```
<sup>function signature</sup>
- - -

### hex-to-decimal

Convert hex `num` to decimal.

```lisp
(hex-to-decimal (num))
```
<sup>function signature</sup>
- - -

### increase-default-font-height

Adjust the default font :height by 10, universal argument is M (to set by multiples).

```lisp
(increase-default-font-height (m))
```
<sup>function signature</sup>
- - -

### increment-number-at-point

Increment number at point like vim's Ctrl a.

```lisp
(increment-number-at-point)
```
<sup>function signature</sup>
- - -

### increment-number-binary

Increment the number forward from point by `arg`.

```lisp
(increment-number-binary (&optional arg))
```
<sup>function signature</sup>
- - -

### indent-buffer

Indent the current buffer.

```lisp
(indent-buffer)
```
<sup>function signature</sup>
- - -

### insert-buffer-base-filename

Insert the base filename for the current buffer.

If your're in the minibuffer it will use the other buffer file name.

```lisp
(insert-buffer-base-filename)
```
<sup>function signature</sup>
- - -

### insert-buffer-filename

Insert the filename for the current buffer.

If your're in the minibuffer it will use the other buffer file name.

```lisp
(insert-buffer-filename)
```
<sup>function signature</sup>
- - -

### insert-iso8601-date

Insert `date`.

```lisp
(insert-iso8601-date (&optional date))
```
<sup>function signature</sup>
- - -

### insert-kill

Insert `string` and copy to the kill ring.

```lisp
(insert-kill (string))
```
<sup>function signature</sup>
- - -

### insert-random-in-range

Insert a random number within the range of `start` and `end`.

```lisp
(insert-random-in-range (start end))
```
<sup>function signature</sup>
- - -

### insert-random-radian

Insert a radian value from 0 to 6.28318 (2PI : 360 deg).

```lisp
(insert-random-radian)
```
<sup>function signature</sup>
- - -

### insert-sample

Insert a random item from a list of `strings`.

```lisp
(insert-sample (strings))
```
<sup>function signature</sup>
- - -

### insert-time-now

Insert current time.

```lisp
(insert-time-now)
```
<sup>function signature</sup>
- - -

### int-to-binary-string

convert an integer into it's binary representation in string format

```lisp
(int-to-binary-string (i))
```
<sup>function signature</sup>
- - -

### is-markdown-filename-p

Is the `filename` markdown.

```lisp
(is-markdown-filename-p (filename))
```
<sup>function signature</sup>
- - -

### join-line-from-below

Join line from below.

```lisp
(join-line-from-below)
```
<sup>function signature</sup>
- - -

### join-line-or-lines-in-region

Join this line or the lines in the selected region.

```lisp
(join-line-or-lines-in-region)
```
<sup>function signature</sup>
- - -

### kill-untitled-buffers

Kill untitled buffers.

```lisp
(kill-untitled-buffers)
```
<sup>function signature</sup>
- - -

### kill-whole-word

Kill the current word at point.

```lisp
(kill-whole-word)
```
<sup>function signature</sup>
- - -

### macos-get-list-of-windowids

Get a list of macOS windowids.

```lisp
(macos-get-list-of-windowids)
```
<sup>function signature</sup>
- - -

### macos-get-window-id-of

Get the windowid of `app`.

```lisp
(macos-get-window-id-of (app))
```
<sup>function signature</sup>
- - -

### macos-get-window-id-of-app

Get the windowid of `app`.

```lisp
(macos-get-window-id-of-app (app))
```
<sup>function signature</sup>
- - -

### magit-just-amend

Just git commit --amend.

```lisp
(magit-just-amend)
```
<sup>function signature</sup>
- - -

### make-kurecolor-24bit-hue-table

Make a 24bit color table using Kurecolor.

```lisp
(make-kurecolor-24bit-hue-table (color))
```
<sup>function signature</sup>
- - -

### make-kurecolor-hue-table

Make a hue table from hex color at top of kill ring, no error checking.

```lisp
(make-kurecolor-hue-table)
```
<sup>function signature</sup>
- - -

### markdown-soma-window-arrangement-start

Arrange windows for `markdown-soma-start`.

Internally uses the script `~/.doom.d/bin/emacs-markdown-preview-layout.osa`.

```lisp
(markdown-soma-window-arrangement-start)
```
<sup>function signature</sup>
- - -

### markdown-soma-window-arrangement-stop

Arrange windows for `markdown-soma-stop`.

Internally uses the script `~/.doom.d/bin/emacs-markdown-preview-close.osa`.

```lisp
(markdown-soma-window-arrangement-stop)
```
<sup>function signature</sup>
- - -

### mc/cua-rectangle-to-multiple-cursors

Switch from cua rectangle to multiple cursors.

```lisp
(mc/cua-rectangle-to-multiple-cursors)
```
<sup>function signature</sup>
- - -

### md-code-to-docstring-arg

Replace markdown inline code with docstring arg style in `string`.

For example:

```lisp
(md-code-to-docstring-arg "`code`")
;;  => `code`
```


```lisp
(md-code-to-docstring-arg (string))
```
<sup>function signature</sup>
- - -

### my-isearch-buffers

Incremental search through open buffers.

```lisp
(my-isearch-buffers)
```
<sup>function signature</sup>
- - -

### my-multi-occur-in-matching-buffers

Show all lines matching `regexp` in all buffers.
Optionally check `allbufs`.

```lisp
(my-multi-occur-in-matching-buffers (regexp &optional allbufs))
```
<sup>function signature</sup>
- - -

### new-untitled-buffer

Open a new buffer called untitled-n.

```lisp
(new-untitled-buffer)
```
<sup>function signature</sup>
- - -

### nuke-all-buffers

Kill all buffers, leaving *scratch* only.

```lisp
(nuke-all-buffers)
```
<sup>function signature</sup>
- - -

### nuke-all-buffers-execept-current

Kill all the open buffers except the current one.
Leave *scratch* and *Messages* alone too.

```lisp
(nuke-all-buffers-execept-current)
```
<sup>function signature</sup>
- - -

### ocodo-clean-key-bindings-for-documentation

Prepare collated binding `list` for documentation.

```lisp
(ocodo-clean-key-bindings-for-documentation (binding-list))
```
<sup>function signature</sup>
- - -

### ocodo-collate-key-bindings-for-documentation

Collate all key bindings found in ocodo-key-bindings-lisp-files.

```lisp
(ocodo-collate-key-bindings-for-documentation)
```
<sup>function signature</sup>
- - -

### ocodo-custom-key-bindings-markdown

Generate markdown `file` with table of custom bindings

```lisp
(ocodo-custom-key-bindings-markdown (file))
```
<sup>function signature</sup>
- - -

### ocodo-filter-key-bindings

Filter `bindings` by `filter` on `index`.

```lisp
(ocodo-filter-key-bindings (filter index bindings))
```
<sup>function signature</sup>
- - -

### ocodo-key-binding-groups-to-markdown

Convert `binding-groups` to string of markdown tables.

```lisp
(ocodo-key-binding-groups-to-markdown (binding-groups headings))
```
<sup>function signature</sup>
- - -

### ocodo-key-bindings-for-documentation

Cleaned list of key bindings for documentation.

```lisp
(ocodo-key-bindings-for-documentation)
```
<sup>function signature</sup>
- - -

### ocodo-key-bindings-use-unicode-symbols

KEY-BINDING string directions to unicode arrows.
<up> <down> <left> <right> replaced with ↑ ↓ ← →.
<return> replaced with ⮐.

Setting `white-arrows` to t, gives these replacements: ⇧ ⇩ ⇦ ⇨ and ⏎.

```lisp
(ocodo-key-bindings-use-unicode-symbols (key-binding &optional white-arrows))
```
<sup>function signature</sup>
- - -

### ocodo-make-key-binding-groups

Collect `bindings` and `headings` into `groups`.

```lisp
(ocodo-make-key-binding-groups (bindings headings groups))
```
<sup>function signature</sup>
- - -

### ocodo-make-key-binding-table-row

Make a table row from `binding`.

```lisp
(ocodo-make-key-binding-table-row (binding))
```
<sup>function signature</sup>
- - -

### ocodo-sh-indent-rules

Set up shell script indenation rules engines.

```lisp
(ocodo-sh-indent-rules)
```
<sup>function signature</sup>
- - -

### ocodo-ungrouped-key-bindings

Collect `bindings` and `headings` into `groups`.

```lisp
(ocodo-ungrouped-key-bindings (bindings title groups))
```
<sup>function signature</sup>
- - -

### open-line-above

Open a newline above the current point, without moving.

```lisp
(open-line-above)
```
<sup>function signature</sup>
- - -

### open-line-below

Open a newline below the current point, without moving.

```lisp
(open-line-below)
```
<sup>function signature</sup>
- - -

### open-this-in-intellij-idea-osx

Open the current file in intellij `idea` (OS X specific).

```lisp
(open-this-in-intellij-idea-osx)
```
<sup>function signature</sup>
- - -

### open-this-in-xcode

Open the current file in `xc`ode.

```lisp
(open-this-in-xcode)
```
<sup>function signature</sup>
- - -

### pcre-regexp-from-list-of-words

Insert a pcre regexp to match a list of `words`.

```lisp
(pcre-regexp-from-list-of-words (words))
```
<sup>function signature</sup>
- - -

### random-in-range

Return a random number in range `start` to `end`.

```lisp
(random-in-range (start end))
```
<sup>function signature</sup>
- - -

### reload-current-chrome-tab-osx

Run a simple applescript to reload the current Google Chrome tab.

OSX specific.

```lisp
(reload-current-chrome-tab-osx)
```
<sup>function signature</sup>
- - -

### reload-current-firefox-tab-osx

Run a simple applescript to reload the current Google Chrome tab.

OSX specific.

```lisp
(reload-current-firefox-tab-osx)
```
<sup>function signature</sup>
- - -

### rename-this-buffer-and-file

Renames current buffer and file it is visiting.

```lisp
(rename-this-buffer-and-file)
```
<sup>function signature</sup>
- - -

### replace-pretty-quotes

Replace pretty quotes with standard quotes.

```lisp
(replace-pretty-quotes)
```
<sup>function signature</sup>
- - -

### replace-regexp-and-return

Replace regexp `from` to `to` and return cursor to point.

```lisp
(replace-regexp-and-return (from to))
```
<sup>function signature</sup>
- - -

### replace-region-with

No docstring available: `todo`

```lisp
(replace-region-with (fn))
```
<sup>function signature</sup>
- - -

### replace-thing-at-point-with

Get the current thing at point.
Replace with the return value of the function `fn`

```lisp
(replace-thing-at-point-with (fn))
```
<sup>function signature</sup>
- - -

### revert-buffer-instant

Revert buffer without prompting.

```lisp
(revert-buffer-instant)
```
<sup>function signature</sup>
- - -

### s-squeeze

Squeeze the occurences of `char` in `string`.
This works the same as `tr -s `char``.

```lisp
(s-squeeze (char string))
```
<sup>function signature</sup>
- - -

### sass-hex-color-to-var

Find a hex color, and replace it with a newly created variable name.
Place the created variable at the top of the file.  Name it based
on the property being set, and its `css` selector, and set its
css-value to the hex color found.

```lisp
(sass-hex-color-to-var)
```
<sup>function signature</sup>
- - -

### screencapture-mac

Screencapture on macOS, interactive or supply `commandline` and `file_keyword`.

```lisp
(screencapture-mac (&optional commandline file-keyword))
```
<sup>function signature</sup>
- - -

### screencapture-mac--complete-arguments-for-option

Do completeing read for arguments of option.

```lisp
(screencapture-mac--complete-arguments-for-option (plist))
```
<sup>function signature</sup>
- - -

### screencapture-mac--entry-from-summaries

No docstring available: `todo`

```lisp
(screencapture-mac--entry-from-summaries (summaries))
```
<sup>function signature</sup>
- - -

### screencapture-mac--filename-generator

Generate a filename for the screenshot at `path` with optional `ext` and `file_keyword`.

```lisp
(screencapture-mac--filename-generator (path &optional ext file-keyword))
```
<sup>function signature</sup>
- - -

### screencapture-mac--get-option

Fetch the option from `summary`

```lisp
(screencapture-mac--get-option (summary))
```
<sup>function signature</sup>
- - -

### screencapture-mac--options

Command line options for screencapture (macOS).

```lisp
(screencapture-mac--options)
```
<sup>function signature</sup>
- - -

### screencapture-mac--options-summary

No docstring available: `todo`

```lisp
(screencapture-mac--options-summary (plist))
```
<sup>function signature</sup>
- - -

### screencapture-mac--run

Execute the shell `command` with `filename`.

```lisp
(screencapture-mac--run (command filename))
```
<sup>function signature</sup>
- - -

### screencapture-mac--summary-list

Summarized list of screencapture mac options

```lisp
(screencapture-mac--summary-list)
```
<sup>function signature</sup>
- - -

### screencapture-mac--windowid-helper

Get the windowid from a completing-read list.

```lisp
(screencapture-mac--windowid-helper)
```
<sup>function signature</sup>
- - -

### screencapture-mac-reset-default-commandline

Reset the default commandline

```lisp
(screencapture-mac-reset-default-commandline)
```
<sup>function signature</sup>
- - -

### search-backward-wrapped-string

Search for a string backwards from the current point.

Use the strings `wrap_start` and `wrap_end`, to match the start and
end of the string.

if `wrap_end` and `wrap_start` are equal, we first position the point
at the beginning of the first `wrap_end` match, before the initial
point.

The string found between the two wrappers is returned.

This is useful for naive finding of symbols previously defined in
the buffer.

```lisp
(search-backward-wrapped-string (wrap_start wrap_end))
```
<sup>function signature</sup>
- - -

### search-for-nearest-hex-color

Search to the nearest hex color.
Use negative prefix P to go backward.

```lisp
(search-for-nearest-hex-color (p))
```
<sup>function signature</sup>
- - -

### set-default-font-height

Set the default font :height P (prefix arg) or enter in minibuffer.

```lisp
(set-default-font-height (p))
```
<sup>function signature</sup>
- - -

### set-internal-border

Set or reset the internal border width N of the selected frame.

```lisp
(set-internal-border (n))
```
<sup>function signature</sup>
- - -

### shell-command-on-buffer-file

Run a shell command, using the file of current buffer as input.
Return an error if no buffer file.

```lisp
(shell-command-on-buffer-file)
```
<sup>function signature</sup>
- - -

### shell-command-on-region-replace

Run `shell-command-on-region` replacing the selected region. `start` `end` `command`.

```lisp
(shell-command-on-region-replace (start end command))
```
<sup>function signature</sup>
- - -

### smart-beginning-of-line

Move point to first non-whitespace character or `beginning-of-line`.

```lisp
(smart-beginning-of-line)
```
<sup>function signature</sup>
- - -

### snippy-comment

Insert a snip line `- - 8< - - -` comment.

```lisp
(snippy-comment)
```
<sup>function signature</sup>
- - -

### sort-sexps

Sort sexps in region.
Comments stay with the code below.

```lisp
(sort-sexps (beg end))
```
<sup>function signature</sup>
- - -

### ssh-agent-env-fix

Ensure $SSH_AUTH_SOCK is set correctly in the environment.

```lisp
(ssh-agent-env-fix)
```
<sup>function signature</sup>
- - -

### switch-to-message-buffer

Switch to the message buffer.

```lisp
(switch-to-message-buffer)
```
<sup>function signature</sup>
- - -

### switch-to-minibuffer-window

Switch to minibuffer window (if active).

```lisp
(switch-to-minibuffer-window)
```
<sup>function signature</sup>
- - -

### switch-to-scratch

Switch to scratch, grab the region if it's active.

```lisp
(switch-to-scratch)
```
<sup>function signature</sup>
- - -

### time-now

current time.

```lisp
(time-now)
```
<sup>function signature</sup>
- - -

### time-to-seconds

Convert `time` `hh:mm:ss` into seconds.

```lisp
(time-to-seconds (time))
```
<sup>function signature</sup>
- - -

### toggle-window-split

Toggle the current window split.

```lisp
(toggle-window-split)
```
<sup>function signature</sup>
- - -

### untabify-buffer

Untabify the current buffer.

```lisp
(untabify-buffer)
```
<sup>function signature</sup>
- - -

### utc-seconds

Insert `utc` seconds.

```lisp
(utc-seconds)
```
<sup>function signature</sup>
- - -
