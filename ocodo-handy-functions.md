# Ocodo handy functions

A collection of miscellaneous functions and macros, which are either
candidates to migrate to a minor mode, or will languish here in perpetuity.

Peppered in here are a few gems, some redundancies and some stuff I was just
playing with. They are auto-documented in this markdown document.

Items used often:...

- `current-buffer-defuns-to-markdown` (which generated this page.)
- `defun-pcase`
- `plist-bind`
- `*-and-replace`
- `screencapture-mac`
- `ocodo-custom-key-bindings-to-markdown`
- `format-multiline`

 - - -
## Functions

### -sample

Return a random element from the `list`.

<sup>function signature</sup>
```lisp
(-sample (list))
```

- - -

### align-number-right [command]

Align columns of numbers right in the region (`begin`, `end`).

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


<sup>function signature</sup>
```lisp
(align-number-right (begin end))
```

- - -

### buffer-file-name-to-kill-ring [command]

Save the buffer file name to the kill ring.

<sup>function signature</sup>
```lisp
(buffer-file-name-to-kill-ring)
```

- - -

### change-number-at-point

Change the number at point using `func`.

It should be wrapped in an interactive function, and func should
take a single numeric argument and return anything.

For example:

```lisp
(defun round-number-at-point ()
"Round the number at point."
  (interactive)
  (change-number-at-point #'round))

;; Or...

(defun number-at-point-to-currency ()
 "Change the number at point to currency."
  (format "$%.2f" (number-at-point))))
```


<sup>function signature</sup>
```lisp
(change-number-at-point (func))
```

- - -

### cleanup-buffer [command]

Perform a cleanup operations on a buffer, tabs to spaces, re-indent, trim whitespace.

<sup>function signature</sup>
```lisp
(cleanup-buffer)
```

- - -

### clear-buffer-text-properties [command]

Clear all text face properties in the buffer.
This is somewhat useful when dealing with text pasted from a
propertied buffer.

Note: this won't turn off face properties in a font-locked buffer.

<sup>function signature</sup>
```lisp
(clear-buffer-text-properties)
```

- - -

### comment-or-uncomment-current-line-or-region [command]

Comments or uncomments the current line or all the lines in region.

<sup>function signature</sup>
```lisp
(comment-or-uncomment-current-line-or-region)
```

- - -

### copy-region-or-rest-of-line-to-other-window [command]

Copy the current region to the other window.

<sup>function signature</sup>
```lisp
(copy-region-or-rest-of-line-to-other-window)
```

- - -

### copy-rest-of-line [command]

Copy from cursor to end the current line to the kill ring.

<sup>function signature</sup>
```lisp
(copy-rest-of-line)
```

- - -

### copy-whole-line [command]

Copy the current line to the kill ring.

<sup>function signature</sup>
```lisp
(copy-whole-line)
```

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


<sup>function signature</sup>
```lisp
(csv-to-lists (csv))
```

- - -

### cua-rectangle-which-key-help [command]

Display cua-rectangle-keymap in which-key.

<sup>function signature</sup>
```lisp
(cua-rectangle-which-key-help)
```

- - -

### decimal-to-hex

Convert `num` to hex.

<sup>function signature</sup>
```lisp
(decimal-to-hex (num))
```

- - -

### decrease-default-font-height [command]

Adjust the default font :height by 10, universal argument is `m` (to set by multiples).

<sup>function signature</sup>
```lisp
(decrease-default-font-height (m))
```

- - -

### decrement-number-at-point [command]

Decrement number at point like vim's Ctrl x.

<sup>function signature</sup>
```lisp
(decrement-number-at-point)
```

- - -

### delete-frame-or-window-dwim [command]

Delete the current frame or buffer.
When there is only one frame, kill the buffer.

<sup>function signature</sup>
```lisp
(delete-frame-or-window-dwim)
```

- - -

### delete-this-buffer-and-file [command]

Delete the file connected to this buffer and kill it, `force` is universal argument.

<sup>function signature</sup>
```lisp
(delete-this-buffer-and-file (force))
```

- - -

### describe-thing-at-point

No docstring available: TODO

<sup>function signature</sup>
```lisp
(describe-thing-at-point)
```

- - -

### dired-find-file-other-window-and-back [command]

In Dired, visit this file or directory in another window and remain in first window.

<sup>function signature</sup>
```lisp
(dired-find-file-other-window-and-back)
```

- - -

### dired-menu [command]

Go to one of the currently open dired buffers (if there is one).

<sup>function signature</sup>
```lisp
(dired-menu)
```

- - -

### dired-osx-open-this-file [command]

Use the OSX `open` command to launch the current dired file at point.

<sup>function signature</sup>
```lisp
(dired-osx-open-this-file)
```

- - -

### dired-visit-library [command]

Open directory with dired which contain the given `libraryname`.

<sup>function signature</sup>
```lisp
(dired-visit-library (libraryname))
```

- - -

### duplicate-current-line-or-region [command]

Duplicates the current line or region `arg` times.

If `up` is non-nil, duplicate and move point to the top.

<sup>function signature</sup>
```lisp
(duplicate-current-line-or-region (arg &optional up))
```

- - -

### duplicate-current-line-or-region-up [command]

Duplicates the current line or region up `arg` times.

<sup>function signature</sup>
```lisp
(duplicate-current-line-or-region-up (arg))
```

- - -

### eval-and-replace [command]

Replace the preceding sexp with its result.

<sup>function signature</sup>
```lisp
(eval-and-replace)
```

- - -

### eval-and-replace-prin1 [command]

Replace the preceding sexp with its value using prin1.

<sup>function signature</sup>
```lisp
(eval-and-replace-prin1)
```

- - -

### filter-recentf [command]

Remove entries matching `pattern` from recent files.
This is operating on the `recentf-list`, in memory.
Use `recentf-save-list` to persist.

<sup>function signature</sup>
```lisp
(filter-recentf (pattern))
```

- - -

### flush-blank-lines [command]

Flush blank lines.

<sup>function signature</sup>
```lisp
(flush-blank-lines)
```

- - -

### format-binary

Convert `val` of `width` to a binary string.
&optional `width` will default to 8.

<sup>function signature</sup>
```lisp
(format-binary (val &optional width))
```

- - -

### format-multiline

Format a multiline indented `format-string` with `args`.

A multiline string can use leading `|` (pipe) characters to line
up indentation.

`args` passed will populate format template tokens in the
`format-string`. Tokens are as defined in `(format ...)`

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


<sup>function signature</sup>
```lisp
(format-multiline (format-string &rest args))
```

- - -

### format-thousands-separators

Format `n` to have thousand separators.

For example:

```lisp
(format-thousands-separators 3032498000)
;; => "3,032,498,000"
```


<sup>function signature</sup>
```lisp
(format-thousands-separators (n))
```

- - -

### fraction-radian [command]

Fraction `denominator` of circle to radians.

<sup>function signature</sup>
```lisp
(fraction-radian (denominator))
```

- - -

### generate-untitled-name

Generate a name with pattern untitled-n.

<sup>function signature</sup>
```lisp
(generate-untitled-name)
```

- - -

### get-osx-display-resolution

Get the current display resolution in OSX.

Uses the mac system_profiler `SPDisplaysDataType` to lookup the
current display resolution. This is then filtered out (using grep
& perl) and formattted to a list of `(w h)`.

For example:

```lisp
(get-osx-display-resolution)
;; => ("3840" "2160")
```


<sup>function signature</sup>
```lisp
(get-osx-display-resolution)
```

- - -

### get-position-of-nearest-matching

Get the position of nearest `s`.

optional `arg` when less than zero, default to the before match
when matches are equidistant from the current point.

<sup>function signature</sup>
```lisp
(get-position-of-nearest-matching (s &optional arg))
```

- - -

### get-position-of-nearest-regexp-match

Get the position of nearest `regexp` match.

optional `arg` when less than zero, default to the before match
when matches are equidistant from the current point.

<sup>function signature</sup>
```lisp
(get-position-of-nearest-regexp-match (regexp &optional arg))
```

- - -

### git-open-changed-and-new-files [command]

Use git ls-files to open changed files.

<sup>function signature</sup>
```lisp
(git-open-changed-and-new-files)
```

- - -

### git-open-changed-files [command]

Use git ls-files to open changed files.

<sup>function signature</sup>
```lisp
(git-open-changed-files)
```

- - -

### git-open-from-ls-files [command]

Use `git-ls-options` to open changed files.

<sup>function signature</sup>
```lisp
(git-open-from-ls-files (git-ls-options))
```

- - -

### git-open-ls-files [command]

Use `git-ls-options` to open changed files.

<sup>function signature</sup>
```lisp
(git-open-ls-files (git-ls-options))
```

- - -

### git-open-untracked-files [command]

Use git ls-files to open untracked files.

    Open any untracked file in the repo (unless it's been .gitignored)

<sup>function signature</sup>
```lisp
(git-open-untracked-files)
```

- - -

### github-browse-repo [command]

Browse a github `repo` by supplying the user/reponame.

<sup>function signature</sup>
```lisp
(github-browse-repo (repo))
```

- - -

### google-en-to-thai

Translate `text` from English to Thai.

<sup>function signature</sup>
```lisp
(google-en-to-thai (text))
```

- - -

### google-en-to-thai-on-region [command]

Translate english in region (`begin` `end`) to Thai.

<sup>function signature</sup>
```lisp
(google-en-to-thai-on-region (begin end))
```

- - -

### hex-to-decimal

Convert hex `num` to decimal.

<sup>function signature</sup>
```lisp
(hex-to-decimal (num))
```

- - -

### increase-default-font-height [command]

Adjust the default font :height by 10, universal argument is `m` (to set by multiples).

<sup>function signature</sup>
```lisp
(increase-default-font-height (m))
```

- - -

### increment-number-at-point [command]

Increment number at point like vim's Ctrl a.

<sup>function signature</sup>
```lisp
(increment-number-at-point)
```

- - -

### increment-number-binary [command]

Increment the number forward from point by `arg`.

<sup>function signature</sup>
```lisp
(increment-number-binary (&optional arg))
```

- - -

### indent-buffer [command]

Indent the current buffer.

<sup>function signature</sup>
```lisp
(indent-buffer)
```

- - -

### insert-buffer-base-filename [command]

Insert the base filename for the current buffer.

If your're in the minibuffer it will use the other buffer file name.

<sup>function signature</sup>
```lisp
(insert-buffer-base-filename)
```

- - -

### insert-buffer-filename [command]

Insert the filename for the current buffer.

If your're in the minibuffer it will use the other buffer file name.

<sup>function signature</sup>
```lisp
(insert-buffer-filename)
```

- - -

### insert-iso8601-date [command]

Insert `date`.

<sup>function signature</sup>
```lisp
(insert-iso8601-date (&optional date))
```

- - -

### insert-kill [command]

Insert `string` and copy to the kill ring.

<sup>function signature</sup>
```lisp
(insert-kill (string))
```

- - -

### insert-random-in-range [command]

Insert a random number within the range of `start` and `end`.

<sup>function signature</sup>
```lisp
(insert-random-in-range (start end))
```

- - -

### insert-random-radian [command]

Insert a radian value from 0 to 6.28318 (2PI : 360 deg).

<sup>function signature</sup>
```lisp
(insert-random-radian)
```

- - -

### insert-sample [command]

Insert a random item from a list of `strings`.

<sup>function signature</sup>
```lisp
(insert-sample (strings))
```

- - -

### insert-time-now [command]

Insert current time.

<sup>function signature</sup>
```lisp
(insert-time-now)
```

- - -

### int-to-binary-string

convert an integer into it's binary representation in string format

<sup>function signature</sup>
```lisp
(int-to-binary-string (i))
```

- - -

### is-markdown-filename-p

Is the `filename` markdown.

<sup>function signature</sup>
```lisp
(is-markdown-filename-p (filename))
```

- - -

### join-line-from-below [command]

Join line from below.

<sup>function signature</sup>
```lisp
(join-line-from-below)
```

- - -

### join-line-or-lines-in-region [command]

Join this line or the lines in the selected region.

<sup>function signature</sup>
```lisp
(join-line-or-lines-in-region)
```

- - -

### kill-untitled-buffers [command]

Kill untitled buffers.

<sup>function signature</sup>
```lisp
(kill-untitled-buffers)
```

- - -

### kill-whole-word [command]

Kill the current word at point.

<sup>function signature</sup>
```lisp
(kill-whole-word)
```

- - -

### macos-get-list-of-windowids

Get a list of macOS windowids.

<sup>function signature</sup>
```lisp
(macos-get-list-of-windowids)
```

- - -

### macos-get-window-id-of

Get the windowid of `app`.

<sup>function signature</sup>
```lisp
(macos-get-window-id-of (app))
```

- - -

### macos-get-window-id-of-app [command]

Get the windowid of `app`.

<sup>function signature</sup>
```lisp
(macos-get-window-id-of-app (app))
```

- - -

### magit-just-amend [command]

Just git commit --amend.

<sup>function signature</sup>
```lisp
(magit-just-amend)
```

- - -

### make-kurecolor-24bit-hue-table [command]

Make a 24bit color table using Kurecolor.

<sup>function signature</sup>
```lisp
(make-kurecolor-24bit-hue-table (color))
```

- - -

### make-kurecolor-hue-table [command]

Make a hue table from hex color at top of kill ring, no error checking.

<sup>function signature</sup>
```lisp
(make-kurecolor-hue-table)
```

- - -

### make-yas-from-region [command]

Make a yasnippet from the current region `begin` `end`.

You should use standard snippet formatting in place, e.g. $1,
${1:default value} and so on.  See the yasnippet docs for more info.

You'll be prompted for a name, trigger key and when `prefix-arg` is
specified, a snippet group.

<sup>function signature</sup>
```lisp
(make-yas-from-region (begin end))
```

- - -

### markdown-soma-window-arrangement-start [command]

Arrange windows for `markdown-soma-start`.

Internally uses the script `~/.doom.d/bin/emacs-markdown-preview-layout.osa`.

<sup>function signature</sup>
```lisp
(markdown-soma-window-arrangement-start)
```

- - -

### markdown-soma-window-arrangement-stop [command]

Arrange windows for `markdown-soma-stop`.

Internally uses the script `~/.doom.d/bin/emacs-markdown-preview-close.osa`.

<sup>function signature</sup>
```lisp
(markdown-soma-window-arrangement-stop)
```

- - -

### mc/cua-rectangle-to-multiple-cursors [command]

Switch from cua rectangle to multiple cursors.

<sup>function signature</sup>
```lisp
(mc/cua-rectangle-to-multiple-cursors)
```

- - -

### md-code-to-docstring-arg

Replace markdown inline code with docstring arg style in `string`.

For example:

```lisp
(md-code-to-docstring-arg "`code`")
;;  => CODE
```


<sup>function signature</sup>
```lisp
(md-code-to-docstring-arg (string))
```

- - -

### my-isearch-buffers [command]

Incremental search through open buffers.

<sup>function signature</sup>
```lisp
(my-isearch-buffers)
```

- - -

### my-multi-occur-in-matching-buffers [command]

Show all lines matching `regexp` in all buffers.
Optionally check `allbufs`.

<sup>function signature</sup>
```lisp
(my-multi-occur-in-matching-buffers (regexp &optional allbufs))
```

- - -

### new-untitled-buffer [command]

Open a new buffer called untitled-n.

<sup>function signature</sup>
```lisp
(new-untitled-buffer)
```

- - -

### nuke-all-buffers [command]

Kill all buffers, leaving *scratch* only.

<sup>function signature</sup>
```lisp
(nuke-all-buffers)
```

- - -

### nuke-all-buffers-execept-current [command]

Kill all the open buffers except the current one.
Leave *scratch* and *Messages* alone too.

<sup>function signature</sup>
```lisp
(nuke-all-buffers-execept-current)
```

- - -

### ocodo-clean-key-bindings-for-documentation

Prepare collated binding LIST for documentation.

<sup>function signature</sup>
```lisp
(ocodo-clean-key-bindings-for-documentation (binding-list))
```

- - -

### ocodo-collate-key-bindings-for-documentation

Collate all key bindings found in ocodo-key-bindings-lisp-files.

<sup>function signature</sup>
```lisp
(ocodo-collate-key-bindings-for-documentation)
```

- - -

### ocodo-custom-key-bindings-markdown [command]

Generate markdown `file` with table of custom bindings

<sup>function signature</sup>
```lisp
(ocodo-custom-key-bindings-markdown (file))
```

- - -

### ocodo-filter-key-bindings

Filter `bindings` by `filter` on `index`.

<sup>function signature</sup>
```lisp
(ocodo-filter-key-bindings (filter index bindings))
```

- - -

### ocodo-key-binding-groups-to-markdown

Convert `binding-groups` to string of markdown tables.

<sup>function signature</sup>
```lisp
(ocodo-key-binding-groups-to-markdown (binding-groups headings))
```

- - -

### ocodo-key-bindings-for-documentation

Cleaned list of key bindings for documentation.

<sup>function signature</sup>
```lisp
(ocodo-key-bindings-for-documentation)
```

- - -

### ocodo-key-bindings-use-unicode-symbols

`key-binding` string directions to unicode arrows.
<up> <down> <left> <right> replaced with ↑ ↓ ← →.
<return> replaced with ⮐.

Setting `white-arrows` to t, gives these replacements: ⇧ ⇩ ⇦ ⇨ and ⏎.

<sup>function signature</sup>
```lisp
(ocodo-key-bindings-use-unicode-symbols (key-binding &optional white-arrows))
```

- - -

### ocodo-make-key-binding-groups

Collect `bindings` and `headings` into `groups`.

<sup>function signature</sup>
```lisp
(ocodo-make-key-binding-groups (bindings headings groups))
```

- - -

### ocodo-make-key-binding-table-row

Make a table row from `binding`.

<sup>function signature</sup>
```lisp
(ocodo-make-key-binding-table-row (binding))
```

- - -

### ocodo-sh-indent-rules

Set up shell script indenation rules engines.

<sup>function signature</sup>
```lisp
(ocodo-sh-indent-rules)
```

- - -

### ocodo-ungrouped-key-bindings

Collect `bindings` and HEADINGS into `groups`.

<sup>function signature</sup>
```lisp
(ocodo-ungrouped-key-bindings (bindings title groups))
```

- - -

### open-line-above [command]

Open a newline above the current point, without moving.

<sup>function signature</sup>
```lisp
(open-line-above)
```

- - -

### open-line-below [command]

Open a newline below the current point, without moving.

<sup>function signature</sup>
```lisp
(open-line-below)
```

- - -

### open-this-in-intellij-idea-osx [command]

Open the current file in intellij IDEA (OS X specific).

<sup>function signature</sup>
```lisp
(open-this-in-intellij-idea-osx)
```

- - -

### open-this-in-xcode [command]

Open the current file in XCode.

<sup>function signature</sup>
```lisp
(open-this-in-xcode)
```

- - -

### package-commentary-to-markdown [command]

Read the commentary from current emacslisp file and write it to `markdown-file`.

Conversion is minimal and expects that most of the docstring is already formatted as
markdown.  Quoted `items` will be converted to backquoted `items`.

<sup>function signature</sup>
```lisp
(package-commentary-to-markdown (markdown-file &optional emacslisp-file))
```

- - -

### package-insert-ends-here [command]

Insert the ELPA package file ending string.

(When it's missing)

<sup>function signature</sup>
```lisp
(package-insert-ends-here)
```

- - -

### package-markdown-to-commentary [command]

Read `markdown-file` and insert it into the `emacslisp-file` commentary.

Conversion is minimal and assumes the the markdown is suitable for insertion as
commentary.  Backquoted `code` will be converted to Emacs quoted `items`.

<sup>function signature</sup>
```lisp
(package-markdown-to-commentary (markdown-file &optional emacslisp-file))
```

- - -

### pcre-regexp-from-list-of-words [command]

Insert a pcre regexp to match a list of `words`.

<sup>function signature</sup>
```lisp
(pcre-regexp-from-list-of-words (words))
```

- - -

### random-in-range

Return a random number in range `start` to `end`.

<sup>function signature</sup>
```lisp
(random-in-range (start end))
```

- - -

### reload-current-chrome-tab-osx [command]

Run a simple applescript to reload the current Google Chrome tab.

OSX specific.

<sup>function signature</sup>
```lisp
(reload-current-chrome-tab-osx)
```

- - -

### reload-current-firefox-tab-osx [command]

Run a simple applescript to reload the current Google Chrome tab.

OSX specific.

<sup>function signature</sup>
```lisp
(reload-current-firefox-tab-osx)
```

- - -

### rename-this-buffer-and-file [command]

Renames current buffer and file it is visiting.

<sup>function signature</sup>
```lisp
(rename-this-buffer-and-file)
```

- - -

### replace-pretty-quotes [command]

Replace pretty quotes with standard quotes.

<sup>function signature</sup>
```lisp
(replace-pretty-quotes)
```

- - -

### replace-regexp-and-return

Replace regexp `from` to `to` and return cursor to point.

<sup>function signature</sup>
```lisp
(replace-regexp-and-return (from to))
```

- - -

### replace-region-with

No docstring available: TODO

<sup>function signature</sup>
```lisp
(replace-region-with (fn))
```

- - -

### replace-thing-at-point-with

Get the current thing at point.
Replace with the return value of the function `fn`

<sup>function signature</sup>
```lisp
(replace-thing-at-point-with (fn))
```

- - -

### revert-buffer-instant [command]

Revert buffer without prompting.

<sup>function signature</sup>
```lisp
(revert-buffer-instant)
```

- - -

### s-squeeze

Squeeze the occurences of `char` in `string`.
This works the same as `tr -s `char``.

<sup>function signature</sup>
```lisp
(s-squeeze (char string))
```

- - -

### sass-hex-color-to-var [command]

Find a hex color, and replace it with a newly created variable name.
Place the created variable at the top of the file.  Name it based
on the property being set, and its CSS selector, and set its
css-value to the hex color found.

<sup>function signature</sup>
```lisp
(sass-hex-color-to-var)
```

- - -

### screencapture-mac [command]

Screencapture on macOS, interactive or supply `commandline` and FILE_KEYWORD.

<sup>function signature</sup>
```lisp
(screencapture-mac (&optional commandline file-keyword))
```

- - -

### screencapture-mac--complete-arguments-for-option

Do completeing read for arguments of option.

<sup>function signature</sup>
```lisp
(screencapture-mac--complete-arguments-for-option (plist))
```

- - -

### screencapture-mac--entry-from-summaries

No docstring available: TODO

<sup>function signature</sup>
```lisp
(screencapture-mac--entry-from-summaries (summaries))
```

- - -

### screencapture-mac--filename-generator

Generate a filename for the screenshot at `path` with optional `ext` and FILE_KEYWORD.

<sup>function signature</sup>
```lisp
(screencapture-mac--filename-generator (path &optional ext file-keyword))
```

- - -

### screencapture-mac--get-option

Fetch the option from `summary`

<sup>function signature</sup>
```lisp
(screencapture-mac--get-option (summary))
```

- - -

### screencapture-mac--options

Command line options for screencapture (macOS).

<sup>function signature</sup>
```lisp
(screencapture-mac--options)
```

- - -

### screencapture-mac--options-summary

No docstring available: TODO

<sup>function signature</sup>
```lisp
(screencapture-mac--options-summary (plist))
```

- - -

### screencapture-mac--run

Execute the shell `command` with `filename`.

<sup>function signature</sup>
```lisp
(screencapture-mac--run (command filename))
```

- - -

### screencapture-mac--summary-list

Summarized list of screencapture mac options

<sup>function signature</sup>
```lisp
(screencapture-mac--summary-list)
```

- - -

### screencapture-mac--windowid-helper

Get the windowid from a completing-read list.

<sup>function signature</sup>
```lisp
(screencapture-mac--windowid-helper)
```

- - -

### screencapture-mac-reset-default-commandline [command]

Reset the default commandline

<sup>function signature</sup>
```lisp
(screencapture-mac-reset-default-commandline)
```

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

<sup>function signature</sup>
```lisp
(search-backward-wrapped-string (wrap_start wrap_end))
```

- - -

### search-for-nearest-hex-color [command]

Search to the nearest hex color.
Use negative prefix `p` to go backward.

<sup>function signature</sup>
```lisp
(search-for-nearest-hex-color (p))
```

- - -

### set-default-font-height [command]

Set the default font :height `p` (prefix arg) or enter in minibuffer.

<sup>function signature</sup>
```lisp
(set-default-font-height (p))
```

- - -

### set-internal-border [command]

Set or reset the internal border width `n` of the selected frame.

<sup>function signature</sup>
```lisp
(set-internal-border (n))
```

- - -

### shell-command-on-buffer-file [command]

Run a shell command, using the file of current buffer as input.
Return an error if no buffer file.

<sup>function signature</sup>
```lisp
(shell-command-on-buffer-file)
```

- - -

### shell-command-on-region-replace [command]

Run `shell-command-on-region` replacing the selected region.  `start` `end` `command`.

<sup>function signature</sup>
```lisp
(shell-command-on-region-replace (start end command))
```

- - -

### smart-beginning-of-line [command]

Move point to first non-whitespace character or `beginning-of-line`.

<sup>function signature</sup>
```lisp
(smart-beginning-of-line)
```

- - -

### snippy-comment [command]

Insert a snip line `- - 8< - - -` comment.

<sup>function signature</sup>
```lisp
(snippy-comment)
```

- - -

### sort-sexps [command]

Sort sexps in region.
Comments stay with the code below.

<sup>function signature</sup>
```lisp
(sort-sexps (beg end))
```

- - -

### ssh-agent-env-fix [command]

Ensure $SSH_AUTH_SOCK is set correctly in the environment.

<sup>function signature</sup>
```lisp
(ssh-agent-env-fix)
```

- - -

### switch-to-message-buffer [command]

Switch to the message buffer.

<sup>function signature</sup>
```lisp
(switch-to-message-buffer)
```

- - -

### switch-to-minibuffer-window [command]

Switch to minibuffer window (if active).

<sup>function signature</sup>
```lisp
(switch-to-minibuffer-window)
```

- - -

### switch-to-scratch [command]

Switch to scratch, grab the region if it's active.

<sup>function signature</sup>
```lisp
(switch-to-scratch)
```

- - -

### time-now [command]

current time.

<sup>function signature</sup>
```lisp
(time-now)
```

- - -

### time-to-seconds

Convert `time` `hh:mm:ss` into seconds.

<sup>function signature</sup>
```lisp
(time-to-seconds (time))
```

- - -

### toggle-window-split [command]

Toggle the current window split.

<sup>function signature</sup>
```lisp
(toggle-window-split)
```

- - -

### untabify-buffer [command]

Untabify the current buffer.

<sup>function signature</sup>
```lisp
(untabify-buffer)
```

- - -

### utc-seconds [command]

Insert UTC seconds.

<sup>function signature</sup>
```lisp
(utc-seconds)
```

- - -
