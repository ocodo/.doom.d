# Elisp to markdown

 - - -
## Functions

### buffer-defuns-to-markdown

Create markdown for the defuns in `buffer` and save to `file`.

<sup>function signature</sup>
```lisp
(buffer-defuns-to-markdown (buffer file))
```

- - -

### current-buffer-defuns-to-markdown

Create a markdown `file` of all defuns in the current buffer.

<sup>function signature</sup>
```lisp
(current-buffer-defuns-to-markdown (file))
```

- - -

### docstring-args-to-markdown-code

Using `args` transform `docstring` arguments to inline markdown `code` style.

<sup>function signature</sup>
```lisp
(docstring-args-to-markdown-code (args docstring))
```

- - -

### docstring-back-quoted-to-markdown-code

transform back-quoted docstring elements to inline markdown `code` style.

<sup>function signature</sup>
```lisp
(docstring-back-quoted-to-markdown-code (docstring))
```

- - -

### generate-markdown-defun-entry

Generate a markdown entry for `fn`.

<sup>function signature</sup>
```lisp
(generate-markdown-defun-entry (fn))
```

- - -

### generate-markdown-list-of-buffer-defuns

Generate markdown text of all defuns in buffer

<sup>function signature</sup>
```lisp
(generate-markdown-list-of-buffer-defuns (buffer))
```

- - -

### generate-markdown-page-of-buffer-defuns

Generate markdown page for all defun in `buffer`.

`buffer` file name and commentary are used as the page heading.

<sup>function signature</sup>
```lisp
(generate-markdown-page-of-buffer-defuns (&optional buffer))
```

- - -

### get-defun-info

Get information about all `defun` top-level sexps in a `buffer`.
Returns a list with elements of the form (symbol args docstring).

<sup>function signature</sup>
```lisp
(get-defun-info (buffer))
```

- - -

### order-substring-matches

Order a set of `raw`-`substring`-`matches`.

Ordered substrings can then be used to perform replacements
on the original source string.

The list is sorted last to first, so that string replacements
don't invalidate replacements using subsequent substring indexes.

Raw substring matches are in the form:

    '(("s1" ((10 . 12) (30 . 32) ...))...)
      ("s2" ((15 . 17) (20 . 22) ...))...)

This would become:

    '(("s1" (30 . 33)))
      ("s2" (20 . 22)))
      ("s2" (15 . 17)))
      ("s1" (10 . 13)))


<sup>function signature</sup>
```lisp
(order-substring-matches (raw-substring-matches))
```

- - -

### segments-ok-p

Check the `left`-`string` and `right`-`string`.

<sup>function signature</sup>
```lisp
(segments-ok-p (left-string right-string))
```

- - -
