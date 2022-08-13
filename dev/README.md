# Elisp to markdown

 - - -
## Functions

### buffer-defuns-to-markdown

Parse all defuns in `buffer` and save to markdown `file`.

<sup>function signature</sup>
```lisp
(buffer-defuns-to-markdown (buffer file))
```

- - -

### current-buffer-defuns-to-markdown [command]

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

### docstring-to-text-and-code

Split `docstring` into text and code sections.

```
(setq docstring (format-multiline "|Split `docstring` into text and code blocks
                                   |
                                   |Example:
                                   |
                                   |```
                                   |(docstring-to-text-and-code docstring)
                                   |```
                                   |
                                   |Also indented code blocks...
                                   |
                                   |    (docstring-to-text-and-code docstring)
                                   |")

# docstring split into text and code
'((:text "Split `docstring` into text and code blocks

Example:

")
  (:code "(docstring-to-text-and-code docstring)"))
  (:text "

Also indented code blocks...

"))
  (:code "(docstring-to-text-and-code docstring)"))
```

<sup>function signature</sup>
```lisp
(docstring-to-text-and-code (docstring))
```

- - -

### elisp-file-defuns-to-markdown [command]

Parse all defuns in `elisp-file` and save to `markdown-file`.

<sup>function signature</sup>
```lisp
(elisp-file-defuns-to-markdown (elisp-file markdown-file))
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
