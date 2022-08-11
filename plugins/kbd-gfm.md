# Kbd gfm

 Reformat kbd strings for use with Github formatted
 Markdown.  Basically it wraps the parts of a kbd string in <kbd>
 tags.

 - - -
## Functions

### kbd-gfm-expanded-emacs-modifier

Convert an Emacs `modifier` to it's long form.
Expansion is defined in `kbd-gfm-modifier-list`.

<sup>function signature</sup>
```lisp
(kbd-gfm-expanded-emacs-modifier (modifier))
```

- - -

### kbd-gfm-get-region-or-sexp-and-prefix

Get the region or current sexp and prefix for the `interactive` macro.

Note: `region-beginning` and `region-end` are the reason why an
`interactive` macro with "r" will blow up with the error:

"The mark is not set now, so there is no region"

Predicate `region-active-p` blocks calls to these functions when
there's no region.

When `override-prefix` is non-nil, return it instead of the true prefix.

<sup>function signature</sup>
```lisp
(kbd-gfm-get-region-or-sexp-and-prefix (&optional override-prefix))
```

- - -

### kbd-gfm-key-chord-to-gfm-kbd

Convert a `key-chord` to gfm kbd element style.

When `long-modifiers` is non-nil convert modifiers to long form.

Uses `kbd-gfm-modifier-list` to define long form.

<sup>function signature</sup>
```lisp
(kbd-gfm-key-chord-to-gfm-kbd (key-chord &optional long-modifiers))
```

- - -

### kbd-gfm-key-to-gfm-kbd

Take a `key` and wrap it in a <kbd></kbd> tag.

<sup>function signature</sup>
```lisp
(kbd-gfm-key-to-gfm-kbd (key))
```

- - -

### kbd-gfm-replace-with-gfm-kdb [command]

Replace the region or sexp (`begin` `end`) with the gfm kbd version.

if `long` is non-nil use extended modifier names.

<sup>function signature</sup>
```lisp
(kbd-gfm-replace-with-gfm-kdb (begin end long))
```

- - -

### kbd-gfm-replace-with-long-gfm-kbd [command]

Replace the region or sexp with the long gfm kbd version.

<sup>function signature</sup>
```lisp
(kbd-gfm-replace-with-long-gfm-kbd)
```

- - -

### kbd-gfm-string-to-gfm

Convert a kbd binding `string` to github flavored markdown `<kbd></kbd>` style.
if `long-modifiers` is non-nil expand modifier using `kbd-gfm-expanded-emacs-modifier`

<sup>function signature</sup>
```lisp
(kbd-gfm-string-to-gfm (string &optional long-modifiers))
```

- - -
