;;; dev/elisp-to-markdown-tests.el -*- lexical-binding: t; -*-

(require 'elisp-to-markdown)

(ert-deftest docstring-back-quoted-to-markdown-code ()
  "Convert doctring back quoted text to markdown `code`."
  (let ((docstring  "Convert DOCSTRING to markdown `code'")
        (expected  "Convert DOCSTRING to markdown `code`"))
    (should (string=
             expected
             (docstring-back-quoted-to-markdown-code docstring)))
    (should (string=
             "Change a HEX color's brightness VAL, amount values from 0.0-1.0.
returns a 6 digit hex color."
             (docstring-back-quoted-to-markdown-code "Change a HEX color's brightness VAL, amount values from 0.0-1.0.
returns a 6 digit hex color.")))))

(ert-deftest docstring-to-markdown ()
  "Test ensure that the combination of arg and back quoted conversion to markdown doesn't do anything unexpected."
  (let* ((input "Change a HEX color's brightness VAL, amount values from 0.0-1.0.
returns a 6 digit hex color.")
         (expected "Change a `hex` color's brightness `val`, amount values from 0.0-1.0.
returns a 6 digit hex color."))
    (should (equal
             (docstring-args-to-markdown-code "( hex val)"
              (docstring-back-quoted-to-markdown-code
                  input))
             expected))))

(ert-deftest docstring-table-to-markdown ()
  "It converts a docstring plist table definition to a markdown table."

  (should ;; pass through
   (string= (docstring-options-table-to-markdown "Change a `hex` color's brightness `val`, amount values from 0.0..1.0.
returns a 6 digit hex color.")
            "Change a `hex` color's brightness `val`, amount values from 0.0..1.0.
returns a 6 digit hex color."))

  (should ;; do the thing...
   (string= (docstring-options-table-to-markdown
             "#TABLE Option - Description #
:test - this is what testing is about
:opt  - an option
#TABLE#") "| Option | Description |
|-|-|
| `:test' | this is what testing is about |
| `:opt' | an option |"))

  (should ;; in place...
   (string=
    (docstring-options-table-to-markdown
     "Create an SVG palette image for a theme.

Optional parameter `options` (a plist). Any required values not
supplied in OPTIONS will use defaults or prompt interactively.

#TABLE Option - Description #
    :theme-file - theme filename
    :theme-name - override the title found in :theme-file
    :theme-description - override the description found in :theme-file
    :theme-url - override the url found in :theme-file
    :font-family - font name to use in the generated SVG
    :columns - number of columns for each palette row (default: 6)
    :bg-color - Page background color
    :text-color - Main text color
    :text-accent-color - Text accent color
    :page-template - see page-template below
    :page-top-margin - (default: 120)
    :page-right-margin - (default: 30)
    :page-bottom-margin - (default: 60)
    :page-left-margin - (default: 30)
    :swatch-template - see swatch-template below
    :swatch-border-color - the border color of a color swatch
    :swatch-width - px spacing width of a color swatch (default: 100)
    :swatch-height - px spacing height of a color swatch (default: 150)
    :swatch-rotate - degrees of rotation for swatch (default: 45)
    :h-space - horizontal-space between swatches (default: 10)
    :v-space - vertical-space between swatches (default: 10)
    :sort-palette - arrange palette using a function name
    :svg-out-file - the file/pathname to save SVG output
#TABLE#

For advanced customization the :page-template and :swatch-template can be
used to provide customize the SVG templates.

Note: Template parameters are filled by `format' so we mark them as follows:

Page Template parameters:

    %1$s  - width
    %2$s  - height
    %3$s  - font-family
    %4$s  - text-color
    %5$s  - text-accent-color
    %6$s  - bg-color
    %7$s  - theme-name
    %8$s  - theme-description
    %9$s  - theme-url
    %10$s - color swatches

Swatch Template parameters:

    %1$s - x
    %2$s - y
    %3$s - swatch-border-color
    %4$s - swatch-color
    %5$s - text-accent-color
    %6$s - swatch-color-name")

    "Create an SVG palette image for a theme.

Optional parameter `options` (a plist). Any required values not
supplied in OPTIONS will use defaults or prompt interactively.

| Option | Description |
|-|-|
    | `:theme-file' | theme filename |
    | `:theme-name' | override the title found in :theme-file |
    | `:theme-description' | override the description found in :theme-file |
    | `:theme-url' | override the url found in :theme-file |
    | `:font-family' | font name to use in the generated SVG |
    | `:columns' | number of columns for each palette row (default: 6) |
    | `:bg-color' | Page background color |
    | `:text-color' | Main text color |
    | `:text-accent-color' | Text accent color |
    | `:page-template' | see page-template below |
    | `:page-top-margin' | (default: 120) |
    | `:page-right-margin' | (default: 30) |
    | `:page-bottom-margin' | (default: 60) |
    | `:page-left-margin' | (default: 30) |
    | `:swatch-template' | see swatch-template below |
    | `:swatch-border-color' | the border color of a color swatch |
    | `:swatch-width' | px spacing width of a color swatch (default: 100) |
    | `:swatch-height' | px spacing height of a color swatch (default: 150) |
    | `:swatch-rotate' | degrees of rotation for swatch (default: 45) |
    | `:h-space' | horizontal-space between swatches (default: 10) |
    | `:v-space' | vertical-space between swatches (default: 10) |
    | `:sort-palette' | arrange palette using a function name |
    | `:svg-out-file' | the file/pathname to save SVG output |

For advanced customization the :page-template and :swatch-template can be
used to provide customize the SVG templates.

Note: Template parameters are filled by `format' so we mark them as follows:

Page Template parameters:

    %1$s  - width
    %2$s  - height
    %3$s  - font-family
    %4$s  - text-color
    %5$s  - text-accent-color
    %6$s  - bg-color
    %7$s  - theme-name
    %8$s  - theme-description
    %9$s  - theme-url
    %10$s - color swatches

Swatch Template parameters:

    %1$s - x
    %2$s - y
    %3$s - swatch-border-color
    %4$s - swatch-color
    %5$s - text-accent-color
    %6$s - swatch-color-name")))



(ert-deftest docstring-args-to-markdown-code ()
  "Transform DOCSTRING arguments to inline markdown `code` document style."
    (should (string=
             (docstring-args-to-markdown-code " (hex val)" "Change a HEX color's brightness VAL, amount values from 0.0-1.0.
returns a 6 digit hex color.")
             "Change a `hex` color's brightness `val`, amount values from 0.0-1.0.
returns a 6 digit hex color."))
    (should (string= "Can handle args with dashes `like-this`."
                     (docstring-args-to-markdown-code " (like-this)"
                      "Can handle args with dashes LIKE-THIS.")))
    (should (string= "This is an $ENV. This is an `arg`."
                     (docstring-args-to-markdown-code " (arg)"
                      "This is an $ENV. This is an ARG.")))
    (should (string= "Convert `h` `s` `v` to a 6 digit hex color."
                     (docstring-args-to-markdown-code " (h s v)"
                      "Convert H S V to a 6 digit hex color.")))
    (should (string= "Convert `h` `s` `v` to a 6 digit Hex color."
                     (docstring-args-to-markdown-code " (h &optional s &rest v)"
                      "Convert H S V to a 6 digit Hex color."))))
