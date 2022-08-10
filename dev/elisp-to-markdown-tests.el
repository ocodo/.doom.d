;;; dev/elisp-to-markdown-tests.el -*- lexical-binding: t; -*-

(load-file
 (format "%s%s"
  doom-private-dir
  "dev/elisp-to-markdown.el"))

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

(ert-deftest docstring-args-to-markdown-code ()
  "Transform DOCSTRING arguments to inline markdown `code` document style."
    (should (string=
             (docstring-args-to-markdown-code " (hex val)" "Change a HEX color's brightness VAL, amount values from 0.0-1.0.
returns a 6 digit hex color.")
             "Change a `hex` color's brightness `val`, amount values from 0.0-1.0.
returns a 6 digit hex color."))
    (should (string= "This is an $ENV. This is an `arg`."
                     (docstring-args-to-markdown-code " (arg)"
                      "This is an $ENV. This is an ARG.")))
    (should (string= "Convert `h` `s` `v` to a 6 digit hex color."
                     (docstring-args-to-markdown-code " (h s v)"
                      "Convert H S V to a 6 digit hex color.")))
    (should (string= "Convert `h` `s` `v` to a 6 digit hex color."
                     (docstring-args-to-markdown-code " (h &optional s &rest v)"
                      "Convert H S V to a 6 digit hex color."))))

(ert-deftest test-order-substring-matches ()
  "Test order-substring-matches."
  (should (equal (order-substring-matches
                  '(("h" ((35 . 36) (27 . 28) (8 . 9)))
                    ("s" ((10 . 11)))
                    ("v" ((12 . 13)))))
           '(("h" (35 . 36))
             ("h" (27 . 28))
             ("v" (12 . 13))
             ("s" (10 . 11))
             ("h" (8 . 9))))))
