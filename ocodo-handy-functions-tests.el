;;; ocodo-handy-functions-tests.el -*- lexical-binding: t; -*-

(require 'ocodo-handy-functions)
(require 'ert)

(ert-deftest let1 ()
  "Test let1."
  (should (equal 2 (let1 a 2 a))))

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

(ert-deftest test-docstring-to-markdown ()
  "Test ensure that the combination of arg and back quoted conversion to markdown doesn't do anything unexpected."
  (let* ((input "Change a HEX color's brightness VAL, amount values from 0.0-1.0.
returns a 6 digit hex color.")
         (expected "Change a `hex` color's brightness `val`, amount values from 0.0-1.0.
returns a 6 digit hex color."))
    (should (equal
             (docstring-args-to-markdown-code
              (docstring-back-quoted-to-markdown-code
                  input))
             expected))))

(ert-deftest docstring-args-to-markdown-code ()
  "Transform DOCSTRING arguments to inline markdown `code` document style."
    (should (string=
             (docstring-args-to-markdown-code "Change a HEX color's brightness VAL, amount values from 0.0-1.0.
returns a 6 digit hex color.")
             "Change a `hex` color's brightness `val`, amount values from 0.0-1.0.
returns a 6 digit hex color."))
    (should (string= "This is an $ENV. This is an `arg`."
                     (docstring-args-to-markdown-code
                      "This is an $ENV. This is an ARG.")))
    (should (string= "Convert `h` `s` `v` to a 6 digit hex color."
                     (docstring-args-to-markdown-code
                      "Convert H S V to a 6 digit hex color."))))

(ert-deftest csv-to-lists ()
  "Test csv-to-lists."
  (let ((csv "1,2,3,4,5,Foo
5,4,3,2,1,Bar")
        (expected '(("1" "2" "3" "4" "5" "Foo")
                    ("5" "4" "3" "2" "1" "Bar"))))
    (csv-to-lists csv)
    (should (equal expected (csv-to-lists csv)))))

(ert-deftest decimal-to-hex ()
  "Test decimal to hex conversion (editor fn so all numbers are strings)."
  (let ((decimal-num "410")
        (expected "19A"))
    (should (string= expected (decimal-to-hex decimal-num)))))

(ert-deftest int-to-binary-string ()
  "Test decimal int to binary string."
  (let ((decimal-num 6)
        (expected "110"))
    (should (string= expected (int-to-binary-string decimal-num)))))

(ert-deftest s-squeeze ()
  "Test s-squeeze."
  (let ((expected ".Test.")
        (str ".....Test...."))
    (should (string= expected (s-squeeze "." str)))))

(ert-deftest time-to-seconds ()
  "Test time to seconds."
  (let ((expected 369)
        (time "00:06:09"))
    (should (= expected (time-to-seconds time)))))

(ert-deftest generate-markdown-defun-entry ()
  "Test markdown generation from defun info."
  (let ((expected (format-multiline "|### align-number-right
                                     |
                                     |Align region to equal signs from `begin` to `end`.
                                     |
                                     |<sup>function signature</sup>
                                     |```lisp
                                     |(align-number-right (begin end))
                                     |```
                                     |
                                     |- - -
                                     |"))
        (info '(align-number-right (begin end) "Align region to equal signs from BEGIN to END.")))
   (should (string= expected (generate-markdown-defun-entry info)))))

(ert-deftest is-markdown-filename-p ()
  "Test is-markdown-filename-p"
  (let ((expected `(,t ,t ,nil))
        (filenames '("test.md"
                     "test.markdown"
                     "test.txt")))
    (should (equal expected
                   (--map
                    (is-markdown-filename-p it)
                    filenames)))))

(ert-deftest format-multiline ()
  "Test multiline format."
  (let ((input '("|Line %s
                |  Line %i
                |    Line %x : %x : %#X
                |  Line %.2f
                |    Line %s
                |" "one" 2 3 255 255 4.23 "five"))
        (expected "Line one
  Line 2
    Line 3 : ff : 0XFF
  Line 4.23
    Line five
"))

   (should (equal expected (apply 'format-multiline input)))))

(ert-deftest md-code-to-docstring-arg ()
  "Test md-code-to-docstring-arg."
  (let ((input "this is a `test`")
        (expected "this is a TEST"))

    (should (equal
              expected
              (md-code-to-docstring-arg input)))))

(ert-deftest plist-bind ()
  "Test plist-bind."
  (let1 result (plist-bind (a c)
                     '(:a 0 :b 1 :c 2 :d 3)
                     `(,a ,c))
    (should (equal result '(0 2)))))
