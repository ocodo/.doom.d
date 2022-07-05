;;; ocodo-handy-functions-tests.el -*- lexical-binding: t; -*-

(require 'ocodo/handy-functions)
(require 'ert)

(ert-deftest docstring-back-quoted-to-markdown-code ()
  "Convert doctring back quoted text to markdown `code`."
  (let ((docstring  "Convert DOCSTRING to markdown `code'")
        (expected  "Convert DOCSTRING to markdown `code`"))
    (should (string= expected (docstring-back-quoted-to-markdown-code docstring)))))
             
(ert-deftest docstring-args-to-markdown-code ()
  "Transform DOCSTRING arguments to inline markdown `code` document style."
  (let ((docstring  "Convert `doc-string_1` to markdown `code'")
        (expected  "Convert `doc-string`_1`` to markdown `code'"))
    (should (string= expected (docstring-args-to-markdown-code docstring)))))

(ert-deftest csv--to-lists ()
  "Test csv--to-lists."
  (let ((csv "1,2,3,4,5,Foo
5,4,3,2,1,Bar")
        (expected '(("1" "2" "3" "4" "5" "Foo")
                    ("5" "4" "3" "2" "1" "Bar"))))
    (csv--to-lists csv)
    (should (equal expected (csv--to-lists csv)))))

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
  "Test markdownm generation from defun info."
  (let ((expected "### align-number-right

Align region to equal signs from `begin` to `end`.

```lisp
(align-number-right (begin end))
```
")
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
