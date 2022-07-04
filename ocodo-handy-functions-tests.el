;;; ocodo-handy-functions-tests.el -*- lexical-binding: t; -*-

(require 'ocodo/handy-functions)
(require 'ert)

(ert-deftest docstring-back-quoted-to-markdown-code-test ()
  "Convert doctring back quoted text to markdown `code`."
  (let ((docstring  "Convert DOCSTRING to markdown `code'")
        (expected  "Convert DOCSTRING to markdown `code`"))
    (should (string= expected
             (s-replace-regexp (rx (syntax ) (group (?* anychar)) syntax character-quote)  "`\1`" docstring)))))

             ;; (docstring-back-quoted-to-markdown-code docstring)
