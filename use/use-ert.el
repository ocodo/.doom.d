;;; use/use-ert.el -*- lexical-binding: t; -*-

;; ert settings
(use-package! ert

  :config
   (defun ert-run-all-tests ()
     "Run all tests available."
     (interactive)
     (ert t))

   (bind-key "C-c / t"   #'ert-run-all-tests    'emacs-lisp-mode-map)
   (bind-key "C-c / c"   #'ert-delete-all-tests 'emacs-lisp-mode-map))
