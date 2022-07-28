;;; use/use-edebug.el -*- lexical-binding: t; -*-

(use-package! edebug
 :config
 (bind-key "C-x X d n" #'edebug-remove-instrumentation)
 (bind-key "C-x X d y" #'edebug-defun))
