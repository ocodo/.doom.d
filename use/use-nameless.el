(use-package! nameless
  :config
  (add-hook 'emacs-lisp-mode-hook #'nameless-mode)
  (setq! nameless-private-prefix "◇"
         nameless-prefix "◆"
         nameless-separator "-"))
