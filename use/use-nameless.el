(use-package! nameless
  :config
  (add-hook 'emacs-lisp-mode-hook #'nameless-mode)
  (setq! nameless-private-prefix "-"
         nameless-prefix "+"
         nameless-separator "-"))

;; The ⎆ Enter symbol was used in ocodo/,emacs.d
;; UML though.. sets some sort of precedent.
