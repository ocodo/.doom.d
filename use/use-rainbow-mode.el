;;; use/use-rainbow-mode.el -*- lexical-binding: t; -*-

(use-package! rainbow-mode
     :config  (add-hook! 'rainbow-mode-hook
               (hl-line-mode (if rainbow-mode -1 +1))))
