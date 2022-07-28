;;; use/use-hl-line.el -*- lexical-binding: t; -*-

(use-package! hl-line
  :config

  (global-hl-line-mode -1)
  (bind-key "C-c l h" #'hl-line-mode))
