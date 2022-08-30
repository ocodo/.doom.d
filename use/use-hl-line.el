;;; use/use-hl-line.el -*- lexical-binding: t; -*-

(use-package! hl-line
  :config

  (global-hl-line-mode -1)
  (setq global-hl-line-modes nil)
  (bind-key "C-c l h" #'hl-line-mode))
