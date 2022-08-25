;;; use/use-lambda-line.el -*- lexical-binding: t; -*-
(use-package lambda-line
  :custom
  (lambda-line-position 'bottom) ;; Set position of status-line
  (lambda-line-abbrev t) ;; abbreviate major modes
  (lambda-line-hspace "  ")  ;; add some cushion
  (lambda-line-prefix t) ;; use a prefix symbol
  (lambda-line-prefix-padding nil) ;; no extra space for prefix
  (lambda-line-status-invert nil)  ;; no invert colors
  (lambda-line-gui-ro-symbol  "  ⨂  ") ;; symbols
  (lambda-line-gui-mod-symbol "  ⬤  ")
  (lambda-line-gui-rw-symbol  "  ◯  ")
  (lambda-line-space-top +.25)  ;; padding on top and bottom of line
  (lambda-line-space-bottom -.25)
  (lambda-line-symbol-position 0.1) ;; adjust the vertical placement of symbol
  :config
  ;; activate lambda-line
  (lambda-line-mode 1))
