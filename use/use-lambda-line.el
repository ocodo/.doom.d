;;; use/use-lambda-line.el -*- lexical-binding: t; -*-

(use-package lambda-line
  :custom
    (lambda-line-position 'bottom) ;; Set position of status-line
    (lambda-line-abbrev t) ;; abbreviate major modes
    (lambda-line-hspace "")  ;; add some cushion
    (lambda-line-prefix t) ;; use a prefix symbol
    (lambda-line-prefix-padding t) ;; no extra space for prefix
    (lambda-line-status-invert nil)  ;; no invert colors
    (lambda-line-gui-ro-symbol  "  ⍺ ") ;; symbols
    (lambda-line-gui-mod-symbol "  ⍺ ")
    (lambda-line-gui-rw-symbol  "  ⍺ ")
    (lambda-line-vc-symbol  "   ")
    (lambda-line-icon-time nil)
    (lambda-line-display-group-start "  |  ")
    (lambda-line-display-group-end "  |  ")
    (lambda-line-space-top 0.30)  ;; padding on top and bottom of line
    (lambda-line-space-bottom -0.40)
    (lambda-line-symbol-position -0.150) ;; adjust the vertical placement of symbol
    (lambda-line-abbrev-alist
     `((dired-mode . "DR")
       (emacs-lisp-mode . "λ")
       (fundamental-mode . "🗌")
       (helpful-mode . "")
       (help-mode . "")
       (lisp-interaction-mode . "λi")
       (markdown-mode . "MD")
       (magit-mode . "MG")
       (web-mode . "W3")
       (prog-mode . "PR")
       (python-mode . "PY")
       (text-mode . "")))
  :config

    ;; activate lambda-line
    (lambda-line-mode 1))


;; Gfn doom font shat... bye
(unbind-key "s-=")
(unbind-key "s--")

(bind-key "s-=" #'text-scale-increase)
(bind-key "s--" #'text-scale-decrease)

(set-fontset-font
  "fontset-default"
  (cons (decode-char 'ucs #xF0000)
        (decode-char 'ucs #xF008F))
  "ClockFace")
