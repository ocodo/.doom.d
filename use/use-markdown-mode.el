;;; use/use-markdown-mode.el -*- lexical-binding: t; -*-

(defun ocodo/insert-kbd-tag ()
  "Insert a <kbd></kbd> tag."
  (interactive)
  (insert "<kbd></kbd>") (backward-char 6))
(when (display-graphic-p)
  ;; Markdown settings
  (use-package! markdown-mode
    :config (use-package! markdown-soma
              :config (setq markdown-soma-custom-css (markdown-soma--css-pathname-from-builtin-name "github-dark")
                            markdown-soma-highlight-theme "atelier-plateau.dark")

             (bind-key "M-s-=" #'ocodo/markdown-faces-size-increase 'markdown-mode-map)
             (bind-key "M-s--" #'ocodo/markdown-faces-size-decrease 'markdown-mode-map)
             (bind-key "M-s-0" #'ocodo/markdown-faces-size-reset 'markdown-mode-map)

             (bind-key "C-c i k"   #'ocodo/insert-kbd-tag 'markdown-mode-map)
             (bind-key "C-c S b"   #'markdown-soma-select-builtin-css 'markdown-mode-map)
             (bind-key "C-c S c"   #'markdown-soma-select-css-file 'markdown-mode-map)
             (bind-key "C-c S h"   #'markdown-soma-select-highlight-theme 'markdown-mode-map)
             (bind-key "C-c S s"   #'markdown-soma-mode 'markdown-mode-map)
             (bind-key "C-c S r"   #'markdown-soma-restart 'markdown-mode-map))))
