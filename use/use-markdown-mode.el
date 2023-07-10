;;; use/use-markdown-mode.el -*- lexical-binding: t; -*-

;; Markdown settings
(use-package! markdown-mode
  :config (use-package! markdown-soma
            :config (setq markdown-soma-custom-css (markdown-soma--css-pathname-from-builtin-name "github-dark")
                          markdown-soma-highlight-theme "atelier-plateau.dark")

            (bind-key "C-c S b"   #'markdown-soma-select-builtin-css 'markdown-mode-map)
            (bind-key "C-c S c"   #'markdown-soma-select-css-file 'markdown-mode-map)
            (bind-key "C-c S h"   #'markdown-soma-select-highlight-theme 'markdown-mode-map)
            (bind-key "C-c S s"   #'markdown-soma-mode 'markdown-mode-map)
            (bind-key "C-c S r"   #'markdown-soma-restart 'markdown-mode-map)))
