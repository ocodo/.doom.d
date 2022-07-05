;;; use/use-markdown-mode.el -*- lexical-binding: t; -*-

;; Markdown settings
(use-package! markdown-mode
  :config
  (use-package! markdown-soma
    :config (setq markdown-soma-custom-css
                  "~/workspace/soma/styles/lopped-off-dark-subtle.css"
                  markdown-soma-highlight-theme
                  "atelier-plateau.dark")

    (bind-key "C-c S c"   #'markdown-soma-select-css-file 'markdown-mode-map)
    (bind-key "C-c S h"   #'markdown-soma-select-highlight-theme 'markdown-mode-map)
    (bind-key "C-c S s"   #'markdown-soma-mode 'markdown-mode-map)
    (bind-key "C-c S r"   #'markdown-soma-restart 'markdown-mode-map)))
