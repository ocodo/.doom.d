;;; use/use-ruby -*- lexical-binding: t; -*-

(use-package! ruby-mode
  :config

  (bind-key "C-c l e b" #'ruby-send-buffer 'ruby-mode-map)
  (bind-key "C-c l e d" #'ruby-send-block 'ruby-mode-map)
  (bind-key "C-c l e r" #'ruby-send-region 'ruby-mode-map))

;;; use-ruby.el ends here
