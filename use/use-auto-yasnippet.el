;;; use/use-auto-yasnippet.el -*- lexical-binding: t; -*-

(use-package! auto-yasnippet
  :config
  (bind-key "C-c C-a s"      #'aya-create)
  (bind-key "C-c C-a e"      #'aya-expand)
  (bind-key "C-c C-a w"      #'aya-persist-snippet-from-history)
  (bind-key "C-c C-a p"      #'aya-previous-in-history)
  (bind-key "C-c C-a n"      #'aya-next-in-history)
  (bind-key "C-c C-a SPC"    #'aya-expand-from-history)
  (bind-key "C-c C-a d"      #'aya-delete-from-history))
