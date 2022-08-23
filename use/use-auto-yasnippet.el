;;; use/use-auto-yasnippet.el -*- lexical-binding: t; -*-

(use-package! auto-yasnippet
  :config
  (bind-key "C-c C-a s"      #'aya-create)
  (bind-key "C-c C-a e"      #'aya-expand)
  (bind-key "C-c C-a w"      #'ayah-persist-snippet-from-history)
  (bind-key "C-c C-a p"      #'ayah-previous-in-history)
  (bind-key "C-c C-a n"      #'ayah-next-in-history)
  (bind-key "C-c C-a SPC"    #'ayah-expand-from-history)
  (bind-key "C-c C-a d"      #'ayah-delete-from-history))
