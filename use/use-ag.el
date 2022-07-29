;;; use/use-ag.el -*- lexical-binding: t; -*-

;;; Code:
(require 'use-package)

(defhydra silver-searcher-hydra (:hint nil) "
_g_ : Ag Search for string       _r_ : Ag Search for regex
_d_ : Ag Dired search for string _f_ : Ag Dired search for regex
_k_ : Ag Close all Ag buffers"
   ("g" ag)
   ("r" ag-regexp)
   ("d" ag-dired)
   ("f" ag-dired-regexp)
   ("k" ag-kill-buffers))

(use-package ag
  :config
  (progn
    (add-to-list 'ag-arguments "--hidden")
    (setq ag-group-matches nil)
    (bind-key "C-x f" #'silver-searcher-hydra/body)))

(provide 'use-ag)
;;; use-ag ends here
