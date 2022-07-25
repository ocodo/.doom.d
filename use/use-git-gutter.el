(use-package! git-gutter
  :config

  (bind-key "C-x v <left> g" #'+vc/gutter-hydra/body)
  (bind-key "C-x v <left> s" #'+vc/smerge-hydra/body)
  (bind-key "C-x v <left> b" #'+vc/browse-at-remote-homepage))
