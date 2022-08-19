(use-package vertico-posframe
  :config (setq vertico-posframe-width 70 
                vertico-posframe-border-width 15
                vertico-posframe-font "Avenir Next"
                vertico-posframe-show-minibuffer-rules nil
                vertico-posframe-parameters '((left-fringe . 10) (right-fringe . 10)))
          (vertico-posframe-mode 1))
