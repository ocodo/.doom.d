;;; use/use-sideline.el -*- lexical-binding: t; -*-

;;; Code:
(require 'use-package)

(use-package sideline
  :hook (flycheck-mode . sideline-mode)
  :init
  (setq sideline-backends-right '(sideline-flycheck sideline-lsp)))

(use-package sideline-lsp :hook (flycheck-mode . sideline-lsp-setup))
(use-package sideline-flycheck :hook (flycheck-mode . sideline-flycheck-setup))

(provide 'use-sideline)
;;; use-sideline.el ends here
