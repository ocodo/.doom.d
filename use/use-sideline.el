;;; use/use-sideline.el -*- lexical-binding: t; -*-

;;; Code:
(require 'use-package)

(when (display-graphic-p)
 (use-package sideline
   :hook (flycheck-mode . sideline-mode)
   :init
   (setq sideline-backends-right '(sideline-flycheck sideline-lsp)
         sideline-backends-skip-current-line t  ; don't display on current line
         sideline-order-left 'down              ; or 'up
         sideline-order-right 'up               ; or 'down
         sideline-format-left "%s   "           ; format for left aligment
         sideline-format-right "   %s"          ; format for right aligment
         sideline-priority 100                  ; overlays' priority
         sideline-display-backend-name t))      ; display the backend name

 (use-package lsp-mode :hook (lsp-mode . sideline-mode))  ; enable it when lsp is on
 (use-package lsp-ui :init (setq lsp-ui-sideline-enable nil))  ; disable original sideline
 (use-package sideline-flycheck :hook (flycheck-mode . sideline-flycheck-setup))

 (provide 'use-sideline))
;;; use-sideline.el ends here
