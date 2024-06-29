;;; use/use-web-mode.el --- Web mode
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;  config for web mode
;;
;;; Code:
(require 'use-package)

(use-package web-mode
  :config
  (unbind-key "M-/" web-mode-map)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-markup-comment-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-sql-indent-offset 4)
  (setq web-mode-attr-indent-offset 4)
  (setq web-mode-attr-value-indent-offset 4))

(provide 'use-web-mode)
;;; use-web-mode.el ends here
