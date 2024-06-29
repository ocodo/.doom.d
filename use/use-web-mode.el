;;; use/use-web-mode.el --- Web mode
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;  config for web mode
;;
;;; Code:
(require 'use-package)

(use-package web-mode
  :config (unbind-key "M-/" web-mode-map))

(provide 'use-web-mode)
;;; use-web-mode.el ends here
