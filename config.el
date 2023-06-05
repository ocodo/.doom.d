;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Load non-doom stuff
(add-to-list 'load-path "~/.doom.d/")
(add-to-list 'load-path "~/.doom.d/plugins/")
(require 'ocodo-handy-functions)
(require 'key-bindings)

(display-time)

(setq initial-major-mode 'lisp-interaction-mode)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Jason M23"
      user-mail-address "jasonm23@gmail.com")

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'doom-load-theme-hook #'set-fancy-splash)

(defun set-fancy-splash ()
  (setq fancy-splash-image "~/.doom.d/golden-yak.png")
  (add-hook! 'after-setting-font-hook (+doom-dashboard-reload 'force)))

(setq doom-theme 'soothe)

(setq doom-unreal-buffer-functions
      '(minibufferp))

;; load private plugins
(dolist
    (plugin (directory-files "~/.doom.d/plugins/" t ".*el$"))
    "Loading local plugins..."
  (load-file plugin))

;; load use-package configs
(dolist
    (config
     (directory-files "~/.doom.d/use/" t ".*el$"))
    "Loading local use configs..."
  (load-file config))

;; Config that is too small to break out a use-package / file...
;; Prettify symbols
(global-prettify-symbols-mode t)

(advice-add
 'mac-handle-font-selection
 :before
 'ocodo/mac-log-handle-font-selection)

;; (advice-remove
;;  'mac-handle-font-selection
;;  'ocodo/mac-log-handle-font-selection)

(defun ocodo/mac-log-handle-font-selection (event)
  (let* ((ae (mac-event-ae event))
         (font-spec (cdr (mac-ae-parameter ae 'font-spec))))
   (when font-spec (message "Font Selected: %S" font-spec))))

(setq doom-modeline-height 0.9)

;; Set doom font.
(when (eq system-type 'darwin)
  (setq doom-font
        (font-spec
         :family "PFDinMono-XThin"
         :weight 'normal)
        doom-variable-pitch-font
        (font-spec
         :family "Helvetica Neue"
         :weight 'ultra-light))
  (doom/reload-font)

 (custom-set-faces
   '(mode-line ((t (:family "Helvetica Neue"
                    :weight ultra-light))))
   '(mode-line-active ((t (:family "Helvetica Neue"
                           :weight ultra-light))))
   '(mode-line-inactive ((t (:family "Helvetica Neue"
                             :weight ultra-light))))))

(setq display-line-numbers-type nil)

(setq org-directory "~/org/")

;; turn paging back on in which-key
(setq which-key-use-C-h-commands t)

(edit-server-start)

(add-hook 'sh-mode-hook #'ocodo-sh-indent-rules)

(set-doom-lambda-line-fonts)

(ssh-agent-env-fix)
