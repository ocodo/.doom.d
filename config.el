;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Load non-doom stuff
(add-to-list 'load-path "~/.doom.d/")
(add-to-list 'load-path "~/.doom.d/plugins/")
(require 'ocodo-handy-functions)
(require 'key-bindings)

(setq initial-major-mode 'lisp-interaction-mode)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Jason M23"
      user-mail-address "jasonm23@gmail.com")

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'doom-load-theme-hook #'set-fancy-splash)

(defun set-fancy-splash ()
  (setq fancy-splash-image "~/.doom.d/doom-creamsody.png")
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

;; Font
(setq doom-modeline-height 1)
(when (eq system-type 'darwin)
  (setq doom-font
        (font-spec
         :family "IBM Plex Mono"
         :weight 100)
        doom-variable-pitch-font
        (font-spec
         :family "Avenir Next"))

 (custom-set-faces
   '(mode-line ((t (:family "Avenir Next" :height 0.9))))
   '(mode-line-active ((t (:family "Avenir Next" :height 0.9)))) ; For 29+
   '(mode-line-inactive ((t (:family "Avenir Next" :height 0.9))))))

(setq display-line-numbers-type nil)

(setq org-directory "~/org/")

;; turn paging back on in which-key
(setq which-key-use-C-h-commands t)

(edit-server-start)

(add-hook 'sh-mode-hook #'ocodo-sh-indent-rules)
