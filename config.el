;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(global-visual-line-mode -1)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Jason M23"
      user-mail-address "jasonm23@gmail.com")

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'doom-load-theme-hook #'set-fancy-splash)

(defun set-fancy-splash ()
  (setq fancy-splash-image "~/.doom.d/doom-vapourwave.png")
  (add-hook! 'after-setting-font-hook (+doom-dashboard-reload 'force)))

(setq doom-theme 'orangey-bits)

(setq doom-unreal-buffer-functions
      '(minibufferp))

;; Load non-doom stuff
(load-file "~/.doom.d/ocodo-handy-functions.el")
(load-file "~/.doom.d/key-bindings.el")

(ssh-agent-env-fix)

;; load use-package configs
(dolist-with-progress-reporter
    (config
     (directory-files "~/.doom.d/use/" t ".*el"))
    "Loading local use configs..."
  (load-file config)
  (sit-for 0.1))
  ;; fake waiting...
  ;remove when it actually takes time to run this!

;; Config that is too small to break out a use-package / file...
;; Prettify symbols
(global-prettify-symbols-mode t)

;; Disable hl-mode
(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

(global-hl-line-mode -1)

;; Font
(setq doom-modeline-height 1)
(when (eq system-type 'darwin)
  (setq doom-font
        (font-spec
         :family "SauceCodePro Nerd Font"
         :weight 'light)
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
