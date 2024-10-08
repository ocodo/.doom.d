;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq user-full-name "Jason M23" user-mail-address "jasonm23@gmail.com")
(defalias 'yes-or-no-p 'y-or-n-p)

(setq doom-unreal-buffer-functions
      '(minibufferp))

(let* ((plugins-dir (format "%s/plugins" doom-user-dir))
       (use-dir (format "%s/use" doom-user-dir)))
  ;; Load non-doom stuff
 (add-to-list 'load-path doom-user-dir)
 (add-to-list 'load-path plugins-dir)

 (require 'ocodo-handy-functions)
 (require 'key-bindings)

 (display-time)
 (setq initial-major-mode 'lisp-interaction-mode)

  ;; Doom annoyances solutions...:
  ;; Whitespace
 (setq-default
  whitespace-style
  '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark missing-newline-at-eof))

  ;; Some functionality uses this to identify you, e.g. GPG configuration, email
  ;; clients, file templates and snippets. It is optional.

  ;; load private plugins
 (dolist
     (plugin (directory-files plugins-dir t ".*el$"))
     "Loading local plugins..."
   (load-file plugin))

  ;; load use-package configs
 (dolist
     (config
      (directory-files use-dir t ".*el$"))
     "Loading local use configs..."
   (load-file config))

  ;; Config that is too small to break out a use-package / file...
  ;; Prettify symbols
 (global-prettify-symbols-mode t))

(setq display-line-numbers-type nil
      org-directory "~/org/"
      kill-whole-line t
      ;; turn paging back on for which-key
      which-key-use-C-h-commands t)

(add-hook 'sh-mode-hook #'ocodo-sh-indent-rules)
(set-doom-lambda-line-fonts)

;; 8< Term --->

(when (not (display-graphic-p))
  (xterm-mouse-mode 1))

;; 8< GUI ---->

(when-gui

 (defun ocodo/mac-log-handle-font-selection (event)
   (let* ((ae (mac-event-ae event))
          (font-spec (cdr (mac-ae-parameter ae 'font-spec))))
    (when font-spec (message "Font Selected: %S" font-spec))))

 (advice-add
  'mac-handle-font-selection
  :before
  'ocodo/mac-log-handle-font-selection)

 (add-hook! 'doom-load-theme-hook (ocodo/set-fancy-splash))
 (add-hook! 'after-setting-font-hook (+doom-dashboard-reload 'force))

 (defun ocodo/set-fancy-splash ()
   (setq doom-modeline-height 0.9
         doom-font (font-spec
                    :family "OcodoMono Nerd Font"
                    :weight 'thin)

         doom-variable-pitch-font (font-spec
                                   :family "Helvetica Neue"
                                   :weight 'light)
         fancy-splash-image (format "%s/tri-arrow-motif.png" doom-user-dir))

  (doom/reload-font)

  (custom-set-faces
    '(mode-line ((t (:family "Helvetica Neue" :weight ultra-light))))
    '(mode-line-active ((t (:family "Helvetica Neue" :weight ultra-light))))
    '(mode-line-inactive ((t (:family "Helvetica Neue" :weight ultra-light)))))

  (run-at-time
   "1 sec"
   nil
   (lambda ()
     (ocodo/reload-fonts)
     (ocodo/markdown-faces-size-reset)))))

;; 8< --- GUI <<<

(when (string-match "-[Mm]icrosoft" operating-system-release)
  ;; WSL: WSL1 has "-Microsoft", WSL2 has "-microsoft-standard"
 (defun wsl-copy-clip (&rest _args)
   "Copy current-kill to WSL clip.exe"
   (setq mytemp (make-temp-file "winclip"))
   (write-region (current-kill 0 t) nil mytemp)
   (shell-command (concat "clip.exe < " mytemp))
   (delete-file mytemp))

 (advice-add 'kill-new :after #'wsl-copy-clip))
;;
;; must load creamsody before loading variants
(ocodo/load-theme "creamsody")
(ocodo/load-theme "creamsody-darker")

(ocodo/reload-keys)

(define-key special-event-map [config-changed-event] 'ignore)
