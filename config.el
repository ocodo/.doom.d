;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(defun ocodo/mac-log-handle-font-selection (event)
  (let* ((ae (mac-event-ae event))
         (font-spec (cdr (mac-ae-parameter ae 'font-spec))))
   (when font-spec (message "Font Selected: %S" font-spec))))

(advice-add
 'mac-handle-font-selection
 :before
 'ocodo/mac-log-handle-font-selection)

(setq user-full-name "Jason M23"
      user-mail-address "jasonm23@gmail.com")

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'doom-load-theme-hook #'set-fancy-splash)

(defun set-fancy-splash ()
  (setq fancy-splash-image (format "%s/golden-yak.png" doom-user-dir))
  (add-hook! 'after-setting-font-hook (+doom-dashboard-reload 'force)))

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
  (global-prettify-symbols-mode t)

  ;; (advice-remove
  ;;  'mac-handle-font-selection
  ;;  'ocodo/mac-log-handle-font-selection)

  (setq doom-modeline-height 0.9)

  ;; Set doom font on Macos
  (when (eq system-type 'darwin)
    (setq
     doom-font
     (font-spec
      :family "OcodoMono"
      :weight 'thin)

     doom-variable-pitch-font
     (font-spec
      :family "Helvetica Neue"
      :weight 'light))

    (doom/reload-font)

   (custom-set-faces
     '(mode-line ((t (:family "Helvetica Neue"
                      :weight ultra-light))))
     '(mode-line-active ((t (:family "Helvetica Neue"
                             :weight ultra-light))))
     '(mode-line-inactive ((t (:family "Helvetica Neue"
                               :weight ultra-light))))))


  ;; Set doom font on linux
  (when (eq system-type 'gnu/linux)
    (setq
     doom-font
     (font-spec
      :family "OcodoMono"
      :weight 'thin)

     doom-variable-pitch-font
     (font-spec
      :family "Helvetica"))

    (doom/reload-font)

   (custom-set-faces
     '(mode-line ((t (:family "Helvetica"))))
     '(mode-line-active ((t (:family "Helvetica"))))
     '(mode-line-inactive ((t (:family "Helvetica"))))))

  (setq display-line-numbers-type nil
        org-directory "~/org/"
        kill-whole-line t
        ;; turn paging back on for which-key
        which-key-use-C-h-commands t)

  (ssh-agent-env-fix)
  (edit-server-start)
  (add-hook 'sh-mode-hook #'ocodo-sh-indent-rules)
  (set-doom-lambda-line-fonts)

  ;; must load creamsody before loading variants
  (ocodo/load-theme "creamsody")
  (ocodo/load-theme "creamsody-obsidian")

  (ocodo/reload-keys)

    ;; Because... Doom ain't perfect, unless you have unlimited time to track down it's ... failings.
    ;; KludGY time delays to unkcuf the disylap
  (run-at-time
   "3 sec"
   nil
   (lambda ()
     (ocodo/reload-fonts)
     (ocodo/markdown-faces-size-reset)
     (when (eq system-type 'darwin)
       (ocodo/resize-frame-inset-maximized 20)))))
