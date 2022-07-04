;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Jason M23"
      user-mail-address "jasonm23@gmail.com")

(add-hook 'doom-load-theme-hook #'set-fancy-splash)

(defun set-fancy-splash ()
  (setq fancy-splash-image "~/.doom.d/doom-vapourwave.png")
  (add-hook! 'after-setting-font-hook (+doom-dashboard-reload 'force)))

(setq doom-unreal-buffer-functions
      '(minibufferp))

;; Prettify symbols
(global-prettify-symbols-mode t)

;; ert settings
(use-package! ert

  :config
  (defun ert-run-all-tests ()
    "Run all tests available."
    (interactive)
    (ert t))

  :bind (:map emacs-lisp-mode-map
         ("C-c / t" . #'ert-run-all-tests)
         ("C-c / c" . #'ert-delete-all-tests)))

;; Markdown settings
(use-package! markdown-mode
  :config
  (use-package! markdown-soma
    :config (setq markdown-soma-custom-css
                  "~/workspace/soma/styles/lopped-off-dark-subtle.css"
                  markdown-soma-highlight-theme
                  "atelier-plateau.dark")

    :bind  (:map markdown-mode-map
            ("C-c S c" . markdown-soma-select-css-file)
            ("C-c S h" . markdown-soma-select-highlight-theme)
            ("C-c S s" . markdown-soma-mode)
            ("C-c S r" . markdown-soma-restart))))

;; Disable hl-mode
(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

(global-hl-line-mode -1)

;; Ensure SSH_AUTH_SOCK is set correctly
;; check that only one ssh-agent is running first.
(defun ssh-agent-env-fix ()
  "Ensure ssh_auth_sock is set correctly in the environment."
  (interactive)
  (if (= (string-to-number (shell-command-to-string "pgrep ssh-agent | wc -l")) 1)
      (let ((private-sock (shell-command-to-string "lsof | grep ssh-agent | grep /private"))
            (agent-sock (shell-command-to-string "lsof | grep ssh-agent | grep /agent")))
        (unless (string= private-sock "")
          (setenv "SSH_AUTH_SOCK" (shell-command-to-string "lsof | grep ssh-agent | grep /private | awk '{printf($8)}'")))

        (unless (string= agent-sock "")
          (setenv "SSH_AUTH_SOCK" (shell-command-to-string "lsof | grep ssh-agent | grep /agent | awk '{printf($8)}'"))))

    (message "There are more than 1 ssh-agents running...:\n %s" (shell-command-to-string "pgrep -l ssh-agent"))))

(ssh-agent-env-fix)

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

(setq doom-font
      (font-spec
       :family "SauceCodePro Nerd Font"
       :weight 'light)
      doom-variable-pitch-font
      (font-spec
       :family "Avenir Next"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'creamsody)

;; Use flex completion style (fuzzy match)
(add-to-list 'completion-styles 'flex)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for bining new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

(use-package swift-mode
  :hook (swift-mode . (lambda () (lsp))))

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable
        "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(use-package nameless
  :config (add-hook 'emacs-lisp-mode-hook #'nameless-mode))

(load-file "~/.doom.d/ocodo-handy-functions.el")

(bind-key "C-("           #'decrement-number-at-point)
(bind-key "C-)"           #'increment-number-at-point)
(bind-key "C-M-^"         #'join-line-from-below)
(bind-key "C-c ;"         #'iedit-mode)
(bind-key "C-c K"         #'+rgb/kurecolor-hydra/body)
(bind-key "C-c ]"         #'embrace-commander)
(bind-key "C-c f w"       #'write-region)
(bind-key "C-c k H"       #'+rgb/kurecolor-hydra/body)
(bind-key "C-c k X"       #'kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgba)
(bind-key "C-c k c"       #'kurecolor-cssrgb-at-point-or-region-to-hex)
(bind-key "C-c k h"       #'kurecolor-hexcolor-at-point-or-region-to-css-rgba)
(bind-key "C-c k s"       #'search-for-nearest-hex-color)
(bind-key "C-c k x"       #'kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgb)
(bind-key "C-c l e i"     #'eval-print-last-sexp)
(bind-key "C-c l h"       #'hl-line-mode)
(bind-key "C-c s-e"       #'eval-and-replace)
(bind-key "C-c t t -"     #'dasherise-at-point-or-region)
(bind-key "C-c t t ."     #'hex-to-decimal-at-point-or-region)
(bind-key "C-c t t /"     #'decimal-to-hex-at-point-or-region)
(bind-key "C-c t t U"     #'url-encode-string-at-point)
(bind-key "C-c t t _"     #'snake-case-at-point-or-region)
(bind-key "C-c t t h"     #'humanize-at-point-or-region)
(bind-key "C-c t t l"     #'lower-camelcase-at-point-or-region)
(bind-key "C-c t t t"     #'titleized-at-point-or-region)
(bind-key "C-c t t u"     #'upper-camelcase-at-point-or-region)
(bind-key "C-c t t v"     #'video-time-to-seconds-at-point-or-region)
(bind-key "C-x /"         #'align-regexp)
(bind-key "C-x f"         #'ag)
(bind-key "C-c r /"       #'sp-split-sexp)
(bind-key "C-c r ."       #'sp-join-sexp)
(bind-key "C-c r <down>"  #'sp-backward-barf-sexp)
(bind-key "C-c r <left>"  #'sp-slurp-hybrid-sexp)
(bind-key "C-c r <right>" #'sp-forward-barf-sexp)
(bind-key "C-x x ."       #'er/expand-region)
(bind-key "M-O"           #'+macos/open-in-default-program)
(bind-key "M-`"           #'magit)
(bind-key "M-o"           #'dired-osx-open-this-file dired-mode-map)
(bind-key "M-s-g g"       #'google-this)
(bind-key "M-s-g n"       #'google-this-noconfirm)
(bind-key "M-z"           #'zap-up-to-char)
(bind-key "s-'"           #'other-window)
(bind-key "s-/"           #'hippie-expand)
(bind-key "s-0"           #'delete-window)
(bind-key "s-1"           #'delete-other-windows)
(bind-key "s-2"           #'split-window-below)
(bind-key "s-3"           #'split-window-right)
(bind-key "s-<down>"      #'duplicate-current-line-or-region)
(bind-key "s-<left>"      #'previous-buffer)
(bind-key "s-<right>"     #'next-buffer)
(bind-key "s-<up>"        #'duplicate-current-line-or-region-up)
(bind-key "s-T"           #'treemacs)
(bind-key "s-U"           #'(lambda () "Revert buffer without prompting" (interactive) (revert-buffer t t t)))
(bind-key "s-^"           #'join-line-from-below)
(bind-key "s-`"           #'other-frame)
(bind-key "s-b"           #'ivy-switch-buffer)
(bind-key "s-B"           #'ibuffer)
(bind-key "s-k"           #'kill-this-buffer)
(bind-key "s-o"           #'find-file)
(bind-key "s-q"           #'kill-emacs)
(bind-key "s-s"           #'save-buffer)
(bind-key "s-t"           #'projectile-find-file)
(bind-key "s-w"           #'delete-frame)
(bind-key "s-|"           #'shell-command-on-region-replace)
(bind-key "C-M-%"         #'anzu-query-replace-regexp)
(bind-key "C-%"           #'anzu-query-replace)

;; Unbind C-RET on global key map
(map!
   "C-RET"      nil
   [C-return]   nil
   "C-S-RET"    nil
   [C-S-return] nil)

;; bind C-enter to CUA Rectangle mode
(bind-key "C-<return>" #'cua-rectangle-mark-mode)
;; turn paging back on in which-key
(setq which-key-use-C-h-commands t)
;; Better help for CUA Rectangle mode via which-key
(bind-key "C-?"
          #'(lambda ()
             (interactive)
             (which-key-show-keymap 'cua--rectangle-keymap))
          cua--rectangle-keymap)

(defalias 'yes-or-no-p 'y-or-n-p)

(edit-server-start)

(defun ocodo-sh-indent-rules ()
  "Try to set sh-mode indent rules."
  (setq smie-config
        '((sh-mode
           (2 :after "then" 2)
           (0 :before "then" 0)
           (2 :after "then" nil)
           (2 :after "{" 2)
           (2 :after "do" 2)
           (2 :after "else" 2))))

  (setq sh-styles-alist
        '(("ocodo"
           (sh-basic-offset . 4)
           (sh-first-lines-indent . 0)
           (sh-indent-after-case . +)
           (sh-indent-after-do . +)
           (sh-indent-after-done . 0)
           (sh-indent-after-else . +)
           (sh-indent-after-if . +)
           (sh-indent-after-loop-construct . +)
           (sh-indent-after-open . +)
           (sh-indent-comment . t)
           (sh-indent-for-case-alt . ++)
           (sh-indent-for-case-label . +)
           (sh-indent-for-continuation . +)
           (sh-indent-for-do . 0)
           (sh-indent-for-done . 0)
           (sh-indent-for-else . 0)
           (sh-indent-for-fi . 0)
           (sh-indent-for-then . 0))))
  (sh-load-style "ocodo"))

(add-hook 'sh-mode-hook #'ocodo-sh-indent-rules)
