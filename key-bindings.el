;;; key-bindings --- Custom key bindings
;;; Commentary:
;;  Customized key bindings
;;; Code:

(require 'bind-key)
(require 'ocodo-handy-functions)
(require 'web-mode)

(unbind-key "s-=")
(unbind-key "s--")
(unbind-key "C-c t b")

(map!
 ;; Unbind C-RET on global key map
 "C-RET"      nil
 [C-return]   nil
 "C-S-RET"    nil
 [C-S-return] nil
 ;; unbind tabbing
 "C-<tab>"     nil
 ;; unbind load theme
 "C-H t" nil)

(bind-key "C-c t 9" #'ocodo/default-face-size-large-screen)

(bind-key "M-RET" #'toggle-frame-fullscreen)

(bind-key "C-<tab>" #'ocodo/match-indent-above)

(bind-key "s-=" #'text-scale-increase)
(bind-key "s--" #'text-scale-decrease)

(bind-key "s-)" #'text-scale-adjust)

(bind-key "s-_" #'subword-mode)

(bind-key "s-<help>" #'overwrite-mode)

(bind-key "s-p" #'find-file-at-point)
(bind-key "C-c f a p" #'find-file-at-point)

(bind-key "M-s-p" #'ocodo/projectile-find-file-dwim)
(bind-key "C-c p f" #'ocodo/projectile-find-file-dwim)

(bind-key "M-s-=" #'ocodo/default-face-size-increase)
(bind-key "M-s--" #'ocodo/default-face-size-decrease)
(bind-key "M-s-0" #'ocodo/default-face-size-reset)

(bind-key "C-c t =" #'ocodo/default-face-size-increase)
(bind-key "C-c t -" #'ocodo/default-face-size-decrease)
(bind-key "C-c t 0" #'ocodo/default-face-size-reset)

(bind-key "s-<mouse-1>" #'browse-url-at-mouse)
(bind-key "C-S-<mouse-1>" #'browse-url-at-mouse)

(bind-key "M-s-o" #'ocodo-open-project)

(bind-key "s-r" #'+make/run)
(bind-key "s-R" #'+make/run-last)

(bind-key "s-r" #'+make/run)
(bind-key "s-R" #'+make/run-last)

(bind-key "s-l" #'consult-line)

(bind-key "s-m" #'jump-to-message-buffer)

(bind-key "s-f"
          (defhydra file-commands
            (:color blue :hint nil)
            "
This file:
[_r_] Rename/Move
[_d_] Delete
"
            ("r" rename-this-buffer-and-file)
            ("d" delete-this-buffer-and-file)))

(bind-key "s-<f8>" #'ocodo/choose-favorite-theme)

(bind-key "s-a" (defhydra region-and-flycheck (:color blue :hint nil)
                  "
- Select/Region ------------------------------------------------------
  [_a_] Select All [_<backspace>_] Delete All [_s_] Select sexp
  [_w_] Copy All   [_<tab>_] Indent All       [_W_] Write to file
  [_y_] Yank All   [_d_] Select defun"
                  ("a" mark-whole-buffer)
                  ("e" consult-flycheck)
                  ("<tab>" indent-buffer)
                  ("W" ocodo/write-region)
                  ("<backspace>" ocodo/kill-buffer-text)
                  ("w" ocodo/kill-ring-save-buffer)
                  ("n" flycheck-next-error)
                  ("y" ocodo/yank-replace-buffer)
                  ("s" mark-sexp)
                  ("d" mark-defun)))

(bind-key "C-H t" #'load-theme)

(bind-key "s-q"            #'kill-emacs)

(bind-key "C-("            #'decrement-number-at-point)
(bind-key "C-)"            #'increment-number-at-point)

(bind-key "C-<return>"     #'cua-rectangle-mark-mode)
(bind-key "C-c SPC r"      #'cua-rectangle-mark-mode)
(bind-key "C-?"            #'cua-rectangle-which-key-help)

(bind-key "C-M-^"          #'join-line-from-below)

(bind-key "C-c ;"          #'iedit-mode)
(bind-key "s-;"
          (defhydra iedit-commands
            (:color blue :hint nil)
            "
_RET_ toggle iedit mode     _s_ Search iedit
_;_ toggle selection        _[_ expand up a line      _d_ Delete all
_\"_ toggle occurence lines  _]_ expand down a line   _m_ Multiple Cursors
_'_ toggle context lines    _<_ goto first
_h_ only this function      _>_ goto last
_i_ only this line
"
            ("RET" iedit-mode)
            ("s" (progn
                   (isearch-forward)
                   (iedit-mode-from-isearch)))
            (";" iedit-toggle-selection)
            ("\"" iedit-show/hide-occurrence-lines)
            ("'" iedit-show/hide-context-lines)
            ("h" iedit-restrict-function)
            ("i" iedit-restrict-current-line)
            ("[" iedit-expand-up-a-line)
            ("]" iedit-expand-down-a-line)
            ("<" iedit-goto-first-occurrence)
            (">" iedit-goto-last-occurrence)
            ("d" iedit-delete-occurrences)
            ("m" iedit-switch-to-mc-mode)))

(bind-key "C-c ]"          #'embrace-commander)

(bind-key "C-c f i"        #'insert-file)

(bind-key "C-x x m"        #'xterm-mouse-mode)
(bind-key "C-c l b"        #'lambda-line-mode)

(bind-key "C-c k H"        #'+rgb/kurecolor-hydra/body)
(bind-key "C-c k X"        #'kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgba)
(bind-key "C-c k c"        #'kurecolor-cssrgb-at-point-or-region-to-hex)
(bind-key "C-c k h a"      #'kurecolor-hexcolor-at-point-or-region-to-css-rgba)
(bind-key "C-c k h r"      #'kurecolor-hexcolor-at-point-or-region-to-css-rgb)
(bind-key "C-c k s"        #'search-for-nearest-hex-color)
(bind-key "C-c k x"        #'kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgb)

(bind-key "C-c q e"        #'edit-server-start)

(bind-key "C-c r ."        #'sp-join-sexp)
(bind-key "C-c r /"        #'sp-split-sexp)
(bind-key "C-c r <down>"   #'sp-backward-barf-sexp)
(bind-key "C-c r <left>"   #'sp-slurp-hybrid-sexp)
(bind-key "C-c r <right>"  #'sp-forward-barf-sexp)

(bind-key "s-&"            #'comint-run)
(bind-key "s-|"            #'shell-command-on-region-replace)
(bind-key "s-\\"            #'ocodo/shell-command-to-insert)

(bind-key "C-c s-e"        #'eval-and-replace)
(bind-key "C-c l e i"      #'eval-print-last-sexp)

(bind-key "C-c l l"        (defhydra eval-commands (:color blue)
                             "Eval"
                             ("RET" eval-and-replace "replace")
                             ("l" eval-buffer "buffer")
                             ("r" eval-region "region")
                             (";" eval-defun "defun")))

(bind-key "C-c l h"        #'hl-line-mode)
(bind-key "C-c l v"        #'vline-mode)

(bind-key "C-c t t -"      #'dasherise-at-point-or-region)
(bind-key "C-c t t ."      #'hex-to-decimal-at-point-or-region)
(bind-key "C-c t t /"      #'decimal-to-hex-at-point-or-region)
(bind-key "C-c t t U"      #'url-encode-string-at-point)
(bind-key "C-c t t _"      #'snake-case-at-point-or-region)
(bind-key "C-c t t h"      #'humanize-at-point-or-region)
(bind-key "C-c t t l"      #'lower-camelcase-at-point-or-region)
(bind-key "C-c t t s"      #'time-to-seconds-at-point-or-region)
(bind-key "C-c t t t"      #'titleized-at-point-or-region)
(bind-key "C-c t t u"      #'upper-camelcase-at-point-or-region)
(bind-key "C-c t t <tab>"    #'tabify)

(bind-key "C-x /"          #'align-regexp)

(bind-key "C-x f"          #'projectile-ripgrep)
(bind-key "C-x x ."        #'er/expand-region)

(bind-key "M-`"            #'magit)

(bind-key "M-o"            #'dired-osx-open-this-file dired-mode-map)

(bind-key "M-s-g g"        #'google-this)
(bind-key "M-s-g n"        #'google-this-noconfirm)

(bind-key "M-z"            #'zap-up-to-char)

(bind-key "s-'"            #'other-window)
(bind-key "s-0"            #'delete-window)
(bind-key "s-1"            #'delete-other-windows)
(bind-key "s-2"            #'split-window-below)
(bind-key "s-3"            #'split-window-right)
(bind-key "s-4"            #'toggle-window-split)
(bind-key "C-x 4 w t"      #'toggle-window-split)
(bind-key "s-5"            #'balance-windows-area)
(bind-key "s-6"            #'ace-swap-window)
(bind-key "C-x 4 w s"      #'ace-swap-window)
(bind-key "s-7"            #'clone-frame)
(bind-key "s-^"            #'join-lines-in-sexp)
(bind-key "s-`"            #'other-frame)
(bind-key "s-w"            #'delete-frame)

(bind-key "s-/"            #'hippie-expand)

(bind-key "s-<down>"       #'duplicate-current-line-or-region)
(bind-key "s-<up>"         #'duplicate-current-line-or-region-up)

(bind-key "s-<down>"       #'duplicate-current-line-or-region)
(bind-key "s-<up>"         #'duplicate-current-line-or-region-up)

(bind-key "M-S-<down>"     #'duplicate-current-line-or-region)
(bind-key "M-S-<up>"       #'duplicate-current-line-or-region-up)

(bind-key "s-<left>"       #'previous-buffer)
(bind-key "s-<right>"      #'next-buffer)

(bind-key "s-B"            #'ibuffer)
(bind-key "s-b"            #'ivy-switch-buffer)

(bind-key "s-U"            #'revert-buffer-instant)
(bind-key "C-c u"          #'revert-buffer-instant)

(bind-key "s-s"            #'save-buffer)
(bind-key "s-t"            #'projectile-find-file)
(bind-key "s-o"            #'find-file)
(bind-key "s-k"            #'kill-this-buffer)
(bind-key "C-c k k"        #'kill-this-buffer)
(bind-key "C-x k"          #'kill-this-buffer)
(bind-key "C-c f w"        #'write-region)

(bind-key "s-y"            #'yank-from-kill-ring)
(bind-key "C-c y"          #'yank-from-kill-ring)

;; --- mode/keymap specific ---
;; --- Magit ---
(bind-key "s-p" #'magit-push-current-to-upstream magit-status-mode-map)
(bind-key "M-s-p" #'ocodo/git-pull-and-push magit-status-mode-map)
(bind-key "s-f" #'magit-pull-from-upstream magit-status-mode-map)

(bind-key "C-c g p" #'magit-push-current-to-upstream magit-status-mode-map)
(bind-key "C-c g P" #'ocodo/git-pull-and-push magit-status-mode-map)
(bind-key "C-c g f" #'magit-pull-from-upstream magit-status-mode-map)

(bind-key "M-<left>" #'backward-word)
(bind-key "M-<right>" #'forward-word)

(bind-key "<mouse-8>" 'previous-buffer)
(bind-key "<mouse-9>" 'next-buffer)

(provide 'key-bindings)
;;; key-bindings.el ends here
