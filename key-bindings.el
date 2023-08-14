;;; key-bindings --- Custom key bindings
;;; Commentary:
;;  Customized key bindings
;;; Code:

(require 'bind-key)
(require 'ocodo-handy-functions)

(set-face-attribute 'default nil :height 240)

;;; Zoom
;;;
(defun ocodo/default-face-size-decrease ()
  "Decrease the default face size."
  (interactive)
  (ocodo/default-face-size-adjust -10))

(defun ocodo/default-face-size-increase ()
  "Increase the default face size."
  (interactive)
  (ocodo/default-face-size-adjust 10))

(defun ocodo/default-face-size-adjust (amount)
  "Adjust the default face size by AMOUNT."
  (let* ((current (face-attribute 'default :height))
         (size (+ amount current)))
    (message "Resize default face to: %i (delta: %i, current: %i)"
             size amount current)
    (set-face-attribute 'default nil :height size)))

(defun ocodo/default-face-size-reset ()
  "Reset the default face size to 230."
  (interactive)
  (set-face-attribute 'default nil :height 230))

(unbind-key "s-=")
(unbind-key "s--")

(bind-key "s-=" #'text-scale-increase)
(bind-key "s--" #'text-scale-decrease)

(bind-key "M-s-=" #'ocodo/default-face-size-increase)
(bind-key "M-s--" #'ocodo/default-face-size-decrease)
(bind-key "M-s-0" #'ocodo/default-face-size-reset)
(bind-key "s-)" #'text-scale-adjust)

(bind-key "s-<mouse-1>" #'browse-url-at-mouse)

(bind-key "M-s-o" #'ocodo-open-project)

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

(defun ocodo/kill-ring-save-buffer ()
  "Copy the whole buffer to the kill ring."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (kill-ring-save)))

(bind-key "s-a"
          (defhydra avy-commands
            (:color blue :hint nil)
            "
Region             Avy              Flycheck
[_a_]   Select all   [_m_] Move line    [_e_] Errors
[_SPC_] Expand       [_j_] Goto line    [_n_] Next error
[_w_]   Copy all     [_g_] Goto char
[_TAB_] Indent all
"
            ("a" mark-whole-buffer)
            ("SPC" er/expand-region)
            ("j" avy-goto-line)
            ("m" avy-move-line)
            ("g" avy-goto-char)
            ("e" consult-flycheck)
            ("TAB" indent-buffer)
            ("w" ocodo/kill-ring-save-buffer)
            ("n" flycheck-next-error)))

(map!
;; Unbind C-RET on global key map
   "C-RET"      nil
   [C-return]   nil
   "C-S-RET"    nil
   [C-S-return] nil
;; unbind load theme
   "C-H t" nil)

(bind-key "C-H t" #'load-theme)

(bind-key "s-q"            #'kill-emacs)

(bind-key "C-("            #'decrement-number-at-point)
(bind-key "C-)"            #'increment-number-at-point)

(bind-key "C-<return>"     #'cua-rectangle-mark-mode)
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

(bind-key "C-c s-e"        #'eval-and-replace)
(bind-key "C-c l e i"      #'eval-print-last-sexp)

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
(bind-key "C-c t t TAB"    #'tabify)

(bind-key "C-x /"          #'align-regexp)

(bind-key "C-x f"          #'ag)
(bind-key "C-x x ."        #'er/expand-region)

(bind-key "M-O"            #'+macos/open-in-default-program)

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
(bind-key "s-5"            #'balance-windows-area)
(bind-key "s-6"            #'ace-swap-window)
(bind-key "s-^"            #'join-lines-in-sexp)
(bind-key "s-`"            #'other-frame)
(bind-key "s-w"            #'delete-frame)

(bind-key "s-/"            #'hippie-expand)
(bind-key "s-<down>"       #'duplicate-current-line-or-region)
(bind-key "s-<up>"         #'duplicate-current-line-or-region-up)

(bind-key "s-<left>"       #'previous-buffer)
(bind-key "s-<right>"      #'next-buffer)
(bind-key "s-B"            #'ibuffer)
(bind-key "s-b"            #'ivy-switch-buffer)
(bind-key "s-U"            #'revert-buffer-instant)
(bind-key "s-s"            #'save-buffer)
(bind-key "s-t"            #'projectile-find-file)
(bind-key "s-o"            #'find-file)
(bind-key "s-k"            #'kill-this-buffer)
(bind-key "C-c f w"        #'write-region)

(bind-key "s-y"            #'yank-from-kill-ring)
(bind-key "s-T"            #'treemacs)

; --- mode/keymap specific ---

; --- Magit ---
(bind-key "s-p" #'magit-push-current-to-upstream magit-status-mode-map)
(bind-key "s-f" #'magit-pull-from-upstream magit-status-mode-map)

(provide 'key-bindings)
;;; key-bindings.el ends here
