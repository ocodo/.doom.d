# .doom.d

My attempt so far to discard `~/.ocodo.emacs` 

FWIW I thoroughly recommend Doom Emacs.

- Migrated `handy-functions.el` ⟶ `ocodo-handy-functions.el`
- Set font to SauceCodePro Nerd Font extra light
- turn off hl-line mode when rainbow-mode is switched on
- bind keys for...

| key                           | command                                                      | notes |
|-------------------------------|--------------------------------------------------------------|-------|
| <kbd>M-s-g g</kbd>            | google-this                                                  |       |
| <kbd>M-s-g n</kbd>            | google-this-noconfirm                                        |       |
| <kbd>C-x x .</kbd>            | er/expand-region                                             |       |
| <kbd>C-x f</kbd>              | ag                                                           |       |
| <kbd>C-c ]</kbd>              | embrace-commander                                            |       |
| <kbd>C-x r &lt;left&gt;</kbd> | sp-slurp-hybrid-sexp                                         |       |
| <kbd>s-'</kbd>                | other-window                                                 |       |
| <kbd>s-0</kbd>                | delete-window                                                |       |
| <kbd>s-1</kbd>                | delete-other-windows                                         |       |
| <kbd>s-2</kbd>                | split-window-below                                           |       |
| <kbd>s-3</kbd>                | split-window-right                                           |       |
| <kbd>s-&#96;</kbd>            | other-frame                                                  |       |
| <kbd>s-w</kbd>                | delete-frame                                                 |       |
| <kbd>s-b</kbd>                | switch-to-buffer                                             |       |
| <kbd>s-&lt;right&gt;</kbd>    | next-buffer                                                  |       |
| <kbd>s-&lt;left&gt;</kbd>     | previous-buffer                                              |       |
| <kbd>s-o</kbd>                | find-file                                                    |       |
| <kbd>s-s</kbd>                | save-buffer                                                  |       |
| <kbd>s-k</kbd>                | kill-this-buffer                                             |       |
| <kbd>s-t</kbd>                | projectile-find-file-dwim                                    |       |
| <kbd>s-q</kbd>                | kill-emacs                                                   |       |
| <kbd>s-T</kbd>                | treemacs                                                     |       |
| <kbd>C-x /</kbd>              | align-regexp                                                 |       |
| <kbd>s-&lt;up&gt;</kbd>       | duplicate-current-line-or-region-up                          |       |
| <kbd>s-&lt;down&gt;</kbd>     | duplicate-current-line-or-region                             |       |
| <kbd>s-\|</kbd>               | shell-command-on-region-replace                              |       |
| <kbd>C-)</kbd>                | increment-number-at-point                                    |       |
| <kbd>C-(</kbd>                | decrement-number-at-point                                    |       |
| <kbd>C-c l h</kbd>            | hl-line-mode                                                 |       |
| <kbd>M-o</kbd>                | dired-osx-open-this-file dired-mode-map                      |       |
| <kbd>M-O</kbd>                | +macos/open-in-default-program                               |       |
| <kbd>C-c ;</kbd>              | iedit-mode                                                   |       |
| <kbd>M-&#96;</kbd>            | magit                                                        |       |
| <kbd>M-z</kbd>                | zap-up-to-char                                               |       |
| <kbd>C-M-^</kbd>              | join-line-from-below                                         |       |
| <kbd>s-^</kbd>                | join-line-from-below                                         |       |
| <kbd>C-&lt;return&gt;</kbd>   | cua-rectangle-mark-mode                                      |       |
| <kbd>C-?</kbd>                | show cua-rectangle help                                      |       |
| <kbd>C-s-H</kbd>              | #hippie-expand                                               |       |
| <kbd>C-c f w</kbd>            | #write-region                                                |       |
| <kbd>C-c s-e</kbd>            | #eval-and-replace                                            |       |
| <kbd>C-c K</kbd>              | +rgb/kurecolor-hydra/body                                    |       |
| <kbd>C-c k H</kbd>            | +rgb/kurecolor-hydra/body                                    |       |
| <kbd>C-c k x</kbd>            | kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgb  |       |
| <kbd>C-c k X</kbd>            | kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgba |       |
| <kbd>C-c k c</kbd>            | kurecolor-cssrgb-at-point-or-region-to-hex                   |       |
| <kbd>C-c k h</kbd>            | kurecolor-hexcolor-at-point-or-region-to-css-rgba            |       |
| <kbd>C-c t t h</kbd>          | humanize-at-point-or-region                                  |       |
| <kbd>C-c t t -</kbd>          | dasherise-at-point-or-region                                 |       |
| <kbd>C-c t t _</kbd>          | snake-case-at-point-or-region                                |       |
| <kbd>C-c t t l</kbd>          | lower-camelcase-at-point-or-region                           |       |
| <kbd>C-c t t u</kbd>          | upper-camelcase-at-point-or-region                           |       |
| <kbd>C-c t t t</kbd>          | titleized-at-point-or-region                                 |       |
| <kbd>C-c t t U</kbd>          | url-encode-string-at-point                                   |       |
| <kbd>C-c t t .</kbd>          | hex-to-decimal-at-point-or-region                            |       |
| <kbd>C-c t t /</kbd>          | decimal-to-hex-at-point-or-region                            |       |
| <kbd>C-c t t v</kbd>          | video-time-to-seconds-at-point-or-region                     |       |


# Special functions

- `ssh-agent-env-fix`
- `ocodo-sh-indent-rules`

# Extra packages

- `google-this`
- `iedit`
- `creamsody-theme`
- `autothemer`
- `edit-server`
- `ag`
- `embrace`
- `swiper`
- `try`
- `free-keys`
- `flymake-shellcheck`
- unpin `kurecolor` grab from: `emacsfodder/kurecolor`
- `text-transformers` grab from: `jasonm23/emacs-text-transformers`

