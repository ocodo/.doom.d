![](./doom-vapourwave.png)

# .doom.d

My attempt so far to discard `~/.ocodo.emacs` 

FWIW I thoroughly recommend Doom Emacs.

- Migrated [`handy-functions.el` ⟶  `ocodo-handy-functions.el`](./ocodo-handy-functions.md)
- Set font to SauceCodePro Nerd Font extra light
- turn off hl-line mode when rainbow-mode is switched on

## Custom Key bindings

| command                                                      | key                           |
|--------------------------------------------------------------|-------------------------------|
| +macos/open-in-default-program                               | <kbd>M-O</kbd>                |
| +rgb/kurecolor-hydra/body                                    | <kbd>C-c K</kbd>              |
| +rgb/kurecolor-hydra/body                                    | <kbd>C-c k H</kbd>            |
| ag                                                           | <kbd>C-x f</kbd>              |
| align-regexp                                                 | <kbd>C-x /</kbd>              |
| cua-rectangle-mark-mode                                      | <kbd>C-&lt;return&gt;</kbd>   |
| dasherise-at-point-or-region                                 | <kbd>C-c t t -</kbd>          |
| decimal-to-hex-at-point-or-region                            | <kbd>C-c t t /</kbd>          |
| decrement-number-at-point                                    | <kbd>C-(</kbd>                |
| delete-frame                                                 | <kbd>s-w</kbd>                |
| delete-other-windows                                         | <kbd>s-1</kbd>                |
| delete-window                                                | <kbd>s-0</kbd>                |
| dired-osx-open-this-file dired-mode-map                      | <kbd>M-o</kbd>                |
| duplicate-current-line-or-region                             | <kbd>s-&lt;down&gt;</kbd>     |
| duplicate-current-line-or-region-up                          | <kbd>s-&lt;up&gt;</kbd>       |
| embrace-commander                                            | <kbd>C-c ]</kbd>              |
| er/expand-region                                             | <kbd>C-x x .</kbd>            |
| eval-and-replace                                             | <kbd>C-c s-e</kbd>            |
| find-file                                                    | <kbd>s-o</kbd>                |
| google-this                                                  | <kbd>M-s-g g</kbd>            |
| google-this-noconfirm                                        | <kbd>M-s-g n</kbd>            |
| hex-to-decimal-at-point-or-region                            | <kbd>C-c t t .</kbd>          |
| hippie-expand                                                | <kbd>C-s-H</kbd>              |
| hl-line-mode                                                 | <kbd>C-c l h</kbd>            |
| humanize-at-point-or-region                                  | <kbd>C-c t t h</kbd>          |
| iedit-mode                                                   | <kbd>C-c ;</kbd>              |
| increment-number-at-point                                    | <kbd>C-)</kbd>                |
| join-line-from-below                                         | <kbd>C-M-^</kbd>              |
| join-line-from-below                                         | <kbd>s-^</kbd>                |
| kill-emacs                                                   | <kbd>s-q</kbd>                |
| kill-this-buffer                                             | <kbd>s-k</kbd>                |
| kurecolor-cssrgb-at-point-or-region-to-hex                   | <kbd>C-c k c</kbd>            |
| kurecolor-hexcolor-at-point-or-region-to-css-rgba            | <kbd>C-c k h</kbd>            |
| kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgb  | <kbd>C-c k x</kbd>            |
| kurecolor-xcode-color-literal-at-point-or-region-to-hex-rgba | <kbd>C-c k X</kbd>            |
| lower-camelcase-at-point-or-region                           | <kbd>C-c t t l</kbd>          |
| magit                                                        | <kbd>M-&#96;</kbd>            |
| next-buffer                                                  | <kbd>s-&lt;right&gt;</kbd>    |
| other-frame                                                  | <kbd>s-&#96;</kbd>            |
| other-window                                                 | <kbd>s-'</kbd>                |
| previous-buffer                                              | <kbd>s-&lt;left&gt;</kbd>     |
| projectile-find-file-dwim                                    | <kbd>s-t</kbd>                |
| save-buffer                                                  | <kbd>s-s</kbd>                |
| shell-command-on-region-replace                              | <kbd>s-\|</kbd>               |
| show cua-rectangle help                                      | <kbd>C-?</kbd>                |
| snake-case-at-point-or-region                                | <kbd>C-c t t _</kbd>          |
| sp-slurp-hybrid-sexp                                         | <kbd>C-x r &lt;left&gt;</kbd> |
| split-window-below                                           | <kbd>s-2</kbd>                |
| split-window-right                                           | <kbd>s-3</kbd>                |
| switch-to-buffer                                             | <kbd>s-b</kbd>                |
| titleized-at-point-or-region                                 | <kbd>C-c t t t</kbd>          |
| treemacs                                                     | <kbd>s-T</kbd>                |
| upper-camelcase-at-point-or-region                           | <kbd>C-c t t u</kbd>          |
| url-encode-string-at-point                                   | <kbd>C-c t t U</kbd>          |
| video-time-to-seconds-at-point-or-region                     | <kbd>C-c t t v</kbd>          |
| write-region                                                 | <kbd>C-c f w</kbd>            |
| zap-up-to-char                                               | <kbd>M-z</kbd>                |

---
## Markdown mode custom bindings

| Command                           | Binding            |
|-----------------------------------|--------------------|
| markdown-soma-mode                | <kbd>C-c S s</kbd> |
| markdown-soma-restart             | <kbd>C-c S r</kbd> |
| markdown-soma-set-highlight-theme | <kbd>C-c S h</kbd> |
| markdown-soma-set-css-file        | <kbd>C-c S c</kbd> |
| markdown-soma-set-css-file        |                    |

## Emacs lisp mode custom bindings

| Command              | Binding            |
|----------------------|--------------------|
| ert-run-all-tests    | <kbd>C-c / t</kbd> |
| ert-delete-all-tests | <kbd>C-c / c</kbd> |

---

## Special functions - init.el

- `ssh-agent-env-fix`
- `ocodo-sh-indent-rules`

## Custom installed packages

- `ag`
- `autothemer`
- `consult-spotify`
- `creamsody-theme`
- `edit-server`
- `embrace`
- `flymake-shellcheck`
- `free-keys`
- `google-this`
- `iedit`
- `iedit`
- `kurecolor` grab from: `emacsfodder/kurecolor`
- `kv`
- `lorem-ipsum`
- `lsp-sourcekit`
- `markdown-soma`
- `mpdel`
- `nameless`
- `rainbow-identifiers`
- `swiper`
- `text-transformers` grab from: `jasonm23/emacs-text-transformers`
- `try`
- `xr`

