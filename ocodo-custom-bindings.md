# Ocodo's Emacs Key Bindings.

 Customized key bindings for the global-key-map

### General

| Key(s)               | Command                             | keymap |
|:---------------------|:------------------------------------|-------:|
| <kbd>C-x f</kbd>     | Ag                                  | Global |
| <kbd>C-x /</kbd>     | Align regexp                        | Global |
| <kbd>C-%</kbd>       | Anzu query replace                  | Global |
| <kbd>C-M-%</kbd>     | Anzu query replace regexp           | Global |
| <kbd>s-&</kbd>       | Comint run                          | Global |
| <kbd>C-⮐</kbd>       | Cua rectangle mark mode             | Global |
| <kbd>C-?</kbd>       | Cua rectangle which key help        | Global |
| <kbd>C-(</kbd>       | Decrement number at point           | Global |
| <kbd>s-w</kbd>       | Delete frame                        | Global |
| <kbd>s-↓</kbd>       | Duplicate current line or region    | Global |
| <kbd>s-↑</kbd>       | Duplicate current line or region up | Global |
| <kbd>C-c q e</kbd>   | Edit server start                   | Global |
| <kbd>C-c ]</kbd>     | Embrace commander                   | Global |
| <kbd>C-x x .</kbd>   | Er expand region                    | Global |
| <kbd>C-c s-e</kbd>   | Eval and replace                    | Global |
| <kbd>C-c l e i</kbd> | Eval print last sexp                | Global |
| <kbd>s-o</kbd>       | Find file                           | Global |
| <kbd>M-s-g g</kbd>   | Google this                         | Global |
| <kbd>M-s-g n</kbd>   | Google this noconfirm               | Global |
| <kbd>s-/</kbd>       | Hippie expand                       | Global |
| <kbd>C-c l h</kbd>   | Hl line mode                        | Global |
| <kbd>s-B</kbd>       | Ibuffer                             | Global |
| <kbd>C-c ;</kbd>     | Iedit mode                          | Global |
| <kbd>C-)</kbd>       | Increment number at point           | Global |
| <kbd>s-b</kbd>       | Ivy switch buffer                   | Global |
| <kbd>C-M-^</kbd>     | Join line from below                | Global |
| <kbd>s-q</kbd>       | Kill emacs                          | Global |
| <kbd>s-k</kbd>       | Kill this buffer                    | Global |
| <kbd>M-O</kbd>       | Macos open in default program       | Global |
| <kbd>M-`</kbd>       | Magit                               | Global |
| <kbd>s-→</kbd>       | Next buffer                         | Global |
| <kbd>s-`</kbd>       | Other frame                         | Global |
| <kbd>s-←</kbd>       | Previous buffer                     | Global |
| <kbd>s-t</kbd>       | Projectile find file                | Global |
| <kbd>s-U</kbd>       | Revert buffer instant               | Global |
| <kbd>s-s</kbd>       | Save buffer                         | Global |
| <kbd>s-\|</kbd>      | Shell command on region replace     | Global |
| <kbd>s-T</kbd>       | Treemacs                            | Global |
| <kbd>C-c f w</kbd>   | Write region                        | Global |
| <kbd>s-y</kbd>       | Yank from kill ring                 | Global |
| <kbd>M-z</kbd>       | Zap up to char                      | Global |

### Markdown Soma

| Key(s)             | Command                              |            keymap |
|:-------------------|:-------------------------------------|------------------:|
| <kbd>C-c S s</kbd> | Markdown soma mode                   | Markdown mode map |
| <kbd>C-c S r</kbd> | Markdown soma restart                | Markdown mode map |
| <kbd>C-c S b</kbd> | Markdown soma select builtin css     | Markdown mode map |
| <kbd>C-c S c</kbd> | Markdown soma select css file        | Markdown mode map |
| <kbd>C-c S h</kbd> | Markdown soma select highlight theme | Markdown mode map |

### Smart Parens

| Key(s)             | Command               | keymap |
|:-------------------|:----------------------|-------:|
| <kbd>C-c r ↓</kbd> | Sp backward barf sexp | Global |
| <kbd>C-c r →</kbd> | Sp forward barf sexp  | Global |
| <kbd>C-c r .</kbd> | Sp join sexp          | Global |
| <kbd>C-c r ←</kbd> | Sp slurp hybrid sexp  | Global |
| <kbd>C-c r /</kbd> | Sp split sexp         | Global |

### Text Transforms

| Key(s)               | Command                            | keymap |
|:---------------------|:-----------------------------------|-------:|
| <kbd>C-c t t -</kbd> | Dasherise at point or region       | Global |
| <kbd>C-c t t /</kbd> | Decimal to hex at point or region  | Global |
| <kbd>C-c t t .</kbd> | Hex to decimal at point or region  | Global |
| <kbd>C-c t t h</kbd> | Humanize at point or region        | Global |
| <kbd>C-c t t l</kbd> | Lower camelcase at point or region | Global |
| <kbd>C-c t t _</kbd> | Snake case at point or region      | Global |
| <kbd>C-c t t s</kbd> | Time to seconds at point or region | Global |
| <kbd>C-c t t t</kbd> | Titleized at point or region       | Global |
| <kbd>C-c t t u</kbd> | Upper camelcase at point or region | Global |
| <kbd>C-c t t U</kbd> | Url encode string at point         | Global |

### Auto Snippet

| Key(s)                 | Command                          | keymap |
|:-----------------------|:---------------------------------|-------:|
| <kbd>C-c C-a s</kbd>   | Aya create                       | Global |
| <kbd>C-c C-a d</kbd>   | Aya delete from history          | Global |
| <kbd>C-c C-a e</kbd>   | Aya expand                       | Global |
| <kbd>C-c C-a SPC</kbd> | Aya expand from history          | Global |
| <kbd>C-c C-a n</kbd>   | Aya next in history              | Global |
| <kbd>C-c C-a w</kbd>   | Aya persist snippet from history | Global |
| <kbd>C-c C-a p</kbd>   | Aya previous in history          | Global |

### Ruby

| Key(s)               | Command          |        keymap |
|:---------------------|:-----------------|--------------:|
| <kbd>C-c l e d</kbd> | Ruby send block  | Ruby mode map |
| <kbd>C-c l e b</kbd> | Ruby send buffer | Ruby mode map |
| <kbd>C-c l e r</kbd> | Ruby send region | Ruby mode map |

### Swiper

| Key(s)             | Command    | keymap |
|:-------------------|:-----------|-------:|
| <kbd>C-c s w</kbd> | Swiper     | Global |
| <kbd>C-c s a</kbd> | Swiper all | Global |

### Git

| Key(s)               | Command                      | keymap |
|:---------------------|:-----------------------------|-------:|
| <kbd>C-x v ← b</kbd> | Vc browse at remote homepage | Global |
| <kbd>C-x v ← g</kbd> | Vc gutter hydra body         | Global |
| <kbd>C-x v ← s</kbd> | Vc smerge hydra body         | Global |

### Color

| Key(s)               | Command                                                      | keymap |
|:---------------------|:-------------------------------------------------------------|-------:|
| <kbd>C-c k c</kbd>   | Kurecolor cssrgb at point or region to hex                   | Global |
| <kbd>C-c k h r</kbd> | Kurecolor hexcolor at point or region to css rgb             | Global |
| <kbd>C-c k h a</kbd> | Kurecolor hexcolor at point or region to css rgba            | Global |
| <kbd>C-c k x</kbd>   | Kurecolor xcode color literal at point or region to hex rgb  | Global |
| <kbd>C-c k X</kbd>   | Kurecolor xcode color literal at point or region to hex rgba | Global |
| <kbd>C-c k H</kbd>   | Rgb kurecolor hydra body                                     | Global |
| <kbd>C-c k s</kbd>   | Search for nearest hex color                                 | Global |

### Dired

| Key(s)         | Command                  |         keymap |
|:---------------|:-------------------------|---------------:|
| <kbd>M-o</kbd> | Dired osx open this file | Dired mode map |

### ERT Testing

| Key(s)             | Command                       |              keymap |
|:-------------------|:------------------------------|--------------------:|
| <kbd>C-c / c</kbd> | Ert delete all tests          | Emacs lisp mode map |
| <kbd>C-c / b</kbd> | Ert eval buffer run all tests | Emacs lisp mode map |
| <kbd>C-c / l</kbd> | Ert load file run all tests   | Emacs lisp mode map |
| <kbd>C-c / /</kbd> | Ert run all tests             | Emacs lisp mode map |
| <kbd>C-c / t</kbd> | Ert run tests interactively   | Emacs lisp mode map |

### Debugging

| Key(s)               | Command                       | keymap |
|:---------------------|:------------------------------|-------:|
| <kbd>C-x X d y</kbd> | Edebug defun                  | Global |
| <kbd>C-x X d n</kbd> | Edebug remove instrumentation | Global |

### Windows

| Key(s)         | Command              | keymap |
|:---------------|:---------------------|-------:|
| <kbd>s-5</kbd> | Balance windows area | Global |
| <kbd>s-1</kbd> | Delete other windows | Global |
| <kbd>s-0</kbd> | Delete window        | Global |
| <kbd>s-'</kbd> | Other window         | Global |
| <kbd>s-2</kbd> | Split window below   | Global |
| <kbd>s-3</kbd> | Split window right   | Global |
| <kbd>s-4</kbd> | Toggle window split  | Global |
