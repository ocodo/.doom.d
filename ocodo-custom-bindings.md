# Ocodo's Doom Emacs Custom Key Bindings.

### General

| Key(s)  | Command | keymap  |
|:--------|:--------|--------:|
| C-% | Anzu query replace | Global |
| C-( | Decrement number at point | Global |
| C-) | Increment number at point | Global |
| C-⮐ | Cua rectangle mark mode | Global |
| C-? | Cua rectangle which key help | Global |
| C-M-% | Anzu query replace regexp | Global |
| C-M-^ | Join line from below | Global |
| C-c ; | Iedit mode | Global |
| C-c ] | Embrace commander | Global |
| C-c f w | Write region | Global |
| C-c l e i | Eval print last sexp | Global |
| C-c l h | Hl line mode | Global |
| C-c s-e | Eval and replace | Global |
| C-x / | Align regexp | Global |
| C-x f | Ag | Global |
| C-x x . | Er expand region | Global |
| M-O | Macos open in default program | Global |
| M-` | Magit | Global |
| M-o | Dired osx open this file | Dired mode map |
| M-s-g g | Google this | Global |
| M-s-g n | Google this noconfirm | Global |
| M-z | Zap up to char | Global |
| s-/ | Hippie expand | Global |
| s-↓ | Duplicate current line or region | Global |
| s-← | Previous buffer | Global |
| s-→ | Next buffer | Global |
| s-↑ | Duplicate current line or region up | Global |
| s-B | Ibuffer | Global |
| s-T | Treemacs | Global |
| s-U | Revert buffer instant | Global |
| s-` | Other frame | Global |
| s-b | Ivy switch buffer | Global |
| s-k | Kill this buffer | Global |
| s-o | Find file | Global |
| s-q | Kill emacs | Global |
| s-s | Save buffer | Global |
| s-t | Projectile find file | Global |
| s-w | Delete frame | Global |
| s-y | Yank from kill ring | Global |
| s-\| | Shell command on region replace | Global |

### Markdown Soma

| Key(s)  | Command | keymap  |
|:--------|:--------|--------:|
| C-c S c | Markdown soma select css file | Markdown mode map |
| C-c S h | Markdown soma select highlight theme | Markdown mode map |
| C-c S s | Markdown soma mode | Markdown mode map |
| C-c S r | Markdown soma restart | Markdown mode map |

### Smart Parens

| Key(s)  | Command | keymap  |
|:--------|:--------|--------:|
| C-c r . | Sp join sexp | Global |
| C-c r / | Sp split sexp | Global |
| C-c r ↓ | Sp backward barf sexp | Global |
| C-c r ← | Sp slurp hybrid sexp | Global |
| C-c r → | Sp forward barf sexp | Global |

### Text Transforms

| Key(s)  | Command | keymap  |
|:--------|:--------|--------:|
| C-c t t - | Dasherise at point or region | Global |
| C-c t t . | Hex to decimal at point or region | Global |
| C-c t t / | Decimal to hex at point or region | Global |
| C-c t t U | Url encode string at point | Global |
| C-c t t _ | Snake case at point or region | Global |
| C-c t t h | Humanize at point or region | Global |
| C-c t t l | Lower camelcase at point or region | Global |
| C-c t t s | Time to seconds at point or region | Global |
| C-c t t t | Titleized at point or region | Global |
| C-c t t u | Upper camelcase at point or region | Global |

### Color

| Key(s)  | Command | keymap  |
|:--------|:--------|--------:|
| C-c k H | Rgb kurecolor hydra body | Global |
| C-c k X | Kurecolor xcode color literal at point or region to hex rgba | Global |
| C-c k c | Kurecolor cssrgb at point or region to hex | Global |
| C-c k h a | Kurecolor hexcolor at point or region to css rgba | Global |
| C-c k h r | Kurecolor hexcolor at point or region to css rgb | Global |
| C-c k s | Search for nearest hex color | Global |
| C-c k x | Kurecolor xcode color literal at point or region to hex rgb | Global |

### ERT Testing

| Key(s)  | Command | keymap  |
|:--------|:--------|--------:|
| C-c / t | Ert run all tests | Emacs lisp mode map |
| C-c / c | Ert delete all tests | Emacs lisp mode map |

### Debugging

| Key(s)  | Command | keymap  |
|:--------|:--------|--------:|
| C-x X d n | Edebug remove instrumentation | Global |
| C-x X d y | Edebug defun | Global |

### Windows

| Key(s)  | Command | keymap  |
|:--------|:--------|--------:|
| s-' | Other window | Global |
| s-0 | Delete window | Global |
| s-1 | Delete other windows | Global |
| s-2 | Split window below | Global |
| s-3 | Split window right | Global |
| s-4 | Toggle window split | Global |
| s-5 | Balance windows area | Global |