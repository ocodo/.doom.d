# Box drawing

Draw boxes with unicode box characters.
- activate with <kbd>C-c r B</kbd>

Using QWERTY layout draw 5 different box styles. Hydra powers the UI.

```
╭───────────────────────────────────────────────────[ESC quit]╮
│ Box Drawing                                                 │
├─────────────────────────────────────────────────────────────┤
│ r ─         R ═         v │         V ║                     │
│ q ┌ w ┬ e ┐ Q ╒ W ╤ E ╕ t ╔ y ╦ u ╗ T ╓ Y ╥ U ╖ C-q ╭ C-w ╮ │
│ a ├ s ┼ d ┤ A ╞ S ╪ D ╡ g ╠ h ╬ j ╣ G ╟ H ╫ J ╢ C-a ╰ C-s ╯ │
│ z └ x ┴ c ┘ Z ╘ X ╧ C ╛ b ╚ n ╩ m ╝ B ╙ N ╨ M ╜             │
╰─────────────────────────────────────────[i overwrite/insert]╯
```

## Setup

Place this file in the Emacs lisp load-path

```
M-x load-library RET box-drawing
```
