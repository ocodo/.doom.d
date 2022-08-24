;;; box-drawing --- draw boxes
;;
;;; Commentary:
;; # Box drawing
;;
;; Draw boxes with unicode box characters.
;; - activate with <kbd>C-c r B</kbd>
;;
;; Using QWERTY layout draw 5 different box styles.
;; Hydra powers the UI.
;;
;; ```
;; ╭───────────────────────────────────────────────────[ESC quit]╮
;; │ Box Drawing                                                 │
;; ├─────────────────────────────────────────────────────────────┤
;; │ r ─         R ═         v │         V ║         [space]     │
;; │ q ┌ w ┬ e ┐ Q ╒ W ╤ E ╕ t ╔ y ╦ u ╗ T ╓ Y ╥ U ╖ C-q ╭ C-w ╮ │
;; │ a ├ s ┼ d ┤ A ╞ S ╪ D ╡ g ╠ h ╬ j ╣ G ╟ H ╫ J ╢ C-a ╰ C-s ╯ │
;; │ z └ x ┴ c ┘ Z ╘ X ╧ C ╛ b ╚ n ╩ m ╝ B ╙ N ╨ M ╜             │
;; ╰──────────────────────────────────────────────────[ insert  ]╯
;; [i]  insert  .
;; ```
;;
;; ## Setup
;;
;; Place this file in the Emacs lisp load-path
;;
;; ```
;; M-x load-library RET box-drawing
;; ```
;;; Code:

(defcustom box-drawing--overwrite nil "Overwrite mode for box drawing.")

(defun box-drawing--insert (char)
  "Insert CHAR.
If `box-drawing--overwrite' is non-nil, insert with overwrite."
  (interactive)
  (when box-drawing--overwrite
   (kill-forward-chars 1))
  (insert char))

(bind-key "C-c r B"
 (defhydra box-drawing (:color pink :hint nil) "
╭───────────────────────────────────────────────────[_ESC_ quit]╮
│ Box Drawing                                                 │
├─────────────────────────────────────────────────────────────┤
│ _r_ ─         _R_ ═         _v_ │         _V_ ║                     │
│ _q_ ┌ _w_ ┬ _e_ ┐ _Q_ ╒ _W_ ╤ _E_ ╕ _t_ ╔ _y_ ╦ _u_ ╗ _T_ ╓ _Y_ ╥ _U_ ╖ _C-q_ ╭ _C-w_ ╮ │
│ _a_ ├ _s_ ┼ _d_ ┤ _A_ ╞ _S_ ╪ _D_ ╡ _g_ ╠ _h_ ╬ _j_ ╣ _G_ ╟ _H_ ╫ _J_ ╢ _C-a_ ╰ _C-s_ ╯ │
│ _z_ └ _x_ ┴ _c_ ┘ _Z_ ╘ _X_ ╧ _C_ ╛ _b_ ╚ _n_ ╩ _m_ ╝ _B_ ╙ _N_ ╨ _M_ ╜             │
╰────────────────────────────────────────────────[ ?i? ]╯
"
                       ("ESC" nil nil :color blue) ;; Esc to exit.
                       ("q" (box-drawing--insert "┌")) ("w" (box-drawing--insert "┬")) ("e" (box-drawing--insert "┐"))
                       ("Q" (box-drawing--insert "╒")) ("W" (box-drawing--insert "╤")) ("E" (box-drawing--insert "╕"))
                       ("t" (box-drawing--insert "╔")) ("y" (box-drawing--insert "╦")) ("u" (box-drawing--insert "╗"))
                       ("T" (box-drawing--insert "╓")) ("Y" (box-drawing--insert "╥")) ("U" (box-drawing--insert "╖"))
                       ("a" (box-drawing--insert "├")) ("s" (box-drawing--insert "┼")) ("d" (box-drawing--insert "┤"))
                       ("A" (box-drawing--insert "╞")) ("S" (box-drawing--insert "╪")) ("D" (box-drawing--insert "╡"))
                       ("g" (box-drawing--insert "╠")) ("h" (box-drawing--insert "╬")) ("j" (box-drawing--insert "╣"))
                       ("G" (box-drawing--insert "╟")) ("H" (box-drawing--insert "╫")) ("J" (box-drawing--insert "╢"))
                       ("z" (box-drawing--insert "└")) ("x" (box-drawing--insert "┴")) ("c" (box-drawing--insert "┘"))
                       ("Z" (box-drawing--insert "╘")) ("X" (box-drawing--insert "╧")) ("C" (box-drawing--insert "╛"))
                       ("b" (box-drawing--insert "╚")) ("n" (box-drawing--insert "╩")) ("m" (box-drawing--insert "╝"))
                       ("B" (box-drawing--insert "╙")) ("N" (box-drawing--insert "╨")) ("M" (box-drawing--insert "╜"))
                       ("r" (box-drawing--insert "─")) ("R" (box-drawing--insert "═"))
                       ("v" (box-drawing--insert "│")) ("V" (box-drawing--insert "║"))
                       ("SPC" (box-drawing--insert " "))
                       ("C-q" (box-drawing--insert "╭")) ("C-w" (box-drawing--insert "╮"))
                       ("C-a" (box-drawing--insert "╰")) ("C-s" (box-drawing--insert "╯"))
                       ("i" (setq box-drawing--overwrite (not box-drawing--overwrite))
                        (if box-drawing--overwrite "overwrite" " insert  "))))

(provide 'box-drawing)
;;; box-drawing.el ends here
