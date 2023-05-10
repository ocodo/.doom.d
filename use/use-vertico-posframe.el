(use-package vertico-posframe
  :config
   (defun ocodo/vertico-posframe-get-size (buffer)
     "Override of `vertico-posframe-get-size'"
     (let ((width (round (* (frame-width) 0.8))))
      (list
       :height vertico-posframe-height
       :width (min (or vertico-posframe-max-width)
                   999999
               (or vertico-posframe-min-width
                   width))
       :min-height (or vertico-posframe-min-height
                    (let ((height (+ vertico-count 1)))
                      (min height (or vertico-posframe-height height))))
       :min-width (or vertico-posframe-min-width
                   vertico-posframe-max-width
                   width))))

   (defun ocodo/posframe-poshandler-frame-center-eyelevel (info)
     "Posframe position handler to put the posframe at eye level,
  horizontally centered. Top position is fixed to avoid jittering
  when filtering.

  INFO can be found in docstring of `posframe-show'."
     (cons (- (/ (plist-get info :parent-frame-width) 2)
              (/ (plist-get info :posframe-width) 2))
           (- (/ (plist-get info :parent-frame-height) 4)
              (/ vertico-posframe-min-height 2))))

   (setq vertico-posframe-width 120
         vertico-posframe-max-height 20
         vertico-posframe-min-height 5
         vertico-posframe-max-width 120
         vertico-posframe-poshandler #'ocodo/posframe-poshandler-frame-center-eyelevel
         vertico-posframe-size-function #'ocodo/vertico-posframe-get-size
         ;vertico-posframe-size-function #'vertico-posframe-get-size
         vertico-posframe-border-width 20
         vertico-posframe-font "Avenir Next"
         vertico-posframe-parameters '((left-fringe . 0) (right-fringe . 0))
         vertico-posframe-show-minibuffer-rules '(".*"))

  (vertico-posframe-mode 1)
  (vertico-posframe-cleanup))
