;;; plugins/garbage-man.el -*- lexical-binding: t; -*-

(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
    `(let ((time (current-time)))
         ,@body
          (float-time (time-since time))))

;; Set garbage collection threshold to 1GB.
(setq gc-cons-threshold #x40000000)

;; When idle for 15sec run the GC no matter what.
(defvar k-gc-timer
  (run-with-idle-timer
   15 t
   (lambda ()
     (message
      "Garbage Collector has run for %.06fsec"
      (k-time (garbage-collect))))))
