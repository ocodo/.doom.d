;;; plugins/ocodo-register.el -*- lexical-binding: t; -*-
(require 'cl-lib)

(defun ocodo/register-list ()
  "List of registers for completing-read."
  (mapcar
   (lambda (entry)
     (let* ((entry-text (cdr entry))
            (entry-len (length entry-text)))
       (format "%s: %s"
        (single-key-description (car entry))
        (substring-no-properties (cdr entry) 0 (min 40 entry-len)))))
   register-alist))

(defun ocodo/register-kill ()
  "Kill REGISTER."
  (interactive)
  (if (length> register-alist 0)
      (let* ((register-entry (completing-read "Register to kill: " (ocodo/register-list)))
             (register-index (cl-position register-entry (ocodo/register-list)))
             (register-key (nth register-index register-alist)))
          (assoc-delete-all register-key register-alist)
          (message "Register deleted: %s" register-entry))
    (message "No registers defined")))

(defun ocodo/register-kill-all ()
  "Kill all registers."
  (interactive)
  (if (length> register-alist 0)
      (progn
       (setq register-alist '())
       (message "Registers deleted"))
    (message "No registers defined")))

(provide 'ocodo-register)
;;; ocodo-register.el ends here
