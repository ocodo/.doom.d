;;; ocodo-gh-lists.el --- github lists as tabulated-lists workflow runs, issues...  -*- lexical-binding: t; -*-
;; Copyright (C) 2023  Jason M23
;; Author: Jason M23 <jasonm23@gmail.com>
;;
;;; Commentary:
;;; gh cli lists - to emacs tabulated lists
;;;
;;; Code:

(require 's)
(require 'dash)
(require 'tblui)

(defun ocodo/gh-list-hash-to-tblui-vector-list (columns data)
  "Convert DATA to a tblui ready vector list"
  (let* ((result '()))
    (dotimes (i (length data))
      (let* ((hash (aref data i))
             (values (mapcar (lambda (key) (format "%s" (gethash key hash))) columns)))
        (push (list i (apply 'vector values)) result)))
    result))

(defun ocodo/tblui-column-names-from-layout (layout &optional csv)
  "Extract column names from a tblui LAYOUT.

optional parameter CSV retuns a csv string intead of a list."
  (let ((result (--map (car it) layout)))
    (if csv
        (mapconcat 'identity result ",")
      result)))

(defun ocodo/tblui-entry-column-as-int (entry column)
  "Get the COLUMN value in ENTRY as int."
  (string-to-number (elt (cadr entry) column)))

(provide 'ocodo-gh-lists)
