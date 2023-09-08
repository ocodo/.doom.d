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

(tblui-define ocodo/gh-issue-list-tblui
              "GitHub Issues list"
              "Display issues in a tabulated list."
              ocodo/gh-issue-list-entries-provider
  [("number" 8 t)
   ("title" 100 nil)
   ("state" 8 nil)
   ("updatedAt" 25 nil)
   ("url" 60 nil)]
  ((:key "C" :name ocodo/gh-issue-create
    :funcs ((?C "Create a new issue" ocodo/gh-create-new-issue)))
   (:key "W" :name ocood/gh-issue-browse
    :funcs ((?W "Browse URL for current issue" ocodo/gh-issue-open-url)))))

(defun ocodo/gh-create-new-issue (&optional &rest _)
  "Create a new issue, called by the popup so the WRAPPED-ID is discardable."
  (interactive)
  (let* ((title (read-string "Issue Title: "))
         (body (read-string "Body: "))
         (command (format "gh issue create --title '%s' --body '%s'" title body)))
     (shell-command command (get-buffer "*Messages*"))))

(defun ocodo/gh-issue-list-json-shell-command-string ()
  "Return a gh issue list command to generate json."
  (format
   "gh issue list -s all --json %s"
   (ocodo/tblui-column-names-from-layout
    ocodo/gh-issue-list-tblui-tabulated-list-format t)))

(defun ocodo/gh-issue-open-url (&optional wrapped-id)
  "Open the url for the row-id in at WRAPPED-ID."
  (let* ((row-id (car wrapped-id))
         (row
          (cadr (--find (= row-id (car it))
                        (ocodo/gh-issue-list-entries-provider)))))
    (shell-command (format "open \"%s\"" (elt row 4)))))

(defun ocodo/gh-issue-list-entries-provider ()
  "List workflow runs for the current project as a tblui view.
Filter by WORKFLOW-NAME.

Project is defined by pwd/git repo."
  (ocodo/gh-list-hash-to-tblui-vector-list
   (ocodo/tblui-column-names-from-layout
    ocodo/gh-issue-list-tblui-tabulated-list-format)
   (json-parse-string
    (shell-command-to-string
     (ocodo/gh-issue-list-json-shell-command-string)))))

(defun ocodo/gh-issue-list ()
  "Show the current repo's gh workflow run list."
  (interactive)
  (ocodo/gh-issue-list-tblui-goto-ui)
  (setq-local tabulated-list-sort-key '("number" . t)))

;; ==========================================================================================

(tblui-define ocodo/gh-run-list-tblui
              "GitHub Workflow Runs list"
              "Display workflow runs in a tabulated list."
              ocodo/gh-run-list-entries-provider
  [("startedAt" 15 nil)
   ("url" 60 nil)
   ("status" 10 nil)
   ("event" 10 nil)
   ("workflowName" 15 nil)]
  ((:key "W" :name ocood/gh-run-list-browse
    :funcs ((?W "Browse URL for current run" ocodo/gh-run-list-open-url)))))

(defun ocodo/gh-workflow-names ()
  "List gh workflow names for current project.

Project is defined by git repo."
  (s-lines (shell-command-to-string "gh workflow list | cut -f1")))

(defun ocodo/gh-run-list-json-shell-command-string (&optional workflow-filter)
  "Return a gh run list command to generate json.
WORKFLOW-FILTER can be a --workflow filter or empty string."
  (format
   "gh run list %s --json %s"
   (or workflow-filter "")
   (ocodo/tblui-column-names-from-layout
    ocodo/gh-run-list-tblui-tabulated-list-format t)))

(defun ocodo/gh-run-list-open-url (&optional wrapped-id)
  "Open the url for the row at ROW-ID."
  (let* ((row-id (car wrapped-id))
         (row (cadr (--find (= row-id (car it))
                            (ocodo/gh-run-list-entries-provider)))))
    (shell-command (format "open \"%s\"" (elt row 1)))))

(defun ocodo/gh-run-list-entries-provider ()
  "List workflow runs for the current project as a tblui view.
Filter by WORKFLOW-NAME.

Project is defined by pwd/git repo."
  (ocodo/gh-list-hash-to-tblui-vector-list
   (ocodo/tblui-column-names-from-layout
    ocodo/gh-run-list-tblui-tabulated-list-format)
   (json-parse-string
    (shell-command-to-string
     (ocodo/gh-run-list-json-shell-command-string)))))

(defun ocodo/gh-run-list ()
  "Show the current repo's gh workflow run list."
  (interactive)
  (ocodo/gh-run-list-tblui-goto-ui))

;; ==========================================================================================

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
