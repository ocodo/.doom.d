;;; plugins/ocodo-gh-run-list.el -*- lexical-binding: t; -*-

(require 'ocodo-gh-lists)

(tblui-define ocodo/gh-run-list-tblui
              "GitHub Workflow Runs list"
              "Display workflow runs in a tabulated list."
              ocodo/gh-run-list-entries-provider
  [("workflowName" 15 nil)
   ("startedAt" 15 nil)
   ("status" 10 nil)
   ("event" 10 nil)
   ("url" 60 nil)]

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
    (shell-command (format "open \"%s\"" (elt row 4)))))

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

(provide 'gh-run-list)
