;;; github-actions-mode --- Minor mode for github-actions
;;;
;;; Commentary:
;;; Used to add actionlint flycheck checker and github environment var names
;;;
;;; Code:

(require 'flycheck)
(require 'company-dabbrev)

(flycheck-define-checker yaml-actionlint
  "A YAML syntax checker using actionlint."
  :command ("actionlint" "-oneline" source)
  :error-patterns ((error line-start (file-name) ":" line ":" column ": " (message) line-end))
  :modes yaml-mode)

(add-to-list 'flycheck-checkers 'yaml-actionlint)

(define-minor-mode github-actions-mode
  "Minor mode for GitHub Actions related features."
  :init-value nil
  :lighter " GH-Actions"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-a c") 'github-actions-dabbrev-expand)
            map)

  (add-to-list 'company-backends '(company-capf company-dabbrev))
  (yaml-actionlint-setup))

(defun yaml-actionlint-setup ()
  "Setup actionlint as a Flycheck checker for yaml-mode."
  (interactive)
  (when (eq major-mode 'yaml-mode)
    (flycheck-add-next-checker 'yaml-yamllint 'yaml-actionlint)))

(defvar github-actions-dabbrev-abbrevs
  '(("summary" . "$GITHUB_STEP_SUMMARY")
    ("secrets" . "$GITHUB_SECRETS")))

(provide 'github-actions-mode)
;;; github-actions-mode.el ends here
