;; screengrab-soma --- Take screengrabs of soma renders with each style available.

;;; Commentary:

;;  With each style start a

;;; Code:

(defun screengrab-soma ()
  "Generate screen captures of soma themes"
  (let ((markdown-soma-host-port "0")
        (markdown-soma-host-address "localhost")

        (screencapture-mac-default-commandline
         (format "screencapture -l %s -x -o"
                 (screencapture-mac--windowid-helper)))

        (screencapture-mac-default-file-location (expand-file-name"~/Desktop"))
        (screencapture-mac-default-file-keyword "soma-screencapture")
        (text (f-read-text "~/workspace/soma/code-samples.md")))

    (mapc
     (lambda (style)
       (let ((type (nth 0 style))
             (markdown-soma-custom-css (expand-file-name (nth 1 style))))

         (mapc
                                        ; loop over highlight styles
          (lambda (highlight-style)
            (let ((screencapture-mac-default-file-keyword
                   (format "soma-%s-%s" type highlight-style))
                  (markdown-soma-highlight-theme highlight-style))

              ;; - - 8< - - - - - -
              ;; run soma command
              (markdown-soma--run)
              ;; render string
              (markdown-soma-render text)
              ;; Wait a moment...
              (y-or-n-p-with-timeout "Waiting for reload...?" 3 t)
              ;; Click...
              (screencapture-mac)
              ;; kill soma process + buffer
              (markdown-soma--kill)
              (y-or-n-p-with-timeout "Waiting for process die...?" 1 t)
              ;; Tell Chrome to kill a tab from the top window.
              (do-applescript "
tell application \"Google Chrome\"
  tell window 1 tab 1 to close
end tell
")
              ))
          markdown-soma-highlightjs-theme-list)))

     '(("dark" "~/workspace/soma/styles/lopped-off-dark.css")
       ("light" "~/workspace/soma/styles/lopped-off.css")))))

;; (screengrab-soma)

(provide 'screengrab-soma)

;;; screengrab-soma.el ends here
