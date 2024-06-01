(when (display-graphic-p)
 (defun ocodo/font-favorites ()
   "Set default and variable-pitch fonts from favorites."
   (interactive)
   (let* ((default-mono-fonts '(("PFDin Mono XThin"             (:family "PFDinMono-XThin" :weight normal))
                                ("PFDin Mono Thin"              (:family "PFDinMono-Thin" :weight normal))
                                ("PFDin Mono Light"             (:family "PFDinMono-Light" :weight normal))
                                ("SauceCodePro Nerd ExtraLight" (:family "SauceCodePro Nerd Font" :weight light))
                                ("SauceCodePro Nerd Light"      (:family "SauceCodePro Nerd Font" :weight semi-light))
                                ("SauceCodePro Nerd Regular"    (:family "SauceCodePro Nerd Font" :weight normal))
                                ("OcodoMono"                    (:family "OcodoMono" :weight thin))
                                ("OcodoMono Nerd Font"          (:family "OcodoMono Nerd Font" :weight thin))
                                ("Input Mono Regular"           (:family "Input Mono" :weight normal))
                                ("Input Mono Light"             (:family "Input Mono" :weight semi-light))
                                ("APL385 Unicode"               (:family "APL385 Unicode" :weight normal))
                                ("IBM Plex Mono Thin"           (:family "IBM Plex Mono" :weight ultra-light))
                                ("IBM Plex Mono ExtraLight"     (:family "IBM Plex Mono" :weight light))
                                ("IBM Plex Mono Light"          (:family "IBM Plex Mono" :weight semi-light))
                                ("IBM Plex Mono Regular"        (:family "IBM Plex Mono" :weight normal))))

          (variable-pitch-fonts '(("Helvetica"                    (:family "Helvetica" :weight normal))
                                  ("Helvetica Neue Regular"       (:family "Helvetica Neue" :weight normal))
                                  ("Helvetica Neue Light"         (:family "Helvetica Neue" :weight light))
                                  ("Helvetica Neue Ultralight"    (:family "Helvetica Neue" :weight thin))
                                  ("Helvetica Neue Thin"          (:family "Helvetica Neue" :weight ultra-light))
                                  ("Bodini 72"                    (:family "Bodini 72" :weight normal))
                                  ("American Typewriter Light"    (:family "American Typewriter" :weight light))
                                  ("American Typewriter Regular"  (:family "American Typewriter" :weight normal))
                                  ("Futura"                       (:family "Futura" :weight normal))
                                  ("Gill Sans Regular"            (:family "Gill Sans" :weight normal))
                                  ("Gill Sans Light"              (:family "Gill Sans" :weight light))
                                  ("Avenir Next Ultralight"       (:family "Avenir Next" :weight thin))
                                  ("Avenir Next Regular"          (:family "Avenir Next" :weight normal))))

          (modeline-fonts (append default-mono-fonts variable-pitch-fonts))

          (selected-fonts (--map (assoc
                                   (completing-read (format
                                                     "Select %s Font [nil to skip]:"
                                                     (plist-get it :name))

                                                    (mapcar
                                                     (lambda (that) that)
                                                     (plist-get it :fonts)))
                                   (plist-get it :fonts))

                              (list (list :name "Default Monospaced" :fonts default-mono-fonts)
                                    (list :name "Default Variable Pitch" :fonts variable-pitch-fonts)
                                    (list :name "Modeline" :fonts modeline-fonts))))
          (selected-doom-mono-font (cadr (nth 0 selected-fonts)))
          (selected-doom-variable-font (cadr (nth 1 selected-fonts)))
          (selected-doom-modeline-font (cadr (nth 2 selected-fonts))))

        (when selected-doom-mono-font
          (setq doom-font (apply 'font-spec selected-doom-mono-font)))
        (when selected-doom-variable-font
          (setq doom-variable-pitch-font (apply 'font-spec selected-doom-variable-font)))
        (when selected-doom-modeline-font
          (let* ((spec selected-doom-modeline-font)
                 (font (plist-get spec :family))
                 (weight (plist-get spec :weight)))
               (set-face-attribute 'mode-line nil :family font :weight weight)
               (set-face-attribute 'mode-line-inactive nil :family font :weight weight)))
    (doom/reload-font))))
