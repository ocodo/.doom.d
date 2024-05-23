(when (equal system-type 'darwin)
 (use-package! lsp-sourcekit
   :after lsp-mode
   :config
   (setq lsp-sourcekit-executable
         "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")))
