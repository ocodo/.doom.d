(when (equal system-type 'darwin)
 (use-package! swift-mode
   :hook (swift-mode . (lambda () (lsp)))))
