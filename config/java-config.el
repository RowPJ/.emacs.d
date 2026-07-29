(require 'use-package)

(require 'lsp)

(use-package lsp-java
  :ensure t
  :config (add-hook 'java-mode-hook #'lsp-mode)
  ;; ensure jdtls language server is installed
  (lsp-ensure-server 'jdtls))

(provide 'java-config)
