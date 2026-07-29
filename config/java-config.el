(require 'use-package)

(require 'lsp)

(use-package lsp-java
  :ensure t
  :config (add-hook 'java-mode-hook #'lsp-mode))

(use-package lsp-javacomp
  :ensure t
  :config (add-hook 'java-mode-hook #'lsp-javacomp-enable)
  (lsp-javacomp-install-server))

(provide 'java-config)
