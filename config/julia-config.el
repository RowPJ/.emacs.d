(require 'use-package)
(use-package lsp-mode :ensure t)
(use-package lsp-julia :ensure t)

(add-hook 'julia-mode-hook #'lsp-mode)
(add-hook 'julia-mode-hook #'lsp)

(provide 'julia-config)
