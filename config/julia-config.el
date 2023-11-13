(require 'use-package)
(use-package lsp-mode :ensure t)
(use-package lsp-julia :ensure t)
(use-package julia-repl :ensure t)

(add-hook 'julia-mode-hook 'lsp-mode)
(add-hook 'julia-mode-hook 'lsp)
(add-hook 'julia-mode-hook 'julia-repl-mode)

(provide 'julia-config)
