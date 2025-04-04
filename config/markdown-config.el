(require 'use-package)

(use-package grip-mode
  :ensure t
  :bind (:map markdown-mode-command-map ("g" . grip-mode)))

(provide 'markdown-config)
