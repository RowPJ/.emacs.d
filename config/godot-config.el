(require 'use-package)

(use-package gdscript-mode :ensure t)

(add-hook 'gdscript-mode-hook 'lsp)
(when (eql system-type 'windows-nt)
  (setq gdscript-docs-local-path "C:/Godot/godot-docs-html-stable"))

(provide 'godot-config)
