(require 'use-package)

(cl-case system-type
  ;; On linux / macos, use julia-snail since we can install
  ;; dependencies easily. On ubuntu 20.04, the dependency names are
  ;; "libvterm-bin" and "libtool-bin" and they can be installed with apt.
  ;; On manjaro, can install "libvterm" and "libtool" using pamac.
  ((gnu/linux darwin)
   (use-package julia-snail :ensure t)
   (add-hook 'julia-mode-hook 'julia-snail-mode))
  ;; on windows / unknown, fall back to a combination of other modes
  (t
   (use-package lsp-mode :ensure t)
   (use-package lsp-julia :ensure t)
   (use-package julia-repl :ensure t)
   (add-hook 'julia-mode-hook 'lsp-mode)
   (add-hook 'julia-mode-hook 'lsp)
   (add-hook 'julia-mode-hook 'julia-repl-mode)))

(provide 'julia-config)
