(require 'use-package)

;; (setenv "GOPATH" (concat (getenv "HOME") "/go"))
;; (setenv "GOROOT" "/usr/local/go")
;; (setenv "GOBIN" (concat (getenv "GOPATH") "/bin"))
;; (setenv "PATH" (concat (getenv "PATH") ":" (getenv "GOPATH")))
;; (setenv "PATH" (concat (getenv "PATH") ":" (getenv "GOROOT") "/bin"))
;; (setenv "PATH" (concat (getenv "PATH") ":" (getenv "GOPATH") "/bin"))

(use-package lsp-mode :ensure t)
(use-package go-mode :ensure t)

(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'go-mode-hook 'subword-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook (lambda ()
                          (setq tab-width 4)
                          (flycheck-add-next-checker 'lsp 'go-vet)
                          (flycheck-add-next-checker 'lsp 'go-staticcheck)))

(provide 'go-config)
