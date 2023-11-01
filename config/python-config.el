(require 'use-package)
(use-package anaconda-mode :ensure t)
(use-package company-anaconda :ensure t)

(use-package lsp-pyright :ensure t)

(require 'dap-python)
(setq dap-python-debugger 'debugpy)

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python")))))

(use-package flymake-python-pyflakes :ensure t)

(require 'lsp-pyright)
;; add remote python lsp client for tramp
;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
;;                   :major-modes '(python-mode)
;;                   :remote? t
;;                   :server-id 'pyls-remote))

;; configure python development environment
;; (add-hook 'python-mode-hook 'anaconda-mode)
;; (add-hook 'anaconda-mode 'anaconda-eldoc-mode)
;; (eval-after-load "company"
;;   '(add-to-list 'company-backends 'company-anaconda))

;; configure python flymake integration (requires installing pyflakes
;; executable)
(setq flymake-python-pyflakes-executable (cl-case system-type
					   (gnu/linux "pyflakes3")
					   (darwin "pyflakes")
					   (windows-nt "pyflakes")))
(add-hook 'python-mode-hook 'flymake-mode)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(add-hook 'python-mode-hook 'dap-mode)
(add-hook 'python-mode-hook 'lsp)

(provide 'python-config)
