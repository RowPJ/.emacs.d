(require 'use-package)
(use-package anaconda-mode :ensure t)
(use-package company-anaconda :ensure t)
(use-package flycheck-pyflakes :ensure t)
(use-package python-black :ensure t)
(use-package python-isort :ensure t)

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

;; add remote python lsp client for tramp
;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
;;                   :major-modes '(python-mode)
;;                   :remote? t
;;                   :server-id 'pyls-remote))

;; configure python development environment
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'anaconda-mode 'anaconda-eldoc-mode)
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

(require 'flycheck-pyflakes)
(add-hook 'python-mode-hook 'flycheck-mode)

(add-hook 'python-mode-hook 'dap-mode)

;; run python black on save (reformats code according to standards)
(require 'python-black)
(add-hook 'python-mode-hook 'python-black-on-save-mode)

;; run isort on save. we add this after the black hook so that it runs
;; first.
(require 'python-isort)
(add-hook 'python-mode-hook 'python-isort-on-save-mode)

(provide 'python-config)
