;; requirements:
;; Need to install pyflakes, black, and isort executables on your
;; machine.
;; On linux, you can do "sudo apt install pyflakes3 python3-pyflakes
;; python3-flake8 black isort"

(require 'use-package)
(use-package anaconda-mode :ensure t)
(use-package company-anaconda :ensure t)
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

(use-package pyvenv-auto
  :ensure t
  :hook ((python-mode . pyvenv-auto-run)))

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


;; start flycheck with python mode. this will default to using mypy
;; and flake8 checkers; we also disable pylint
(setq-default flycheck-disabled-checkers (cons 'python-pylint flycheck-disabled-checkers))
(add-hook 'python-mode-hook 'flycheck-mode)

(add-hook 'python-mode-hook 'dap-mode)

;; run python black on save (reformats code according to standards)
(require 'python-black)
(add-hook 'python-mode-hook 'python-black-on-save-mode)

;; run isort on save. we add this after the black hook so that it runs
;; first.
(require 'python-isort)
(add-hook 'python-mode-hook 'python-isort-on-save-mode)

;; indent / de-indent hydra
(defhydra python-indent-shift ()
  ("<" python-indent-shift-left "indent-shift-left")
  (">" python-indent-shift-right "indent-shift-right"))
(define-key flycheck-mode-map (kbd "C-c <tab>") (lambda ()
                                                  (interactive)
                                                  (python-indent-shift/body)))

(provide 'python-config)
