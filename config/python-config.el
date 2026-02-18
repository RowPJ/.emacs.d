;; -*- lexical-binding: nil; -*-
;; requirements:
;; Need to install pyflakes, black, and isort executables on your
;; machine.
;; On linux, you can do "sudo apt install pyflakes3 python3-pyflakes
;; python3-flake8 black isort"

(require 'use-package)

;;;;;;;;;;;;;;;
;; DAP SETUP ;;
;;;;;;;;;;;;;;;
(require 'dap-python)
;; NOTE: need to 'pip install debugpy' in the environment that you are
;; debugging in for this to work.
(setq dap-python-debugger 'debugpy)
(add-hook 'python-mode-hook 'dap-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VIRTUAL ENVIRONMENT MANAGEMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYRIGHT LANGUAGE SERVER ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
  :config (flycheck-add-next-checker 'lsp 'python-pyright))

;; add remote python lsp client for tramp
;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
;;                   :major-modes '(python-mode)
;;                   :remote? t
;;                   :server-id 'pyls-remote))

;; configure python development environment
;; (add-hook 'python-mode-hook 'anaconda-mode)
;; (add-hook 'anaconda-mode 'anaconda-eldoc-mode)

;; use 79 as fill column in python buffers
(setq-mode-local python-mode fill-column 79)

(use-package python-black :ensure t
  :config
  ;; (add-hook 'python-mode-hook 'python-black-on-save-mode)
  )
(use-package python-isort :ensure t
  :config
  ;; (add-hook 'python-mode-hook 'python-isort-on-save-mode)
  )

;; indent / de-indent hydra
(defhydra python-indent-shift ()
  ("<" python-indent-shift-left "indent-shift-left")
  (">" python-indent-shift-right "indent-shift-right"))
(define-key flycheck-mode-map (kbd "C-c <tab>") (lambda ()
                                                  (interactive)
                                                  (python-indent-shift/body)))


;;;;;;;;;;;;;;;;;;;;
;; FLYCHECK SETUP ;;
;;;;;;;;;;;;;;;;;;;;
;; start flycheck with python mode. this will default to using mypy
;; and flake8 checkers; we also disable pylint
;; (setq-default flycheck-disabled-checkers (cons 'python-pylint flycheck-disabled-checkers))
(add-hook 'python-mode-hook 'flycheck-mode)
(flycheck-add-next-checker 'lsp 'python-flake8)
(flycheck-add-next-checker 'lsp 'python-ruff)
(flycheck-add-next-checker 'lsp 'python-pycompile)
(flycheck-add-next-checker 'lsp 'python-pyright)
(flycheck-add-next-checker 'lsp 'python-mypy)
(flycheck-add-next-checker 'lsp 'python-pylint)

(provide 'python-config)
