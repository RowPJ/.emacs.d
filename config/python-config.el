;; -*- lexical-binding: nil; -*-
;; requirements:
;; Need to install pyflakes, black, and isort executables on your
;; machine. Probably some others too for language servers or other
;; features. These should preferably be installed inside the virtual
;; environment of the project you are working on but a system wide
;; install is also possible.

(require 'use-package)

;;;;;;;;;;;;;;;
;; DAP SETUP ;;
;;;;;;;;;;;;;;;
(require 'dap-python)
;; NOTE: need to 'pip install debugpy' in the environment that you are
;; debugging in for this to work.
(setq dap-python-debugger 'debugpy)
(dap-auto-configure-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TREESIT-PYTHON-MODE CONFIG ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'major-mode-remap-alist
             '(python-mode . python-ts-mode))
;; If the language source is not defined in your emacs version, you
;; can uncomment the below two forms to try using the newest commit of
;; the python tree-sitter grammar. Uncommenting the first form allows
;; locating the grammar, and uncommenting the second asks the user to
;; install it (if commented, the user will be asked anyway the first
;; time opening a python file / activating python-ts-mode)
;; (setq treesit-language-source-alist
;;       '((python "https://github.com/tree-sitter/tree-sitter-python")))
;; (when (and (fboundp 'treesit-install-language-grammar)
;;            (treesit-available-p)
;;            (not (treesit-language-available-p 'python)))
;;   (treesit-install-language-grammar 'python))

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
  :hook ((python-ts-mode . pyvenv-auto-run)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYRIGHT LANGUAGE SERVER ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp-deferred)))
  :config (flycheck-add-next-checker 'lsp 'python-pyright))

;; add remote python lsp client for tramp
;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
;;                   :major-modes '(python-ts-mode)
;;                   :remote? t
;;                   :server-id 'pyls-remote))

;; use 79 as fill column in python buffers
(setq-mode-local python-ts-mode fill-column 79)

(use-package python-black :ensure t
  :config
  ;; (add-hook 'python-ts-mode-hook 'python-black-on-save-mode)
  )
(use-package python-isort :ensure t
  :config
  ;; (add-hook 'python-ts-mode-hook 'python-isort-on-save-mode)
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
(add-hook 'python-ts-mode-hook 'flycheck-mode)
;; (flycheck-add-next-checker 'lsp 'python-flake8)
(flycheck-add-next-checker 'lsp 'python-ruff)
;; (flycheck-add-next-checker 'lsp 'python-pycompile)
;; (flycheck-add-next-checker 'lsp 'python-pyright)
(flycheck-add-next-checker 'lsp 'python-mypy)
(flycheck-add-next-checker 'lsp 'python-pylint)

(provide 'python-config)
