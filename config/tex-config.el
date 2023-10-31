;; avoid unset variable warning popup at startup
(setq latex-extra-mode nil)

(require 'use-package)
(use-package company-auctex :ensure t)
(use-package latex-extra :ensure t)
(use-package latex-preview-pane :ensure t)
(use-package lsp-latex :ensure t)

(add-hook 'LaTeX-mode-hook #'latex-extra-mode)

(latex-preview-pane-enable)

(require 'lsp)
(require 'lsp-latex)
;; "texlab" executable must be located at a directory contained in `exec-path'.
;; If you want to put "texlab" somewhere else,
;; you can specify the path to "texlab" as follows:
;; (setq lsp-latex-texlab-executable "/path/to/texlab")
(with-eval-after-load "tex-mode"
  (add-hook 'latex-extra-mode-hook 'lsp)
  (add-hook 'latex-extra-mode-hook 'display-line-numbers-mode)
  (add-hook 'tex-mode-hook 'lsp)
  (add-hook 'latex-mode-hook 'lsp))

;; For bibtex
(with-eval-after-load "bibtex"
  (add-hook 'bibtex-mode-hook 'lsp))

(require 'company-auctex)
(company-auctex-init)

;; auctex variable configuration
(setq-default TeX-master nil) ;makes auctex query for the main project .tex file of each .tex file opened

(provide 'tex-config)
