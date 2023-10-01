;; ensure packages are installed
(require 'use-package)
(use-package "avy")
(use-package "company")
(use-package "company-auctex")
(use-package "company-math")
(use-package "hydra")
(use-package "ivy")
(use-package "ivy-hydra")
(use-package "latex-extra")
(use-package "latex-preview-pane")
(use-package "lsp-latex")
(use-package "lsp-mode")
(use-package "magit")
(use-package "pdf-tools")
(use-package "resize-window")
(use-package "slime")
(use-package "slime-company")
(use-package "solarized-theme")
(use-package "ssh")
(use-package "swiper")
(use-package "yasnippet")


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(solarized-dark-high-contrast))
 '(custom-safe-themes
   '("51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "524fa911b70d6b94d71585c9f0c5966fe85fb3a9ddd635362bfabd1a7981a307" "efcecf09905ff85a7c80025551c657299a4d18c5fcfedd3b2f2b6287e4edd659" "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" default))
 '(package-selected-packages
   '(ivy-hydra idomenu hydra avy swiper ssh company-auctex resize-window lsp-latex company-math latex-preview-pane latex-extra pdf-tools slime-company company ein solarized-theme magit slime))
 '(pdf-view-incompatible-modes
   '(linum-mode linum-relative-mode helm-linum-relative-mode nlinum-mode nlinum-hl-mode nlinum-relative-mode yalinum-mode display-line-numbers-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(setq inferior-lisp-program "/opt/homebrew/bin/sbcl")

(add-hook 'after-init-hook 'global-company-mode)

(slime-setup '(slime-fancy slime-company))

(pdf-tools-install)

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

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(require 'company-auctex)
(company-auctex-init)

;; auctex variable configuration
(setq-default TeX-master nil) ;makes auctex query for the main project .tex file of each .tex file opened


;; placeholder root hydra entry point
(defhydra hydra (:color blue :exit t)
  "hydra"
  ("i" hydra-ivy/body "ivy")
  ("a" hydra-avy/body "avy")
  ("w" hydra-windows/body "windows")
  ("q" nil "quit"))

(defhydra hydra-windows (:color red :exit t)
  "windows"
  ("s" window-swap-states "swap")
  ("r" resize-window "resize"))

(defhydra hydra-avy (:color white :exit t)
  "avy"
  ("a" avy-goto-char "char")
  ("s" avy-goto-char-2 "char2")
  ("C-a" avy-goto-line "line-start")
  ("C-e" avy-goto-end-of-line "line-end")
  ("M-w" avy-kill-ring-save-region "copy-region")
  ("C-w" avy-kill-region "kill-region")
  ("q" nil "quit"))


;; custom keybinds
(global-set-key (kbd "C-c h") 'hydra/body)
(global-set-key (kbd "C-c s") 'swiper)
(global-set-key (kbd "C-c c") 'avy-goto-char)

(autoload 'idomenu "idomenu" nil t)
