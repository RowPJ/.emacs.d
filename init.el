;; load files from custom config directory
(add-to-list 'load-path "~/.emacs.d/config")

;; install use-package manually on older emacs versions
(require 'package-config)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; configure melpa package source
(eval-when-compile
  (require 'use-package))


;; ensure packages are installed
(require 'use-package)
(use-package exec-path-from-shell :ensure t
  :config (when (memq window-system '(mac ns))
            (setq exec-path-from-shell-variables '("JAVA_HOME" "GOPATH" "GOROOT" "GOBIN"))
            (exec-path-from-shell-initialize)))
(use-package avy :ensure t)
(use-package ace-window :ensure t)
(use-package csv-mode :ensure t)
(use-package company :ensure t)
(use-package company-math :ensure t)
(use-package dap-mode :ensure t)
(use-package define-word :ensure t)
(use-package direnv :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package eyebrowse :ensure t)
(use-package flymake-aspell :ensure t)
(use-package flycheck :ensure t)
(use-package inkpot-theme :ensure t)
(use-package ivy :ensure t)
(use-package lsp-mode
  :commands lsp
  :ensure t
  :init (setq lsp-keymap-prefix "C-c C-l")
  :config (define-key lsp-mode-map (kbd "C-c C-l") lsp-command-map))
;; enable lsp as a backend for flycheck
(require 'lsp-diagnostics)
(lsp-diagnostics-flycheck-enable)
(use-package magit :ensure t)
(use-package magit-lfs :ensure t :pin melpa)
(when (eql system-type 'gnu/linux)
  (use-package magit-todos :ensure t)
  (use-package pdf-tools :ensure t))
(use-package orgit :ensure t)
(use-package multiple-cursors :ensure t)
(use-package modus-themes :ensure t)
(defvar user-roam-directory (file-name-as-directory"~/Sync/org-roam/") "Directory to store org-roam files in.")
(when (not (mkdir user-roam-directory t))
  (message (concat "Created org-roam directory " user-roam-directory ".")))
(use-package org-roam
  :ensure t
  :custom (org-roam-directory user-roam-directory)
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:40}" 'face 'org-tag)))
  :init (org-roam-db-autosync-enable))
(use-package org-roam-ui :ensure t)
(use-package orglink :ensure t)
(use-package ox-gfm :ensure t)
(use-package read-aloud :ensure t)	;need to install flite to use this
(use-package resize-window :ensure t)
(use-package reveal-in-folder :ensure t)
(use-package rg :ensure t)
(use-package sicp :ensure t)
(use-package solarized-theme :ensure t)
(use-package ssh :ensure t)
(use-package swiper :ensure t)
(use-package treemacs :ensure t)
(use-package treemacs-tab-bar :ensure t)
(use-package treemacs-magit :ensure t)
(use-package vscode-dark-plus-theme :ensure t)
(use-package which-key :ensure t)
(use-package writeroom-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package yasnippet :ensure t)

;; load other config files
(require 'aider-config)
(require 'common-lisp-config)
(require 'clojure-config)
(require 'tex-config)
(require 'python-config)
(require 'julia-config)
(require 'c-cpp-config)
(require 'hydra-config)
(require 'ellama-config)
(require 'org-config)
(require 'amusements)
(require 'openai-dalle)
(require 'godot-config)
(require 'go-config)
(require 'markdown-config)
(require 'rust-config)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(inkpot))
 '(custom-safe-themes
   '("e410458d3e769c33e0865971deb6e8422457fad02bf51f7862fa180ccc42c032" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "2ccdb4796d3238dd0794f7869750fb0e81fe4f9212f9528cfd4f41da0c78cf25" "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428" "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37" "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "b5fab52f16546a15f171e6bd450ff11f2a9e20e5ac7ec10fa38a14bb0c67b9ab" "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc" "3cdd0a96236a9db4e903c01cb45c0c111eb1492313a65790adb894f9f1a33b2d" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "65ef77d1038e36cb9dd3f514d86713f8242cb1352f5ebf0d2390c7e5bf1fd4d1" "993aac313027a1d6e70d45b98e121492c1b00a0daa5a8629788ed7d523fe62c1" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "524fa911b70d6b94d71585c9f0c5966fe85fb3a9ddd635362bfabd1a7981a307" "efcecf09905ff85a7c80025551c657299a4d18c5fcfedd3b2f2b6287e4edd659" "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" default))
 '(inhibit-startup-screen t)
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(orglink-activate-in-modes '(fundamental-mode text-mode prog-mode))
 '(package-selected-packages
   '(modus-themes go-mode pyvenv-auto python-black python-isort flycheck flymake-aspell dockerfile-mode latex-math-preview rg csv-mode treemacs-magit read-aloud julia-repl magit-lfs eyebrowse projectile ellama dap-mode vscode-dark-plus-theme fantom-theme doom-themes kaolin-themes inkpot-theme magit-todos sicp company-anaconda anaconda-mode pyvenv lsp-python-ms reveal-in-folder ivy-hydra idomenu hydra avy swiper ssh company-auctex resize-window lsp-latex company-math latex-preview-pane latex-extra pdf-tools slime-company company ein solarized-theme magit slime))
 '(pdf-view-incompatible-modes
   '(linum-mode linum-relative-mode helm-linum-relative-mode nlinum-mode nlinum-hl-mode nlinum-relative-mode yalinum-mode display-line-numbers-mode))
 '(safe-local-variable-values
   '((eval let
           ((project-root
             (locate-dominating-file default-directory ".dir-locals.el")))
           (setenv "PYTHONPATH"
                   (concat project-root "src:" project-root "src/qclib"))
           (setenv "MYPYPATH"
                   (concat project-root "src:" project-root "src/qclib"))
           (pyvenv-activate
            (concat project-root ".python/venv")))
     (encoding . utf-8)
     (eval progn
           (remove-hook 'python-mode-hook 'python-black-on-save-mode)
           (remove-hook 'python-mode-hook 'python-isort-on-save-mode))
     (eval let
           ((project-root
             (locate-dominating-file default-directory ".dir-locals.el")))
           (pyvenv-activate
            (concat project-root ".python/venv")))
     (eval let
           ((project-root
             (locate-dominating-file default-directory ".dir-locals.el")))
           (setenv "PYTHONPATH"
                   (concat project-root "src:" project-root "src/qclib")))))
 '(set-mark-command-repeat-pop t)
 '(tramp-remote-path
   '(tramp-default-remote-path tramp-own-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; multiple-cursors keybinds
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M->") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-M-<") 'mc/unmark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-<") 'mc/mark-all-in-region)
(global-set-key (kbd "C-S-c C->") 'mc/mark-all-in-region-regexp)

;; replace completing-read-function with ivy
(ivy-mode 1)

;; load local configuration if it exists
(if (file-exists-p "~/.emacs.d/config/machine-local-config.el")
    (require 'machine-local-config))

;; activate completion everywhere
(add-hook 'after-init-hook 'global-company-mode)

;; don't run this init on windows due to errors
(if (eql system-type 'gnu/linux)
    (pdf-tools-install))

;; show line numbers is programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; show column number in mode line
(column-number-mode 1)

;; custom keybinds
(global-set-key (kbd "C-c a") 'avy-goto-char)
(setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;))

(global-set-key (kbd "C-x M-f") 'global-display-fill-column-indicator-mode) ;show vertical indicator of fill column position
(global-set-key (kbd "C-c R") 'read-aloud-this)
(global-set-key (kbd "C-c r") 'recursive-edit)
(global-set-key (kbd "C-c d") 'treemacs)
(global-set-key (kbd "C-c D") 'define-word-at-point)
(global-set-key (kbd "C-c ;") 'dap-hydra)
(global-set-key (kbd "C-M-;") 'comment-box)
(global-set-key (kbd "C-c l") 'hydra-layout/body)
(global-set-key (kbd "C-c C-s") 'hydra-ispell/body)
(global-set-key (kbd "C-c h") 'hydra/body)
(global-set-key (kbd "C-c s") 'swiper)
(global-set-key (kbd "C-c S") 'rg-menu)
(global-set-key (kbd "C-c M-s") 'swiper-thing-at-point)
(global-set-key (kbd "C-c f") 'reveal-in-folder) ;open file in finder / file explorer etc.
;;(global-set-key (kbd "M-\\") 'avy-goto-char) ;this binding is easy to type, could use it for something else later
(progn
  (global-set-key (kbd "M-u") 'upcase-dwim)
  (global-set-key (kbd "M-l") 'downcase-dwim)
  (global-set-key (kbd "M-c") 'capitalize-dwim)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil))
(progn
  (defun open-init-file ()
    "Opens ~/.emacs.d/init.el Emacs config file."
    (interactive)
    (find-file-other-window "~/.emacs.d/init.el"))
  (global-set-key (kbd "C-c i") 'open-init-file))

;; binding for org roam commands
(global-set-key (kbd "C-c o") 'hydra-org-roam/body)

;; add binding for making whitespace visible
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; set keyboard macro keys behaviour
(global-set-key (kbd "C-M-s-q") 'hydra-avy/body)
(global-set-key (kbd "C-M-s-w") 'hydra-windows/body)
(global-set-key (kbd "C-M-s-e") 'hydra-layout/body)

;; don't know what this does
(autoload 'idomenu "idomenu" nil t)

;; enable ido mode for interactive find-file and switch-buffer
(ido-mode 1)

;; on macos, make command and option keys meta key
(setq mac-option-modifier 'meta
      mac-command-modifier 'meta)

;; make remote connections use bash shell by default instead of sh
(setq-default tramp-default-remote-shell "/bin/bash")

;; disable tabs for indentation; always use spaces
(setq-default indent-tabs-mode nil)

;; bind goto commands for the next and previous error in flymake mode
(defhydra flymake-goto-continue ()
  ("M-n" flymake-goto-next-error "flymake-goto-next-error")
  ("M-p" flymake-goto-prev-error "flymake-goto-prev-error"))
(define-key flymake-mode-map (kbd "C-c M-n") (lambda ()
                                               (interactive)
                                               (flymake-goto-next-error)
                                               (flymake-goto-continue/body)))
(define-key flymake-mode-map (kbd "C-c M-p") (lambda ()
                                               (interactive)
                                               (flymake-goto-prev-error)
                                               (flymake-goto-continue/body)))
;; same for flycheck mode
(defhydra flycheck-goto-continue ()
  ("M-n" flycheck-next-error "flycheck-next-error")
  ("M-p" flycheck-previous-error "flycheck-previous-error"))
(define-key flycheck-mode-map (kbd "C-c M-n") (lambda ()
                                                (interactive)
                                                (flycheck-next-error)
                                                (flycheck-goto-continue/body)))
(define-key flycheck-mode-map (kbd "C-c M-p") (lambda ()
                                                (interactive)
                                                (flycheck-previous-error)
                                                (flycheck-goto-continue/body)))

;; enable flymake aspell checker in text buffers
(add-hook 'text-mode-hook #'flymake-aspell-setup)


;; allow using set-goal-column command
(put 'set-goal-column 'disabled nil)

;; set number of commits shown in magit recent commits section
(setq magit-log-section-commit-count 40) ;; default value is 10

;; adjust default text size
(set-face-attribute 'default nil :height (cl-case system-type
					   (gnu/linux 110)
					   (darwin 150)
					   (windows-nt 150)))

;; hide tool bar by default since it's just mouse controls anyway
(tool-bar-mode -1)
;; hide menu bar by default
(menu-bar-mode -1)

;; also hide scroll bar by default
(if (display-graphic-p)
    (scroll-bar-mode -1))

;; enable workspace management
(eyebrowse-mode t)
;; close current workspace
(global-set-key (kbd "C-M-0") 'eyebrowse-close-window-config)
;; workspace switching / creating keybinds
(global-set-key (kbd "C-M-1") 'eyebrowse-switch-to-window-config-1)
(global-set-key (kbd "C-M-2") 'eyebrowse-switch-to-window-config-2)
(global-set-key (kbd "C-M-3") 'eyebrowse-switch-to-window-config-3)
(global-set-key (kbd "C-M-4") 'eyebrowse-switch-to-window-config-4)
(global-set-key (kbd "C-M-5") 'eyebrowse-switch-to-window-config-5)
(global-set-key (kbd "C-M-6") 'eyebrowse-switch-to-window-config-6)
(global-set-key (kbd "C-M-7") 'eyebrowse-switch-to-window-config-7)
(global-set-key (kbd "C-M-8") 'eyebrowse-switch-to-window-config-8)
(global-set-key (kbd "C-M-9") 'eyebrowse-switch-to-window-config-9)
;; switch workspace relative to current workspace
(global-set-key (kbd "M-[") 'eyebrowse-prev-window-config)
(global-set-key (kbd "M-]") 'eyebrowse-next-window-config)
;; make new workspace on first available number
(global-set-key (kbd "C-M-]") 'eyebrowse-create-window-config)

;; TODO: eliminate either the eyebrowse config or the tabs
;; config since they serve a very similar purpose. note that
;; eyebrowse can save configs between sessions so maybe move
;; to it, although it is more work to redo existing configs
;; for tabs.

;; enable vertical lines to indicate indentation level in treemacs
(treemacs-indent-guide-mode 1)
(treemacs-git-commit-diff-mode 1)

;; enable math symbol completion when typing \
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-math-symbols-unicode))
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-math-symbols-latex))


;; set gc cons threshold to 100MB (by lsp mode configuration recommendation)
(setq gc-cons-threshold 100000000)

;; set amount of data emacs reads from processes at a time to 1MB (also lsp mode configuration recommendation)
(setq read-process-output-max (* 1024 1024))

;; customize behaviour for entering ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally
      ediff-control-frame-parameters '((name . "Ediff") (minibuffer . t)))

;; lookup help for symbol at point in cmake buffers
(define-key cmake-mode-map (kbd "M-?") 'cmake-help)

;; enable which-key global minor mode for command hint popups
(which-key-mode 1)

;; make org-mode links display as links in all buffers
(global-orglink-mode 1)

;; on linux, enable direnv for setting environment variables
(when (eql system-type 'gnu/linux)
  (direnv-mode 1))
