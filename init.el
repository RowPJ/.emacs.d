;; load files from custom config directory
(add-to-list 'load-path "~/.emacs.d/config")

;; configure melpa package source
(require 'package-config)

;; ensure packages are installed
(require 'use-package)
(use-package avy :ensure t)
(use-package ace-window :ensure t)
(use-package company :ensure t)
(use-package company-math :ensure t)
(use-package dap-mode :ensure t)
(use-package eyebrowse :ensure t)
(use-package inkpot-theme :ensure t)
(use-package ivy :ensure t)
(use-package lsp-mode :ensure t)
(use-package magit :ensure t)
(use-package magit-lfs :ensure t :pin melpa)
(when (eql system-type 'gnu/linux)
  (use-package magit-todos :ensure t)
  (use-package pdf-tools :ensure t))
(use-package resize-window :ensure t)
(use-package reveal-in-folder :ensure t)
(use-package sicp :ensure t)
(use-package solarized-theme :ensure t)
(use-package ssh :ensure t)
(use-package swiper :ensure t)
(use-package treemacs :ensure t)
(use-package vscode-dark-plus-theme :ensure t)
(use-package yasnippet :ensure t)

;; load other config files
(require 'common-lisp-config)
(require 'tex-config)
(require 'python-config)
(require 'julia-config)
(require 'hydra-config)
(require 'ellama-config)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(inkpot))
 '(custom-safe-themes
   '("aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "2ccdb4796d3238dd0794f7869750fb0e81fe4f9212f9528cfd4f41da0c78cf25" "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428" "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37" "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "b5fab52f16546a15f171e6bd450ff11f2a9e20e5ac7ec10fa38a14bb0c67b9ab" "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc" "3cdd0a96236a9db4e903c01cb45c0c111eb1492313a65790adb894f9f1a33b2d" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "65ef77d1038e36cb9dd3f514d86713f8242cb1352f5ebf0d2390c7e5bf1fd4d1" "993aac313027a1d6e70d45b98e121492c1b00a0daa5a8629788ed7d523fe62c1" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "524fa911b70d6b94d71585c9f0c5966fe85fb3a9ddd635362bfabd1a7981a307" "efcecf09905ff85a7c80025551c657299a4d18c5fcfedd3b2f2b6287e4edd659" "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" default))
 '(inhibit-startup-screen t)
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(package-selected-packages
   '(julia-repl magit-lfs eyebrowse projectile ellama dap-mode flymake-python-pyflakes vscode-dark-plus-theme fantom-theme doom-themes kaolin-themes inkpot-theme magit-todos sicp company-anaconda anaconda-mode pyvenv lsp-python-ms reveal-in-folder ivy-hydra idomenu hydra avy swiper ssh company-auctex resize-window lsp-latex company-math latex-preview-pane latex-extra pdf-tools slime-company company ein solarized-theme magit slime))
 '(pdf-view-incompatible-modes
   '(linum-mode linum-relative-mode helm-linum-relative-mode nlinum-mode nlinum-hl-mode nlinum-relative-mode yalinum-mode display-line-numbers-mode))
 '(set-mark-command-repeat-pop t)
 '(tramp-remote-path
   '(tramp-default-remote-path tramp-own-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; load local configuration if it exists
(if (file-exists-p "~/.emacs.d/config/machine-local-config.el")
    (require 'machine-local-config))

;; activate completion everywhere
(add-hook 'after-init-hook 'global-company-mode)

;; don't run this init on windows due to errors
(if (not (eql system-type 'windows-nt))
    (pdf-tools-install))

;; show line numbers is programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; org-mode is not a programming mode but we still want line numbers.
(add-hook 'org-mode-hook 'display-line-numbers-mode)

;; custom keybinds
(global-set-key (kbd "C-c r") 'recursive-edit)
(global-set-key (kbd "C-c d") 'treemacs)
(global-set-key (kbd "C-c ;") 'dap-hydra)
(global-set-key (kbd "C-M-;") 'comment-box)
(global-set-key (kbd "C-c l") 'hydra-layout/body)
(global-set-key (kbd "C-c h") 'hydra/body)
(global-set-key (kbd "C-c s") 'swiper)
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
    (interactive)
    (find-file-other-window "~/.emacs.d/init.el"))
  (global-set-key (kbd "C-c i") 'open-init-file))

;; set keyboard macro keys behaviour
(global-set-key (kbd "C-M-s-q") 'hydra-avy/body)
(global-set-key (kbd "C-M-s-w") 'hydra-windows/body)
(global-set-key (kbd "C-M-s-e") 'hydra-layout/body)

;; don't know what this does
(autoload 'idomenu "idomenu" nil t)

;; on macos, make command and option keys meta key
(setq mac-option-modifier 'meta
      mac-command-modifier 'meta)

;; make remote connections use bash shell by default instead of sh
(setq-default tramp-default-remote-shell "/bin/bash")

;; TODO: make this functionality with keymaps / whatever the proper way is
(add-hook 'flymake-mode-hook (lambda ()
			       (local-set-key (kbd "C-c M-n") 'flymake-goto-next-error)
			       (local-set-key (kbd "C-c M-p") 'flymake-goto-prev-error)))
(put 'set-goal-column 'disabled nil)

;; set number of commits shown in magit recent commits section
(setq magit-log-section-commit-count 40) ;; default value is 10

;; TODO: add cross-platform aspell / flymake-spell configuration

;; adjust default text size
(set-face-attribute 'default nil :height (cl-case system-type
					   (gnu/linux 110)
					   (darwin 150)
					   (windows-nt 150)))

;; hide tool bar by default since it's just mouse controls anyway
(tool-bar-mode -1)

;; also hide scroll bar by default
(scroll-bar-mode -1)

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
