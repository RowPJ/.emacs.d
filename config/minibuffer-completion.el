(require 'use-package)

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '(((file (styles partial-completion))))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))


(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-S-s" . consult-line-multi)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x B" . consult-bookmark)
         ("C-c s" . consult-ripgrep)
         ("C-c t" . consult-theme)
         ("C-c m" . consult-mark)
         ("C-c M" . consult-global-mark)
         ("C-c C-k" . consult-kmacro)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)))

(use-package embark
  :ensure t
  :bind
  ("C-." . embark-act)
  ("C-;" . embark-dwim))

(use-package embark-consult :ensure t)

(use-package consult-flycheck
  :ensure t
  :init
  (define-key flycheck-mode-map (kbd "C-c f") #'consult-flycheck))

(use-package consult-org-roam :ensure t)

(provide 'minibuffer-completion)
