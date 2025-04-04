(require 'use-package)
(use-package org-sidebar :ensure t)

;; enable executing python code blocks in org mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
;   (sh t)
;   (bash t)
   (R t)
   (awk t)
;   (zsh t)
   (shell t)
   (scheme t)
   (org t)
   (lua t)
   (makefile t)
   (matlab t)
   (maxima t)
   (latex t)
   (julia t)
   (haskell t)
   (forth t)
   (fortran t)
   (gnuplot t)
   (eshell t)
   (emacs-lisp t)
;   (elisp t)
;   (csh t)
   (calc t)
   (clojure t)

;   (C++ t)
;   (cpp t)
   (C t)))

;; display line numbers in org-mode. we should maybe turn this off
;; because it could be confusing when text is folded.
(add-hook 'org-mode-hook 'display-line-numbers-mode)

;; bind keys for swapping subtree ordering
(add-hook 'org-mode-hook (lambda ()
			   (local-set-key (kbd "C-M-n") 'org-move-subtree-down)
			   (local-set-key (kbd "C-M-p") 'org-move-subtree-up)
                           (local-set-key (kbd "C-M-S-n") 'org-move-item-down)
			   (local-set-key (kbd "C-M-S-p") 'org-move-item-up)))

;; bind keys for navigating items in org mode (like list
;; entries)
(add-hook 'org-mode-hook (lambda ()
			   (local-set-key (kbd "C-c M-n") 'org-next-item)
			   (local-set-key (kbd "C-c M-p") 'org-previous-item)))

;; Add CANCELED as an additional terminal TODO state. Headings marked
;; with CANCELED are also treated as completed.
(setq-default org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELED")))
;; Set colours for TODO keywords.
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "red" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("CANCELED" . (:foreground "gray" :weight bold :strike-through t))))

(defun my/org-todo-face-canceled (limit)
  "Apply a strikethrough and gray color to the entire heading of CANCELED items."
  (when (re-search-forward "^\\*+ \\(CANCELED\\) \\(.*\\)" limit t)
    (add-text-properties (match-beginning 0) (match-end 0)
                         '(face (:foreground "gray" :strike-through t)))))
(font-lock-add-keywords 'org-mode '((my/org-todo-face-canceled 0)))


;; make sure markdown export is loaded
(require 'ox-md)
;; load github-flavoured markdown export (for things like table
;; support)
(require 'ox-gfm)

;; TODO: implement a cross-platform SSH link type. The below doesn't work for various reasons.
;; (org-link-set-parameters
;;  "ssh"
;;  :follow (lambda (path)
;;            (start-process-shell-command "terminal-ssh" nil
;;                                         (format "gnome-terminal -- bash -c 'ssh %s; exec bash'" path))))

(provide 'org-config)
