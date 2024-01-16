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
			   (local-set-key (kbd "C-c M-n") 'org-move-subtree-down)
			   (local-set-key (kbd "C-c M-p") 'org-move-subtree-up)))

(provide 'org-config)
