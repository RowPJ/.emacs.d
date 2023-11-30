;; enable executing python code blocks in org mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;; display line numbers in org-mode. we should maybe turn this off
;; because it could be confusing when text is folded.
(add-hook 'org-mode-hook 'display-line-numbers-mode)

(provide 'org-config)
