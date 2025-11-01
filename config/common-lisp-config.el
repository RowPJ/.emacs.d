(require 'use-package)
(use-package slime :ensure t)
(use-package slime-company :ensure t)

;; TODO: on gnu/linux, maybe check the path exists then use /opt/bin/sbcl if not?
(setq inferior-lisp-program
      (cl-case system-type
	(gnu/linux "/usr/bin/sbcl")
	(darwin "/opt/homebrew/bin/sbcl")
	(windows-nt "sbcl --dynamic-space-size 8GB")))

(slime-setup '(slime-fancy slime-company))

(provide 'common-lisp-config)
