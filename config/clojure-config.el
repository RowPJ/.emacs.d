(require 'use-package)
(require 'flycheck)

(use-package cider :ensure t)
(use-package cider-decompile :ensure t)
(use-package cider-eval-sexp-fu :ensure t)
(use-package cider-hydra :ensure t)
(use-package clojure-essential-ref :ensure t)
(use-package flycheck-clojure :ensure t)

(eval-after-load 'flycheck '(flycheck-clojure-setup))

(provide 'clojure-config)
