(use-package lsp-mode :ensure t)
(use-package cmake-mode :ensure t)

(require 'dap-cpptools)

;; requirements:
;; 1. install bear to generate compilation data for clang tooling (use
;; system package manager to install)
;; 2. install clangd language server (lsp-mode will prompt you to
;; confirm automatic installation when opening a cpp file for the
;; first time
;; 3. Build your project with bear to generate compile_commands.json
;; for the clangd lsp server (example: bear -- g++ -I include
;; src/eigensolver.cpp src/tests/test_eigensolver.cpp -llapack -o
;; eigensolver)

(add-hook 'c-mode-hook 'lsp-mode)
(add-hook 'c++-mode-hook 'lsp-mode)


(with-eval-after-load "cc-mode"
  (define-key c-mode-map (kbd "M-r") 'lsp-find-references)
  (define-key c++-mode-map (kbd "M-r") 'lsp-find-references))


(provide 'c-cpp-config)
