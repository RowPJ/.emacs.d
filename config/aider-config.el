;; -*- lexical-binding: nil; -*-
(require 'use-package)

(require 'openai-key)

(use-package aidermacs
  :ensure t
  :bind (("C-c p" . aidermacs-transient-menu))
  :init
  (setenv "OPENAI_API_KEY" openai-key)
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-backend 'vterm)
  (aidermacs-architect-model "openai/o3-mini")
  (aidermacs-editor-model "openai/gpt-4o-mini")
  (aidermacs-default-model "openai/gpt-4o-mini"))
(provide 'aider-config)
