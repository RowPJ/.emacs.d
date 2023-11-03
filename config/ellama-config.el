;; This file configures Ellama for LLM integration.

(require 'use-package)

;; only run this config if we can load the openai key
(when (file-exists-p "~/.emacs.d/config/openai-key.el")
  (require 'openai-key))
(when (boundp 'openai-key)
  (setq llm-warn-on-nonfree nil)
  (use-package ellama
    :ensure t
    :init
    (require 'llm-openai)
    (setq ellama-provider
	  (make-llm-openai :key openai-key
			   :chat-model "gpt-3.5-turbo")))
  ;; define ellama hydra
  (use-package hydra :ensure t)
  (defhydra hydra-ellama (:exit t)
    ("a" ellama-ask-about "ask-about")
    ("C" ellama-change "change-text")
    ("M-c" ellama-change "change-code")
    ("c" ellama-chat "chat")
    ("r" ellama-code-review "code-review")
    ("b" ellama-make-concise "make-concise")
    ("t" ellama-translate "to-english")
    ("s" ellama-summarize "summarize"))
  (global-set-key (kbd "C-c j") 'hydra-ellama/body))

(provide 'ellama-config)
