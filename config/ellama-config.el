;; This file configures Ellama for GPT API integration.

;; INSTRUCTIONS TO SETUP ELLAMA WITH YOUR OPENAI API KEY:
;; 1. Create the file ~/config/openai-key.el
;;    The path and name is important.
;; 2. Inside the file, put the below 2 lines of code. Make sure to
;;    replace the string contents with your actual openai api key.
;;  (setq openai-key "YOUR-KEY-HERE")
;;  (provide 'openai-key)
;; 3. Restart emacs to reload this file. Alternatively, just running
;;    this buffer as elisp code should be sufficient.

(require 'use-package)

;; only run this config if we can load the openai key
(when (file-exists-p "~/.emacs.d/config/openai-key.el")
  (require 'openai-key))
(if (boundp 'openai-key)
    (progn
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
  (global-set-key (kbd "C-c j") (lambda ()
				  (interactive)
				  (error "You need to set an openai key. Check ~/.emacs.d/config/ellama-config.el for instructions."))))


(provide 'ellama-config)
