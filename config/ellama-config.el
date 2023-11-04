;; This file configures Ellama for GPT API integration.

;; INSTRUCTIONS TO SETUP ELLAMA WITH YOUR OPENAI API KEY:
;; 1. Create the file ~/.emacs.d/config/openai-key.el
;;    The path and name is important.
;; 2. Inside the file, put the below 2 lines of code. Make sure to
;;    replace the string contents with your actual openai api key.
;;  (setq openai-key "YOUR-KEY-HERE")
;;  (provide 'openai-key)
;; 3. Restart emacs to reload this file. Alternatively, just running
;;    this buffer as elisp code should be sufficient.

(require 'use-package)

;; only run this config if we can load the openai key
(setq openai-default-chat-model "gpt-3.5-turbo")

(defun ellama-choose-chat-model ()
  "Sets ellama-provider to use a new openai chat model."
  (interactive)
  (let ((model (cadr (read-multiple-choice "Select openai chat model: " ;prompt
					   '((?3 "gpt-3.5-turbo")
					     (?4 "gpt-4")))))) ; choices
    (setq ellama-provider
	  (make-llm-openai :key openai-key
			   :chat-model model))))

(defun ellama-help-with-edit-op ()
  "Asks the user for an emacs task that they want to perform, and gets
the ellama provider to tell the user how to accomplish this with standard emacs key bindings. If the functionality cannot be accomplished easily, it instead returns the definition of an elisp function that implements the requested task."
  (interactive)
  (let* ((task (read-string "Describe the emacs task that you wish to perform: "))
	 (prompt (concat "Very briefly describe how the following task can be accomplished easily in Emacs with keyboard commands. If there is no convenient sequence of existing standard keys to perform the task, then instead define an elisp function to perform the task. Be brief by not explaining the code. You may also define auxiliary functions if necessary. The defined function should be interactive.:\n\n" task)))
    (ellama-instant prompt)))

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
			       :chat-model openai-default-chat-model)))
      ;; define ellama hydra
      (use-package hydra :ensure t)
      (defhydra hydra-ellama (:exit t)
	("a" ellama-ask-about "ask-about")
	("C" ellama-change "change-text")
	("E" ellama-help-with-edit-op "help-with-edit-op")
	("M-c" ellama-change "change-code")
	("c" ellama-chat "chat")
	("r" ellama-code-review "code-review")
	("b" ellama-make-concise "make-concise")
	("t" ellama-translate "to-english")
	("s" ellama-summarize "summarize")
	("M" ellama-choose-chat-model "choose model"))
      (global-set-key (kbd "C-c j") 'hydra-ellama/body))
  (global-set-key (kbd "C-c j") (lambda ()
				  (interactive)
				  (error "You need to set an openai key. Check ~/.emacs.d/config/ellama-config.el for instructions."))))


(provide 'ellama-config)
