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
					     (?4 "gpt-4o")))))) ; choices
    (setq ellama-provider
	  (make-llm-openai :key openai-key
			   :chat-model model))))

;; TODO: in order to fix this, I need to make execution wait for the
;; full response to be streamed before returning the buffer string.
;; (defun ellama-query (prompt)
;;   "Queries ellama and gets a reponse string to return immediately."
;;   (with-temp-buffer 
;;     (ellama-stream prompt)
;;     (buffer-string)))

(defun ellama-help-with-emacs-task ()
  "Asks the user for an emacs task that they want to perform, and gets
the ellama provider to tell the user how to accomplish this with standard emacs key bindings. If the functionality cannot be accomplished easily, it instead returns the definition of an elisp function that implements the requested task."
  (interactive)
  (let* ((task (read-string "Describe the emacs task that you wish to perform: "))
	 (prompt (format "Provide guidance for completing the following task in Emacs, ideally using standard keybindings or features. If the task requires complex actions, suggest a new Emacs Lisp function definition or a reference to an existing Emacs package and how to use it. Task description: '%s'" task)))
    (ellama-chat prompt)))

(defun ellama-expand-on-region ()
  "Expands on the selected text. This typically becomes an explanation
of the text."
  (interactive)
  
  (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
	 (prompt (concat "Expand on the following text:\n\n" text)))
    (ellama-chat prompt)))

(defun ellama-debug ()
  (interactive)
  (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
	 (prompt (concat "Give me suggestions to help with debugging this code:\n\n") text))
    (ellama-chat prompt)))

;; TODO: in order to fix this, I need to fix ellama-query.
;; (defun ellama-command ()
;;   "Runs an emacs command from a natural language prompt."
;;   (interactive)
;;   (let* ((nl-command (read-string "Enter a natural language command: "))
;; 	 (prompt (concat "Generate some Elisp code to perform the following task in Emacs. Only output the code without any explanations or formatting, as your response will be passed directly to the Elisp intepreter without any further processing.\nTask: " nl-command))
;; 	 (response (ellama-query prompt))
;; 	 ;; to get the code, delete the first and last lines of formatting
;; 	 (code (let* ((lines (split-string response "\n" t))
;; 		      (lines-without-first (cdr lines)) ; Remove the first line
;; 		      (lines-without-first-and-last (butlast lines-without-first)))
;; 		 (mapconcat 'identity lines-without-first-and-last "\n"))))
;;     (if (y-or-n-p (concat "Execute reponse code: \"" code "\"?"))
;; 	(eval (read-from-string code)))))

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
	("3" (progn
	       (setq ellama-provider
		     (make-llm-openai :key openai-key
				      :chat-model "gpt-3.5-turbo"))
	       (message "switched to gpt-3.5-turbo"))
	 "gpt-3.5-turbo"
	 :exit nil)
	("4" (progn
	       (setq ellama-provider
		     (make-llm-openai :key openai-key
				      :chat-model "gpt-4o"))
	       (message "switched to gpt-4o"))
	 "gpt-4o"
	 :exit nil)
	("a" ellama-ask-about "ask-about")
	("C" ellama-complete-code "complete-code")
	("E" ellama-help-with-emacs-task "help-with-emacs-task")
	("e" ellama-expand-on-region "expand-on-region")
	("M-c" ellama-change-code "change-code")
	("c" ellama-chat "chat")
	("d" ellama-define-word "define-word")
	("D" ellama-debug "debug")
	("r" ellama-code-review "code-review")
	("b" ellama-make-concise "make-concise")
	("t" ellama-translate "translate")
	("w" ellama-summarize-webpage "summarize-webpage")
	("s" ellama-summarize "summarize")
	("M" ellama-choose-chat-model "choose model")
	("q" nil "quit" :exit t))
      (global-set-key (kbd "C-c j") 'hydra-ellama/body))
  (global-set-key (kbd "C-c j") (lambda ()
				  (interactive)
				  (error "You need to set an openai key. Check ~/.emacs.d/config/ellama-config.el for instructions."))))


(provide 'ellama-config)
