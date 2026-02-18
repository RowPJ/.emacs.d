;; -*- lexical-binding: nil; -*-
;; (coded by gpt)
(defun randomise-case-region ()
  (interactive)
  (setq case-fold-search nil)

  (unless (region-active-p)
    (error "No region selected"))

  (let ((start (region-beginning))
        (end (region-end)))

    (save-excursion
      (goto-char start)

      (while (< (point) end)
        (let ((char (char-after)))
          (if (and (or (char-after 1) (char-after 1))
                   (zerop (random 2)))
              (progn
                (delete-char 1)
                (insert (upcase (char-to-string char))))
            (progn
              (delete-char 1)
              (insert (downcase (char-to-string char)))))
          )
        (forward-char)))))


;; (coded by gpt) this can be useful for arranging text in a specific way
(defvar replace-region-last-replacement " "
  "Remember the last replacement character used in replace-region.")
(defun replace-region (start end)
  "Replace the region from START to END with a user-specified character, defaulting
to the last used character or space if it's the first time, maintaining text positioning."
  (interactive "r")
  (let ((replacement-char
	 (read-string (format "Enter replacement character (default: %s): "
			      replace-region-last-replacement)
		      nil nil replace-region-last-replacement)))
    (setq replace-region-last-replacement replacement-char)
    (save-excursion
      (let ((line-start start)
	    line-end)
	(goto-char start)
	(while (< (point) end)
	  (setq line-end (line-end-position))
	  (when (> line-end start)
	    (let* ((line-text (buffer-substring line-start (min line-end end)))
		   (replacement (make-string (length line-text)
					     (string-to-char replacement-char))))
	      (delete-region line-start (min line-end end))
	      (insert replacement)))
	  (forward-line 1)
	  (setq line-start (point)))))))


(defhydra hydra-amusements (:color teal :quit t)
  ("r" replace-region "replace region")
  ("c" randomise-case-region "randomise case region")
  ("q" nil "Quit"))
(global-set-key (kbd "C-c A") 'hydra-amusements/body)


(provide 'amusements)
