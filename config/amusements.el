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

(global-set-key (kbd "C-M-s") 'randomise-case-region)

(provide 'amusements)
