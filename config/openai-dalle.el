;; -*- lexical-binding: nil; -*-
(require 'url)
(require 'json)

(require 'openai-key)

;; Declaration of variables
(defvar openai-dalle-buffer-prefix "*DALL·E-"
  "Prefix for the buffers displaying DALL·E images.")

;; Define the minor mode
(define-minor-mode openai-dalle-mode
  "A minor mode for interacting with the OpenAI DALL·E API."
  :lighter " DALL·E"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c d i") 'openai-create-image-prompt)
            ;; Additional keybindings for other functions can be added here
            map))

;; Define the function to create an image
(defun openai-create-image (prompt &optional model n size quality style callback)
  "Create an image given a prompt using OpenAI's API."
  (let ((url-request-method "POST")
        (url-request-extra-headers `(("Content-Type" . "application/json")
                                     ("Authorization" . ,(concat "Bearer " openai-key))))
        (url-request-data (json-encode `(("prompt" . ,prompt)
                                         ("model" . ,(or model "dall-e-2"))
                                         ("n" . ,(or n 1))
                                         ("size" . ,(or size "1024x1024"))
                                         ("quality" . ,(or quality "standard"))
                                         ("style" . ,(or style "vivid"))))))
    (url-retrieve "https://api.openai.com/v1/images/generations"
                  (or callback 'openai-handle-create-image-response))))

;; Define the callback function
(defun openai-handle-create-image-response (status)
  "Handle the HTTP response from the create image operation."
  (if-let ((error (plist-get status ':error)))
      (progn
        (kill-buffer) ; Clean up the temporary buffer created by `url-retrieve`.
        (message "Error creating image: %S" error))
    (goto-char (point-min))
    (when (search-forward "\n\n" nil t)
      (let* ((json-string (buffer-substring (point) (point-max)))
             (json-data (json-read-from-string json-string))
             (images-vector (assoc-default 'data json-data))
             (images (and images-vector (append images-vector nil))))
        (kill-buffer) ; Clean up the temporary buffer used to receive the response.
        (if images
            (dolist (image-data images)
              (let ((image-url (assoc-default 'url image-data)))
                (when image-url
                  (openai-download-image image-url))))
          (message "No images found in API response."))))))

;; Define the function to download an image
(defun openai-download-image (image-url)
  "Download an image from the URL and save it to a temporary file."
  (let* ((url-content (with-current-buffer (url-retrieve-synchronously image-url t t)
                        (goto-char (point-min))
                        (search-forward "\n\n") ; Skip header
                        (buffer-substring-no-properties (point) (point-max))))
         (temp-file (make-temp-file "dalle-" nil ".png")))
    (with-temp-file temp-file
      (insert url-content))
    (openai-display-image-in-buffer temp-file)))

;; Define the function to display an image from file
(defun openai-display-image-in-buffer (file-path)
  "Display an image from FILE-PATH in its own buffer."
  (let ((buffer-name (concat openai-dalle-buffer-prefix (file-name-nondirectory file-path)))
        (buffer (generate-new-buffer openai-dalle-buffer-prefix)))
    (set-buffer buffer)
    (insert-image (create-image file-path))
    (set-buffer-modified-p nil)
    (switch-to-buffer buffer)))

;; Define the prompt function
(defun openai-create-image-prompt ()
  "Prompt the user for parameters and create an image."
  (interactive)
  (let* ((prompt (read-string "Enter DALL·E Prompt: "))
         (model (completing-read "Model: " '("dall-e-2" "dall-e-3") nil t nil 'openai-history "dall-e-2"))
         (n (read-number "Number of images: " 1))
         (size (completing-read "Size: " '("1024x1024" "1792x1024" "1024x1792" "256x256" "512x512") nil t nil 'openai-history "1024x1024"))
         (quality (if (equal model "dall-e-3") (completing-read "Quality: " '("standard" "hd") nil t nil 'openai-history "standard") "standard"))
         (style (if (equal model "dall-e-3") (completing-read "Style: " '("vivid" "natural") nil t nil 'openai-history "vivid") "vivid")))
    (openai-create-image prompt model n size quality style)))

(defhydra hydra-dalle (:exit t)
  ("c" openai-create-image-prompt "create from prompt")
  ("q" nil "quit" :exit t))
(global-set-key (kbd "C-c k") 'hydra-dalle/body)

(provide 'openai-dalle)
