;; -*- lexical-binding: nil; -*-
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; refresh the package list if there isn't already a cache
;; existing. this should make new emacs installs able to download
;; melpa packages at first run. package-refresh-contents can be
;; called manually if use-package packages fail to install due
;; to not being found in the package archive.
(unless package-archive-contents
  (package-refresh-contents))

(provide 'package-config)
