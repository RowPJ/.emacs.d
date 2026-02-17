(require 'use-package)
(use-package hydra :ensure t)
(use-package ivy-hydra :ensure t)


;; Hydra Behavior Control Options:
;;     :exit - When set to t, all heads will exit the hydra by default unless overridden
;;     :foreign-keys - Controls what happens when a key not defined in any head is pressed:
;;         nil (default) - Exit hydra and execute the foreign key command
;;         warn - Show a warning message and don't exit hydra
;;         run - Execute the foreign key command without exiting hydra
;;     :color - A shortcut that sets both :exit and :foreign-keys:
;;         red - :exit nil :foreign-keys nil
;;         blue - :exit t :foreign-keys nil
;;         amaranth - :exit nil :foreign-keys warn
;;         teal - :exit t :foreign-keys warn
;;         pink - :exit nil :foreign-keys run


;; placeholder root hydra entry point
(defhydra hydra (:exit t :color amaranth)
  "hydra"
  ("i" hydra-ivy/body "ivy")
  ("a" hydra-avy/body "avy")
  ("d" dap-hydra "dap")
  ("w" hydra-windows/body "windows")
  ("l" hydra-layout/body "layout")
  ("q" nil "quit"))

(defhydra hydra-windows (:exit t :color amaranth)
  "windows"
  ;; move a window to a new frame
  ;; in the same emacs process
  ("t" tear-off-window "tear-off")
  ("s" window-swap-states "swap")
  ("r" resize-window "resize")
  ("q" nil "quit"))

(defhydra hydra-avy (:exit t :color amaranth)
  "avy"
  ("a" avy-goto-char "char")
  ("s" avy-goto-char-2 "char2")
  ("C-a" avy-goto-line "line-start")
  ("C-e" avy-goto-end-of-line "line-end")
  ("C-k" avy-kill-whole-line "kill-line")
  ("M-w" avy-kill-ring-save-region "copy-region")
  ("C-w" avy-kill-region "kill-region")
  ("q" nil "quit"))

;; layout controlling mode
(defhydra hydra-layout (:color amaranth)
  "layout"

  ;; no-column variants of below commands
  ;; kill other tabs by number
  ("@" (tab-close-other 2))
  ("#" (tab-close-other 3))
  ("$" (tab-close-other 4))
  ("%" (tab-close-other 5))
  ("^" (tab-close-other 6))
  ("&" (tab-close-other 7))
  ("*" (tab-close-other 8))
  ("(" (tab-close-other 9))
  ;; switch to a tab by number
  ("2" (tab-select 2))
  ("3" (tab-select 3))
  ("4" (tab-select 4))
  ("5" (tab-select 5))
  ("6" (tab-select 6))
  ("7" (tab-select 7))
  ("8" (tab-select 8))
  ("9" (tab-select 9))
  ;; resize window a small amount
  ("M-B" (shrink-window-horizontally 1) "")
  ("M-P" (shrink-window 1) "")
  ("M-N" (shrink-window -1) "")
  ("M-F" (shrink-window-horizontally -1) "")

  ;;;; movement commands
  ;; move between windows
  ("b" windmove-left "move-left" :column "Move")
  ("n" windmove-down "move-down")
  ("p" windmove-up "move-up")
  ("f" windmove-right "move-right")
  ;; switch tabs, relative
  ("TAB" tab-next "next-tab")
  ("<backtab>" (tab-next -1) "prev-tab")
  ("1" (tab-select 1) "select-tab-1")


  ;;;; swap commands
  ;; move/swap windows
  ("B" windmove-swap-states-left "swap-left" :column "Swap")
  ("N" windmove-swap-states-down "swap-down")
  ("P" windmove-swap-states-up "swap-up")
  ("F" windmove-swap-states-right "swap-right")
  ;; move tabs
  ("C-<tab>" (tab-move 1) "tab-right")
  ("C-S-<tab>" (tab-move -1) "tab-left")
  ("C-S-<iso-lefttab>" (tab-move -1) "tab-left")
  ;; change buffer
  ("g" previous-buffer "prev-buffer")
  ("h" next-buffer "next-buffer")


  ;;;; creation commands
  ;; open new tab
  ("C-t" tab-new "tab" :column "Create")
  ;; split window
  ("V" (progn (split-window-below) (other-window 1)) "split-below")
  ("v" (progn (split-window-right) (other-window 1)) "split-right")
  ("M-V" (progn (split-root-window-below) (other-window 1)) "split-root-below")
  ("M-v" (progn (split-root-window-right) (other-window 1)) "split-root-right")
  ;; move a window to a new tab
  ;; or frame and switch to it
  ("t" tab-window-detach "tear-to-tab")
  ("T" tear-off-window "tear-to-frame")

  
  ;; resize window
  ("M-b" (shrink-window-horizontally 10) "shrink-_" :column "Resize")
  ("M-p" (shrink-window 10) "shrink-|")
  ("M-n" (shrink-window -10) "grow-|")
  ("M-f" (shrink-window-horizontally -10) "grow-_")
  ("=" (balance-windows) "balance") ;should maybe set to + since default binding is "C-x +"


  ;;;; close commands
  ;; close window
  ("0" delete-window "delete-window" :column "Close")
  ;; close and open tabs (like in browsers)
  ("C-w" tab-close "close-tab")
  ;; switch tabs and close all others, absolute
  ("!" (tab-close-other 1) "just-tab-1")
  ;; close buffer
  ("W" (kill-buffer (current-buffer)) "kill-buffer")
  ("C-S-t" tab-bar-mode "toggle-tab-bar")

  ("r" recursive-edit "recursive-edit" :column "Other")
  ("w" writeroom-mode "writeroom")
  ("=" writeroom-adjust-width "writeroom-reset-width")
  ("-" writeroom-decrease-width "writeroom-decrease-width")
  ("+" writeroom-increase-width "writeroom-increase-width")
  ;; exit without doing anything
  ("q" nil "quit")
  ("RET" nil "quit")
  ("C-c" nil "quit"))

;; ispell / spelling check / spelling correction hydra
(defhydra hydra-ispell (:color blue)
  ("r" ispell-region "ispell region")
  ("b" ispell-buffer "ispell buffer")
  ("w" ispell-word "ispell word")
  ("q" nil "Quit"))

;; numeric window selection for some commands
(defhydra hydra-ace-window (:color teal :quit t)
  ("1" ace-delete-other-windows "delete-other-windows")
  ("0" ace-delete-window "delete-window")
  ("o" ace-select-window "select-window")
  ("s" ace-swap-window "swap-window")
  ("q" nil "Quit"))

(defhydra hydra-org-roam (:color teal :quit t)
  ("q" nil "quit")
  ("f" consult-org-roam-file-find "find note" :column "navigation")
  ("b" consult-org-roam-backlinks "search backlinks" :column "navigation")
  ("o" org-open-at-point-global "follow org link")
  ("l" org-roam-node-insert "insert note link")
  ("L" org-store-link "store org link")
  ("M-l" org-insert-link "insert org link")
  ("a" org-agenda "org agenda" :column "agenda / dailies")
  ("t" org-roam-dailies-find-today "today's note")
  ("T" org-roam-dailies-find-date "find a daily note")
  ("n" org-roam-dailies-find-tomorrow "tomorrow's note")
  ("p" org-roam-dailies-find-yesterday "yesterday's note")
  ("N" org-roam-dailies-find-next-note "next daily note")
  ("P" org-roam-dailies-find-previous-note "previous daily note")
  ("d" org-sidebar-toggle "org sidebar toggle" :column "misc")
  ("s" org-roam-db-sync "sync roam db")
  ("v" org-roam-ui-mode "view notes graph"))

(provide 'hydra-config)
