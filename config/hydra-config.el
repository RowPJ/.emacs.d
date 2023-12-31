(require 'use-package)
(use-package hydra :ensure t)
(use-package ivy-hydra :ensure t)

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

  ;; switch and close buffers
  ("g" previous-buffer "prev-buffer")
  ("h" next-buffer "next-buffer")
  ("W" (kill-buffer (current-buffer)) "kill-buffer")

  ;; move between windows
  ("b" windmove-left "move-left")
  ("n" windmove-down "move-down")
  ("p" windmove-up "move-up")
  ("f" windmove-right "move-right")

  ;; resize window
  ("M-B" (shrink-window-horizontally 1) "")
  ("M-b" (shrink-window-horizontally 4) "shrink-_")
  ("M-P" (shrink-window 1) "")
  ("M-p" (shrink-window 4) "shrink-|")
  ("M-N" (shrink-window -1) "")
  ("M-n" (shrink-window -4) "grow-|")
  ("M-F" (shrink-window-horizontally -1) "")
  ("M-f" (shrink-window-horizontally -4) "grow-_")

  ;; recursive edit
  ("r" recursive-edit "recedit")

  ;; move windows
  ("B" windmove-swap-states-left "swap-left")
  ("N" windmove-swap-states-down "swap-down")
  ("P" windmove-swap-states-up "swap-up")
  ("F" windmove-swap-states-right "swap-right")

  ;; split window
  ("V" (progn (split-window-below) (other-window 1)) "split-below")
  ("v" (progn (split-window-right) (other-window 1)) "split-right")

  ;; close window
  ("0" delete-window "delete-window")

  ;; close and open tabs (like in browsers)
  ("C-t" tab-new "create")
  ("C-w" tab-close "close")

  ;; move tabs
  ("C-<tab>" (tab-move 1) "tab-right")
  ("C-S-<tab>" (tab-move -1) "tab-left")
  ("C-S-<iso-lefttab>" (tab-move -1) "tab-left")

  ;; move a window to a new tab
  ;; or frame and switch to it
  ("t" tab-window-detach "tear-to-tab")
  ("T" tear-off-window "tear-to-frame")
  
  ;; switch tabs, relative and absolute
  ("TAB" tab-next "next-tab")
  ("<backtab>" (tab-next -1) "prev-tab")
  ("1" (tab-select 1) "select-1")
  ("2" (tab-select 2))
  ("3" (tab-select 3))
  ("4" (tab-select 4))
  ("5" (tab-select 5))
  ("6" (tab-select 6))
  ("7" (tab-select 7))
  ("8" (tab-select 8))
  ("9" (tab-select 9))
  
  ;; switch tabs and close all others, absolute
  ("!" (tab-close-other 1) "just-1")
  ("@" (tab-close-other 2) "just-2")
  ("#" (tab-close-other 3) "just-3")
  ("$" (tab-close-other 4) "just-4")
  ("%" (tab-close-other 5) "just-5")
  ("^" (tab-close-other 6) "just-6")
  ("&" (tab-close-other 7) "just-7")
  ("*" (tab-close-other 8) "just-8")
  ("(" (tab-close-other 9) "just-9")
  
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
(global-set-key (kbd "C-c C-s") 'hydra-ispell/body)

;; numeric window selection for some commands
(defhydra hydra-ace-window (:color teal :quit t)
  ("1" ace-delete-other-windows "delete-other-windows")
  ("0" ace-delete-window "delete-window")
  ("o" ace-select-window "select-window")
  ("s" ace-swap-window "swap-window")
  ("q" nil "Quit"))
(global-set-key (kbd "C-c a") 'hydra-ace-window/body)


(provide 'hydra-config)
