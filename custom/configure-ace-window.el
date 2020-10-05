;; -*- lexical-binding: t -*-
(require 'ace-window)

(setq aw-background t
      aw-ignore-current nil
      aw-char-position 'top-left)

;;* Keep track of selected window for modeline.
;;  https://emacs.stackexchange.com/questions/26222
(defvar ml-selected-window nil)

(defun ml-record-selected-window ()
  (setq ml-selected-window (selected-window)))

(add-hook 'post-command-hook 'ml-record-selected-window)

;;* select-minibuffer-window
(defun select-minibuffer-window ()
  "Select minibuffer window if active."
  (interactive)
  (when-let ((minibuffer (active-minibuffer-window)))
    (select-window minibuffer)))

;;* aw-dispatch-alist
(setq aw-dispatch-alist
      '((?x aw-delete-window "Delete Window")
        (?S aw-swap-window "Swap Windows")
        (?m aw-move-window "Move Window")
        (?c aw-copy-window "Copy Window")
        (?j aw-switch-buffer-in-window "Select Buffer")
        (?n aw-flip-window)
        (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
        (?e aw-execute-command-other-window "Execute Command Other Window")
        (?F aw-split-window-fair "Split Fair Window")
        (?v aw-split-window-vert "Split Vert Window")
        (?b aw-split-window-horz "Split Horz Window")
        (?o delete-other-windows "Delete Other Windows")
        (?T aw-transpose-frame "Transpose Frame")
        (32 select-minibuffer-window)  ; SPC
        (?\n select-minibuffer-window)
        (?\r select-minibuffer-window)
        (?+ balance-windows-horizontally)
        (?= balance-windows-area)
        (?? aw-show-dispatch-help)))

;;* show window path in modeline
(defun ace-window-path-lighter ()
  "Ace path in modeline for windows other than selected
if there are more than 2 of them."
  (if (or (eq (selected-window) ml-selected-window)
          (<= (length (aw-window-list)) 2))
      " "
      (concat " " (window-parameter (selected-window) 'ace-window-path) " ")))

(ace-window-display-mode 1)

;;* ace-move-window
(defun ace-move-window ()
  "Ace move window."
  (interactive)
  (aw-select " Ace - Move Window" #'aw-move-window))

(define-key ctl-x-map (kbd "C-m") 'ace-move-window)

(provide 'configure-ace-window)
