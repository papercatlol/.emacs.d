;; -*- lexical-binding: t -*-
(require 'ace-window)

(setq aw-background nil
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

;;* focus new windows after splitting
(defun split-window-right* ()
  "Like `split-window-right', but focus the new window
after splitting."
  (interactive)
  (call-interactively #'split-window-right)
  (other-window 1))

(defun split-window-below* ()
  "Like `split-window-below', but focus the new window
after splitting."
  (interactive)
  (call-interactively #'split-window-below)
  (other-window 1))

(global-set-key [remap split-window-right] 'split-window-right*)
(global-set-key [remap split-window-below] 'split-window-below*)

(defun aw-split-window-below* (window)
  "Split WINDOW below and select the new window."
  (select-window window)
  (split-window-below*))

(defun aw-split-window-right* (window)
  "Split WINDOW right and select the new window."
  (select-window window)
  (split-window-right*))

;;* aw-dispatch-alist
(defun kbd-char (key)
  (elt (kbd key) 0))

;; TODO and bind to M-c M-a
;; Likewise for `select-bottom-side-window' M-c M-z or M-v or smth
;; (defun select-left-side-window ()
;;   (select-window ))

(setq aw-dispatch-alist
      `((?x aw-delete-window "Delete Window")
        (,(kbd-char "M-x") aw-delete-window "Delete Window")
        (,(kbd-char "M-d") aw-delete-window "Delete Window")
        (?S aw-swap-window "Swap Windows")
        (,(kbd-char "M-s") aw-swap-window "Swap Windows")
        (?m aw-move-window "Move Window")
        (,(kbd-char "M-m") aw-move-window "Move Window")
        (?c aw-copy-window "Copy Window")
        (?r aw-replace-window "Replace Window")
        (,(kbd-char "M-r") aw-replace-window "Replace Window")
        (?n aw-flip-window)
        (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
        (?e aw-execute-command-other-window "Execute Command Other Window")
        (?F aw-split-window-fair "Split Fair Window")
        (?o delete-other-windows "Delete Other Windows")
        (?T aw-transpose-frame "Transpose Frame")
        (32 select-minibuffer-window)   ; SPC
        (?\n select-minibuffer-window)
        (?\r select-minibuffer-window)
        (?+ balance-windows-horizontally)
        (?= balance-windows-area)
        (?? aw-show-dispatch-help)
        (,(kbd-char "M-c") aw-delete-window "Delete Window")
        (,(kbd-char "<tab>") other-window)
        (,(kbd-char "M-<tab>") other-window)
        (,(kbd-char "TAB") other-window)
        (,(kbd-char "M-TAB") other-window)
        (?2 aw-split-window-below* "Split vertically")
        (?b aw-split-window-below* "Split vertically")
        (?3 aw-split-window-right* "Split horizontally")
        (?| aw-split-window-right* "Split horizontally")
        ;; counsel-buffers
        (?j aw-counsel-buffers "Select Buffer")
        (?v aw-counsel-buffers "Select buffer")
        (,(kbd-char "M-v") aw-counsel-buffers "Select buffer")
        (,(kbd-char "C-v") aw-counsel-buffers "Select buffer")
        ))

;;* make `aw-dispatch-help' handle non-char key combinations
(defun aw-show-dispatch-help--override ()
  "Display action shortucts in echo area."
  (interactive)
  (message "%s" (mapconcat
                 (lambda (action)
                   (cl-destructuring-bind (key fn &optional description) action
                     (format "%s: %s"
                             (propertize
                              (key-description (vector key))
                              'face 'aw-key-face)
                             (or description fn))))
                 aw-dispatch-alist
                 "\n"))
  ;; Prevent this from replacing any help display
  ;; in the minibuffer.
  (let (aw-minibuffer-flag)
    (mapc #'delete-overlay aw-overlays-back)
    (call-interactively 'ace-window)))
(advice-add 'aw-show-dispatch-help :override #'aw-show-dispatch-help--override)

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

;;* ace-delete-window
;; Since there is C-x C-j for dired anyway
(define-key ctl-x-map (kbd "C-d") 'ace-delete-window)

;; HACK since dired sets its C-x C-d binding at autoload (no idea if this works)
(with-eval-after-load 'dired
  (define-key ctl-x-map (kbd "C-d") 'ace-delete-window))

;;* ace-replace-window
(defun aw-replace-window (window)
  "Move the current buffer to WINDOW.
Delete the current window."
  (let ((buffer (current-buffer)))
    (delete-window)
    (aw-switch-to-window window)
    (switch-to-buffer buffer)))

(defun ace-replace-window ()
  "Replace target window with selected."
  (aw-select " Ace - Replace Window" #'aw-replace-window))

;;* aw-counsel-buffers
(defun aw-counsel-buffers (window)
  "Selece buffer in WINDOW."
  (aw-switch-to-window window)
  (counsel-buffers))


(provide 'configure-ace-window)
