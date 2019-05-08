;; -*- lexical-binding: t -*-
(require 'ace-window)


;; Keep track of selected window for modeline.
;; https://emacs.stackexchange.com/questions/26222
(defvar ml-selected-window nil)

(defun ml-record-selected-window ()
  (setq ml-selected-window (selected-window)))

(add-hook 'post-command-hook 'ml-record-selected-window)

;; Disable ace-window overlay since we use modeline instead.
(advice-add #'aw--lead-overlay :override #'ignore)

(defun switch-to-minibuffer-window ()
  "Switch to minibuffer window if active."
  (interactive)
  (when-let ((minibuffer (active-minibuffer-window)))
    ;; (select-frame-set-input-focus (window-frame minibuffer))
    (select-window minibuffer)))

(defun aw-dispatch-with-minibuffer-switch (char)
  (cond ((or (= char ?m)
             (= char ? )
             (= char (aref (kbd "RET") 0)))
         (switch-to-minibuffer-window)
         (throw 'done 'exit))
        (t (funcall #'aw-dispatch-default char))))

(setq aw-dispatch-function #'aw-dispatch-with-minibuffer-switch
      aw-background nil
      aw-ignore-current nil
      aw-char-position 'left)

(defun ace-window-path-lighter ()
  "Ace path in modeline for windows other than selected
if there are more than 2 of them."
  (if (or (eq (selected-window) ml-selected-window)
          (<= (length (aw-window-list)) 2))
      " "
      (concat " " (window-parameter (selected-window) 'ace-window-path) " ")))

(ace-window-display-mode 1)


(provide 'configure-ace-window)
