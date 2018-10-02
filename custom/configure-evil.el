;; must be set before loading evil?
(setq evil-search-wrap t
      evil-want-Y-yank-to-eol t
      evil-regexp-search t
      evil-disable-insert-state-bindings t
      evil-move-beyond-eol t)

(require 'evil)
(require 'expand-region)

(evil-mode t)
(evil-set-initial-state 'slime-popup-buffer-mode 'emacs)

(defun evil-visual-char-or-expand-region ()
  (interactive)
  (if (region-active-p)
        (call-interactively 'er/expand-region)
    (evil-visual-char)))

(define-key evil-normal-state-map "v" 'evil-visual-char-or-expand-region)
(define-key evil-visual-state-map "v" 'evil-visual-char-or-expand-region)
(define-key evil-visual-state-map (kbd "M-v") 'er/contract-region)
(define-key evil-visual-state-map [escape] 'evil-visual-char)

(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-normal-state-map (kbd "M-,") nil)
(define-key evil-normal-state-map (kbd "C-.") nil)
(define-key evil-normal-state-map (kbd "C-,") nil)
(define-key evil-normal-state-map (kbd "DEL") 'beginning-of-defun)
(define-key evil-normal-state-map (kbd "RET") 'end-of-defun)
(define-key evil-normal-state-map (kbd "RET") 'end-of-defun)
(define-key evil-motion-state-map (kbd "C-t") 'avy-goto-char-timer)
(define-key evil-motion-state-map (kbd "/") 'avy-goto-char-timer)
(define-key evil-window-map (kbd "C-w") 'ace-window)
(define-key evil-window-map (kbd "w") 'ace-window)
(define-key evil-window-map (kbd "C-q") 'evil-window-delete)
(define-key evil-window-map (kbd "q") 'evil-window-delete)


(provide 'configure-evil)
