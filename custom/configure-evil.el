;; must be set before loading evil?
(setq evil-search-wrap t
      evil-want-Y-yank-to-eol t
      evil-regexp-search t
      evil-disable-insert-state-bindings t
      evil-move-beyond-eol t)

(require 'evil)

(evil-mode t)
(evil-set-initial-state 'slime-popup-buffer-mode 'emacs)

(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-normal-state-map (kbd "M-,") nil)
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
