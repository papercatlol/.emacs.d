;; must be set before loading evil?
(setq evil-search-wrap t
      evil-want-Y-yank-to-eol t
      evil-regexp-search t
      evil-disable-insert-state-bindings t
      evil-move-beyond-eol t
      lispy-avy-style-paren 'pre)

(require 'evil)
(require 'expand-region)

(evil-mode t)
(evil-set-initial-state 'slime-popup-buffer-mode 'emacs)
(add-to-list 'evil-emacs-state-modes 'slime-popup-buffer-mode)

;; lispyville
(require 'lispyville)

(add-hook 'lisp-mode-hook #'lispyville-mode)

(with-eval-after-load 'lispyville
  (lispyville-set-key-theme
   '(operators
     prettify
     text-objects
;; a    lispyville-inner-atom
;; l	lispyville-inner-list
;; x	lispyville-inner-sexp
;; f	lispyville-inner-function
;; c	lispyville-inner-comment
;; S	lispyville-inner-string
     (atom-movement t)
     slurp/barf-cp
     additional-insert
     (escape insert)
     (additional-movement normal visual motion))))

(defun end-of-defun-spammable ()
  (interactive)
  (forward-char)
  (call-interactively #'end-of-defun)
  (backward-char))

(defun evil-visual-char-or-expand-region ()
  (interactive)
  (if (region-active-p)
        (call-interactively 'er/expand-region)
    (evil-visual-char)))

(define-key evil-normal-state-map "v" 'evil-visual-char-or-expand-region)
(define-key evil-visual-state-map "v" 'evil-visual-char-or-expand-region)
(define-key evil-visual-state-map (kbd "M-v") 'er/contract-region)
(define-key evil-visual-state-map [escape] 'evil-visual-char)

(dolist (map (list evil-normal-state-map evil-motion-state-map evil-insert-state-map))
  (define-key map (kbd "C-t") (and (fboundp 'lispy-mode) 'lispy-ace-paren)))
(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-normal-state-map (kbd "M-,") nil)
(define-key evil-motion-state-map (kbd "DEL") 'lispyville-beginning-of-defun)
(define-key evil-motion-state-map (kbd "RET") 'end-of-defun-spammable)
(define-key evil-normal-state-map (kbd "RET") 'end-of-defun-spammable)
(define-key evil-motion-state-map (kbd "/") 'avy-goto-char-timer)
(define-key evil-motion-state-map (kbd "C-e") nil)
(define-key evil-window-map (kbd "C-w") 'ace-window)
(define-key evil-window-map (kbd "w") 'ace-window)
(define-key evil-window-map (kbd "C-q") 'evil-window-delete)
(define-key evil-window-map (kbd "q") 'evil-window-delete)


(provide 'configure-evil)
