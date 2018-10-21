;;; -*- lexical-binding: t -*-
;; must be set before loading evil?
(setq evil-search-wrap t
      evil-want-Y-yank-to-eol t
      evil-regexp-search t
      evil-disable-insert-state-bindings t
      evil-move-beyond-eol t
      lispy-avy-style-paren 'pre)

(require 'evil)
(require 'expand-region)
(require 'ivy)

(evil-mode t)
(evil-set-initial-state 'slime-popup-buffer-mode 'emacs)
(add-to-list 'evil-emacs-state-modes 'slime-popup-buffer-mode)

;; lispyville
(require 'lispyville)

(add-hook 'lisp-mode-hook #'lispyville-mode)
(add-hook 'emacs-lisp-mode-hook #'lispyville-mode)

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
;;     (additional-movement normal visual motion)
     )))

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

(defvar swiper-ex-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [remap ivy-done] 'ivy-immediate-done)
    ;; TODO: completion
    ;; (define-key map (kbd "TAB") 'evil-ex-completion)
    map))

(defun swiper-evil-ex (command refresh-swiper)
  "Call evil ex command on lines matched by swiper."
  (interactive (list nil current-prefix-arg))
  (with-ivy-window
    (let* ((swiper-last ivy-last)
           ;; (completion-at-point-functions '(evil-ex-command-completion-at-point
           ;;                                  evil-ex-argument-completion-at-point))
           (command (ivy-read "EX: "
                              ivy--old-cands
                              :keymap swiper-ex-keymap
                              :require-match nil
                              :re-builder (lambda (x) nil)
                              :history 'evil-ex-history
                              :caller this-command))
           (evil-ex-current-buffer (current-buffer))
           (evil-operator-range-motion 'mark-whole-buffer)
           evil-operator-range-type evil-operator-range-beginning
           evil-operator-range-end evil-inhibit-operator
           (command-form (evil-ex-parse command))
           (ivy-recursive-last swiper-last))
      (dolist (candidate ivy--old-cands)
        (evil-operator-range)
        (let ((ln (read (or (get-text-property 0 'swiper-line-number candidate)
                            (and (string-match ":\\([0-9]+\\):.*\\'" candidate)
                                 (match-string-no-properties 1 candidate))))))
          (goto-line ln)
          (eval command-form)))
      (when refresh-swiper
        (setf (ivy-state-collection ivy-recursive-last) (swiper--candidates)))
      (ivy-recursive-restore))))

(define-key evil-normal-state-map "v" 'evil-visual-char-or-expand-region)
(define-key evil-visual-state-map "v" 'evil-visual-char-or-expand-region)
(define-key evil-visual-state-map (kbd "M-v") 'er/contract-region)
(define-key evil-visual-state-map [escape] 'evil-visual-char)

(dolist (map (list evil-normal-state-map evil-motion-state-map evil-insert-state-map))
  (define-key map (kbd "C-t") (and (fboundp 'lispy-mode) 'lispy-ace-paren)))
(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-normal-state-map (kbd "M-,") nil)
(define-key evil-normal-state-map (kbd "C-.") nil)
(define-key evil-normal-state-map (kbd "C-,") nil)
(define-key evil-normal-state-map (kbd "DEL") 'lispyville-beginning-of-defun)
(define-key evil-motion-state-map (kbd "DEL") 'lispyville-beginning-of-defun)
(define-key evil-motion-state-map (kbd "RET") 'end-of-defun-spammable)
(define-key evil-normal-state-map (kbd "RET") 'end-of-defun-spammable)
(define-key evil-motion-state-map (kbd "/") 'avy-goto-char-timer)
(define-key evil-motion-state-map (kbd "C-e") nil)
(define-key evil-window-map (kbd "C-w") 'ace-window)
(define-key evil-window-map (kbd "w") 'ace-window)
(define-key evil-window-map (kbd "C-q") 'evil-window-delete)
(define-key evil-window-map (kbd "q") 'evil-window-delete)

(define-key swiper-map (kbd "M-;") 'swiper-evil-ex)


(provide 'configure-evil)
