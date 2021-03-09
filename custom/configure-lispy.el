;;* disable C-digits theme
(setq lispy-key-theme '(special lispy))
(lispy-set-key-theme lispy-key-theme)

;;* hooks
(add-hook 'emacs-lisp-mode-hook #'lispy-mode)
(add-hook 'lisp-mode-hook #'lispy-mode)

(defun eval-expression-enable-lispy ()
  (when (or (eq this-command 'eval-expression)
            (eq this-command 'pp-eval-dwim)
            ;; TODO: slime/sldb evals
            )
    (lispy-mode 1)))
(add-hook 'minibuffer-setup-hook #'eval-expression-enable-lispy)

;;* unmap
(define-key lispy-mode-map (kbd "M-j") nil)
(define-key lispy-mode-map (kbd "M-k") nil)
(define-key lispy-mode-map (kbd "C-,") 'er/contract-region)

;;* 'special' bindings - these make more sense to me
;;** w/b
(defun lispy-down-or-mark-car (arg)
  (interactive "p")
  (lispy--remember)
  (if (region-active-p)
      (call-interactively #'lispy-down)
    (lispy-mark-car)))

(defun lispy-up-or-mark-last (arg)
  (interactive "p")
  (lispy--remember)
  (if (region-active-p)
      (call-interactively #'lispy-up)
    ;; too lazy to do this properly
    (lispy-mark-car)
    (lispy-down 99)))

(lispy-define-key lispy-mode-map (kbd "w") 'lispy-down-or-mark-car)
(lispy-define-key lispy-mode-map (kbd "b") 'lispy-up-or-mark-last)
(lispy-define-key lispy-mode-map (kbd ",") 'lispy-back)

;;** wW/sS & H/L
(defun lispy-goto-symbol-in-line ()
  (interactive)
  (when (region-active-p)
    (deactivate-mark))
  (and (avy-goto-symbol-in-line)
       (lispy-mark-symbol)))
(setq lispy-avy-style-symbol 'at-full)

(lispy-define-key lispy-mode-map (kbd "s") 'lispy-goto-symbol-in-line)
(lispy-define-key lispy-mode-map (kbd "S") 'lispy-ace-symbol-replace)
(lispy-define-key lispy-mode-map (kbd "M-s") 'lispy-splice)

;; TODO: undecided on W. maybe smth like `lispy-mark-first-list'?
(lispy-define-key lispy-mode-map (kbd "W") 'lispy-mark-list)

;; MAYBE use lispyville-drag-forward/backward instead
(lispy-define-key lispy-mode-map (kbd "H") 'lispy-move-up)
(lispy-define-key lispy-mode-map (kbd "L") 'lispy-move-down)

;;** paredit-like bindings
(lispy-define-key lispy-mode-map (kbd "?") 'lispy-convolute)
(define-key lispy-mode-map (kbd "M-?") 'lispy-convolute)
(define-key lispy-mode-map (kbd "M-(") 'lispy-wrap-round)
(define-key lispy-mode-map (kbd "M-9") 'lispy-wrap-round)

;;** avy-window-list-wrapper: add 'other option to avy-all-windows
(defun avy-window-list-wrapper (fn)
  "Removes current window from window list if `avy-all-windows'
is 'other."
  (if (eq avy-all-windows 'other)
      (remove (selected-window) (window-list))
    (funcall fn)))
(advice-add 'avy-window-list :around #'avy-window-list-wrapper)

;;** tT
(defun lispy-ace-first-paren (flip-windows)
  "Go to first paren on the line. TODO: nth paren on the line?"
  (interactive "P")
  (avy-with lispy-ace-first-paren
    (let ((candidates nil)
          (avy-all-windows 'other))
      (avy-dowindows (not flip-windows)
        ;; (debug (buffer-name (window-buffer (selected-window))))
        (save-excursion
          (save-restriction
            (unless (and (= (point-min) (window-start))
                         (= (point-max) (window-end)))
              (narrow-to-region (window-start) (window-end)))
            (goto-char (point-min))
            (let ((regexp (rx bol (* space) "(")))
              (loop while (search-forward-regexp regexp nil t)
                    do (let ((m (make-marker)))
                         (push (cons (move-marker m (1- (point)))
                                     (selected-window))
                               candidates)))))))
      (avy-process candidates))))

(lispy-define-key lispy-mode-map (kbd "t") 'lispy-ace-first-paren)
(global-set-key (kbd "M-t") 'lispy-ace-first-paren)
(lispy-define-key lispy-mode-map (kbd "T") 'lispy-teleport)

;;** cC
(lispy-define-key lispy-mode-map (kbd "c") 'lispy-kill-at-point)
(lispy-define-key lispy-mode-map (kbd "C") 'lispy-clone)

;;** D (experimental, probably better to delete sexp at point and empty newlines
(lispy-define-key lispy-mode-map (kbd "D") 'lispyville-delete-whole-line)

;;** C-h/DEL
(define-key lispy-mode-map (kbd "C-h") 'lispy-delete-backward)

(when (fboundp 'evil-define-key)
  (evil-define-key '(insert) paredit-mode-map
    (kbd "C-h") 'lispy-delete-backward))

(define-key lispy-mode-map (kbd "DEL") 'ignore)

;;** slime
(defun lispy-slime-init ()
  "Unbind/remap lispy keys for slime only."
  (with-minor-mode-map-overriding (map lispy-mode)
    (define-key map (kbd "M-.") nil)))

(add-hook 'slime-mode-hook #'lispy-slime-init)

;;** make C-a jump to indentation first
(defun lispy-back-to-indentation ()
  "If at indentation, move to beginning of line, else forward to
`back-to-indentation'. Reveal outlines."
  (interactive)
  (lispy--ensure-visible)
  (let ((pos (point)))
    (back-to-indentation)
    (when (eq pos (point))
      (move-beginning-of-line 1))))

(define-key lispy-mode-map (kbd "C-a") 'lispy-back-to-indentation)

;;** other global bindings
(define-key lispy-mode-map (kbd "<return>") 'lispy-right)
(define-key lispy-mode-map (kbd "RET") 'lispy-newline-and-indent)
(define-key lispy-mode-map (kbd "M-<return>") 'lispy-alt-line)
(define-key emacs-lisp-mode-map (kbd "C-c C-x C-x") 'hydra-lispy-x/body)
(define-key slime-mode-map (kbd "C-c C-x C-x") 'hydra-lispy-x/body)
(define-key slime-repl-mode-map (kbd "C-c C-x C-x") 'hydra-lispy-x/body)
(define-key emacs-lisp-mode-map (kbd "C-c x") 'hydra-lispy-x/body)
(define-key slime-mode-map (kbd "C-c x") 'hydra-lispy-x/body)
(define-key slime-repl-mode-map (kbd "C-c x") 'hydra-lispy-x/body)
(define-key slime-mode-map (kbd "M-s") 'lispy-splice)

(provide 'configure-lispy)
