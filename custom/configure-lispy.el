;;* lispy-avy-keys
(setq lispy-avy-keys avy-keys)

;;* disable C-digits theme
(setq lispy-key-theme '(special lispy))
(lispy-set-key-theme lispy-key-theme)

;;* no lispy-colon magic (might revisit this later)
(setq lispy-colon-p nil)

;;* lispy-x hydra
(setq lispy-x-default-verbosity 0)

(defhydra+ hydra-lispy-x (:exit t :hint 0.3 :columns 3)
  ("M" lispy-multiline "m-line")
  ("O" lispy-oneline "1-line"))

;;* outline regexp
(setq lispy-outline lisp-outline-regexp)

;;* hooks
(add-hook 'emacs-lisp-mode-hook #'lispy-mode)
(add-hook 'inferior-emacs-lisp-mode-hook #'lispy-mode)
(add-hook 'lisp-mode-hook #'lispy-mode)
(add-hook 'slime-repl-mode-hook #'lispy-mode)
(add-hook 'string-edit-regexp-mode-hook #'lispy-mode)

(defun eval-expression-enable-lispy ()
  (when (or (eq this-command 'eval-expression)
            (eq this-command 'pp-eval-dwim)
            ;; TODO: slime/sldb evals
            )
    (lispy-mode 1)))
(add-hook 'minibuffer-setup-hook #'eval-expression-enable-lispy)

;;* disable lispy in magit-blame
;; FIXME this is a temporary solution. Ideally both magit-blame and lispy
;; keybindings should be available either via a hydra-magit-blame, higher
;; priority for magit-blame or some special keybind magic.
(add-to-list 'magit-blame-disable-modes 'lispy-mode)

;;* unmap
(define-key lispy-mode-map (kbd "M-j") nil)
(define-key lispy-mode-map (kbd "M-k") nil)
(define-key lispy-mode-map (kbd "M-.") nil)
(define-key lispy-mode-map (kbd ".") nil)
(define-key lispy-mode-map (kbd "C-,") 'er/contract-region)

;;* 'special' bindings
;; (these make more sense to me)
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
(lispy-define-key lispy-mode-map (kbd "C-o") 'lispy-back)

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
(define-key lispy-mode-map (kbd "M-s") 'lispy-goto-symbol-in-line)
(define-key lispy-mode-map (kbd "C-c s") 'lispy-splice)

;;** W is a matching command to B = `special-lispy-ediff-regions'
;; MAYBE map to some ace-jump version of `w' instead
;; MAYBE message some info about stored buffer/region for verbosity and/or
;; highlight or blink stored region
(lispy-define-key lispy-mode-map (kbd "W") 'lispy-store-region-and-buffer)

;; MAYBE use lispyville-drag-forward/backward instead
(lispy-define-key lispy-mode-map (kbd "H") 'lispy-move-up)
(lispy-define-key lispy-mode-map (kbd "L") 'lispy-move-down)

;;** paredit-like bindings
(lispy-define-key lispy-mode-map (kbd "?") 'lispy-convolute)
(define-key lispy-mode-map (kbd "M-?") 'lispy-convolute)
(define-key lispy-mode-map (kbd "M-(") 'lispy-wrap-round)
(define-key lispy-mode-map (kbd "C-9") 'lispy-wrap-round)
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

;;** O
(defun lispy-left-and-newline (arg)
  "Call `lispy-left' ARG times, then open a newline above and
move cursor there. If at list boundaries treat it like we're
inside of that list."
  (interactive "p")
  (when (or (lispy-left-p) (lispy-right-p))
    (cond ((plusp arg) (decf arg))
          ((minusp arg) (incf arg))))
  (lispy-left arg)
  (back-to-indentation)
  (move-beginning-of-line 1)
  (lispy-newline-and-indent)
  (previous-line)
  (indent-according-to-mode))

(lispy-define-key lispy-mode-map "O" 'lispy-left-and-newline)

;;** v
(define-key lispy-mode-map "v" 'special-lispy-mark-list)

;;** slime
(defun lispy-slime-init ()
  "Unbind/remap lispy keys for slime only."
  (with-minor-mode-map-overriding (map lispy-mode)
    (define-key map (kbd "M-.") nil)
    (define-key map (kbd "C-j") nil)
    (define-key map (kbd ",") 'lispy-slime-repl-comma)))

(add-hook 'slime-mode-hook #'lispy-slime-init)
(add-hook 'slime-repl-mode-hook #'lispy-slime-init)

;;*** fix lispy--exit-string jumping to random places in slime repl
(defun lispy--exit-string-slime-repl-wrapper (fn &rest args)
  (when (eq major-mode 'slime-repl-mode)
    (save-restriction
      (when-let ((boundary
                  (if (and (boundp 'slime-repl-input-start-mark)
                           slime-repl-input-start-mark
                           (>= (point) slime-repl-input-start-mark))
                      slime-repl-input-start-mark
                    (save-excursion (slime-repl-find-prompt t)
                                    (point)))))
        (narrow-to-region slime-repl-input-start-mark (point-max)))
      (apply fn args)))
  (apply fn args))

(advice-add 'lispy--exit-string :around #'lispy--exit-string-slime-repl-wrapper)
(advice-add 'lispy--out-forward :around #'lispy--exit-string-slime-repl-wrapper)
(advice-remove 'lispy--out-forward  #'lispy--exit-string-slime-repl-wrapper)

;;*** don't touch whitespaces - this can trigger `text-read-only' error in repl
(defun lispy-disable-whitespace-cleanup ()
  (setq-local lispy-ignore-whitespace t))
(add-hook 'slime-repl-mode-hook #'lispy-disable-whitespace-cleanup)

;;*** also don't indent
(add-to-list 'lispy-no-indent-modes 'slime-repl-mode)

;;*** supress `text-read-only' error when indenting in slime-repl
;; Slime repl has read-only text which triggers an error. This is a lazy solution.
;;(defun slime-repl-ignore-text-read-only-wrapper (fn &rest args)
;;  (if (eq major-mode 'slime-repl-mode)
;;      (condition-case e
;;          (apply fn args)
;;        (error
;;         (when (eq (car e) 'text-read-only)
;;           (message "Text is read-only."))))
;;    (apply fn args)))

;;(advice-add 'indent-region :around #'slime-repl-ignore-text-read-only-wrapper)
;;(advice-remove 'indent-region  #'slime-repl-ignore-text-read-only-wrapper)

;;** slime repl comma shortcuts
(defun lispy-slime-repl-comma ()
  (interactive)
  (call-interactively
   (if (and slime-repl-mode-map
            (or (not lispy-mode)
                ;; Too lazy to handle whitespace input, this should be enough.
                (= (point) (point-max))))
       #'slime-handle-repl-shortcut
     ;; MAYBE: toggle comma on current sexp
     #'self-insert-command)))

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

;;* regular slurp/barf
(define-key lispy-mode-map (kbd "C-c C-x .") 'lispy-forward-slurp-sexp)
(define-key lispy-mode-map (kbd "C-c C-x ,") 'lispy-forward-barf-sexp)
(define-key lispy-mode-map (kbd "C-c C-x <") 'lispy-backward-slurp-sexp)
(define-key lispy-mode-map (kbd "C-c C-x >") 'lispy-backward-barf-sexp)

;;** other global bindings
(define-key lispy-mode-map (kbd "<return>") 'lispy-right)
(define-key lispy-mode-map (kbd "RET") 'lispy-newline-and-indent-plain)
(define-key lispy-mode-map (kbd "M-<return>") 'lispy-alt-line)
(define-key emacs-lisp-mode-map (kbd "C-c C-x C-x") 'hydra-lispy-x/body)
(define-key slime-mode-map (kbd "C-c C-x C-x") 'hydra-lispy-x/body)
(define-key slime-repl-mode-map (kbd "C-c C-x C-x") 'hydra-lispy-x/body)
(define-key emacs-lisp-mode-map (kbd "C-c x") 'hydra-lispy-x/body)
(define-key slime-mode-map (kbd "C-c x") 'hydra-lispy-x/body)
(define-key slime-repl-mode-map (kbd "C-c x") 'hydra-lispy-x/body)
(define-key slime-mode-map (kbd "M-s") 'avy-goto-symbol-in-line)

(provide 'configure-lispy)
