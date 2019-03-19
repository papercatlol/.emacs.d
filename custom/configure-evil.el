;;; -*- lexical-binding: t -*-
;; must be set before loading evil?
(setq evil-search-wrap t
      evil-want-Y-yank-to-eol t
      evil-regexp-search t
      evil-disable-insert-state-bindings t
      evil-move-beyond-eol t
      lispy-avy-style-paren 'pre)

(require 'evil)

(evil-mode t)
;; (dolist (mode '(slime-popup-buffer-mode slime-trace-dialog-mode))
;;   (evil-set-initial-state mode 'emacs))
(evil-set-initial-state 'slime-trace-dialog-mode 'emacs)


;;* `LISPYVILLE'
(require 'lispyville)

(add-hook 'paredit-mode-hook #'lispyville-mode)

(with-eval-after-load 'lispyville
  (lispyville-set-key-theme
   '(operators
     prettify
     text-objects
     c-w
     (atom-movement t)
     slurp/barf-cp
     additional-insert
     (escape insert)
     ;;   (additional-movement normal visual motion)
     )))

;;* `DEFUNS'
;; -----------------------------------------------------------------------------
(defun end-of-defun-spammable ()
  (interactive)
  (forward-char)
  (call-interactively #'end-of-defun)
  (backward-char))

(defun C-w-dwim ()
  "Kill region if active, else delete a word backward."
  (interactive)
  (if (region-active-p)
      (evil-delete (point) (mark))
    (evil-delete-backward-word)))

(defun q-dwim ()
  "Quit window if in read-only, else record a macro."
  (interactive)
  (call-interactively
   (if buffer-read-only
       #'quit-window
     #'evil-record-macro)))

;; TODO
;;** `evil-visual-block-mc-or-ret'
(require 'multiple-cursors)

(defun visual-block-mc-or-ret ()
  (interactive)
  (if (eq (evil-visual-type) 'block)
      ;; enable mc
      nil
    (evil-ret)))

;;** `evil-visual-char-or-expand-region'
(require 'expand-region)

(defun evil-visual-char-or-expand-region ()
  (interactive)
  (if (region-active-p)
        (call-interactively 'er/expand-region)
    (evil-visual-char)))

;;** `swiper-evil-ex'
(require 'swiper)

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

;;** `avy-goto-symbol-in-line'
(defun avy-goto-symbol-in-line ()
  "Jump to start of a symbol in current line. Exclude symbol at point."
  (interactive)
  (let* ((avy-all-windows nil)
         (re-symbol-start (rx symbol-start any))
         (re-word-start (rx word-start any))
         (beg (1+ (line-beginning-position)))
         (end (1- (line-end-position)))
         (redundant (list (point) (1+ (point)))))
    (cl-flet ((%filter (candidates)
                      (loop for candidate in candidates
                            unless (member (caar candidate) redundant)
                            collect candidate)))
      (when-let (bounds (bounds-of-thing-at-point 'symbol))
        (push (car bounds) redundant)
        (push (cdr bounds) redundant))
      (let ((candidates (or (%filter (avy--regex-candidates re-symbol-start beg end))
                            (%filter (avy--regex-candidates re-word-start beg end)))))
        (avy-with avy-goto-symbol-in-line
          (avy--process candidates (avy--style-fn 'at)))))))

(add-to-list 'avy-styles-alist '(avy-goto-symbol-in-line . at))
(add-to-list 'avy-keys-alist
             (cons 'avy-goto-symbol-in-line (list ?f ?c ?d ?g ?s ?a  ?e ?v ?q ?w ?z ?x ?r
                                                  ?j ?n ?k ?h ?l ?o ?i ?u ?p ?\;)))

;;** `evil-move'
(evil-define-command evil-move-forward (beg end n)
  "Move lines in BEG END N lines forward (backward of N is negative)."
  :motion evil-line
  (interactive "<r>p")
  (evil-move beg end (+ n (line-number-at-pos (1- (if (plusp n) end beg)))))
  (evil-normal-state)
  (evil-visual-restore))

(evil-define-command evil-move-backward (beg end n)
  "Move lines in BEG END N lines backward (forward of N is negative)."
  :motion evil-line
  :keep-visual t
  (interactive "<r>p")
  (evil-move-forward beg end (- n)))

;;** `scrolling'
(defun evil-scroll-line-up-dwim ()
  (interactive)
  (evil-scroll-line-up (or (and current-prefix-arg
                                  (prefix-numeric-value current-prefix-arg))
                             4)))

(defun evil-scroll-line-down-dwim ()
  (interactive)
  (evil-scroll-line-down (or (and current-prefix-arg
                                  (prefix-numeric-value current-prefix-arg))
                             4)))

;;* `KEYS'
;; -----------------------------------------------------------------------------
;;** `lispyville'
(with-eval-after-load 'lispyville
  (evil-define-key '(normal insert) lispyville-mode-map
    (kbd "C-t") 'lispy-ace-paren)

  (evil-define-key '(insert) slime-repl-mode-map
    (kbd "C-t") 'evil-avy-goto-char-2)

  (evil-define-key 'motion lispyville-mode-map
    "[" 'lispyville-previous-opening
    "]" 'lispyville-next-closing)

  (evil-define-key 'normal lispyville-mode-map
    (kbd "<backspace>") 'lispyville-beginning-of-defun
    (kbd "<return>") 'end-of-defun-spammable)

  (evil-define-key '(operator visual) lispyville-mode-map
    "s" 'evil-a-paren
    "x" 'lispyville-a-sexp))

;;** `visual-or-expand-region'
(define-key evil-normal-state-map "v" 'evil-visual-char-or-expand-region)
(define-key evil-visual-state-map "v" 'evil-visual-char-or-expand-region)
(define-key evil-visual-state-map (kbd "M-v") 'er/contract-region)
(define-key evil-visual-state-map [escape] 'evil-visual-char)

;;** `evil-window-map'
(define-key evil-window-map (kbd "C-w") 'ace-window)
(define-key evil-window-map (kbd "w") 'ace-window)
(define-key evil-window-map (kbd "C-q") 'evil-window-delete)
(define-key evil-window-map (kbd "q") 'evil-window-delete)

;;** `ex'
(define-key evil-ex-completion-map (kbd "C-a") nil)
(define-key evil-ex-completion-map (kbd "C-b") nil)
(define-key evil-ex-completion-map (kbd "C-k") nil)
(define-key evil-ex-completion-map (kbd "C-d") nil)
(define-key swiper-map (kbd "M-;") 'swiper-evil-ex)

;;** `unbind'
(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-normal-state-map (kbd "M-,") nil)
(define-key evil-normal-state-map (kbd "C-.") nil)
(define-key evil-normal-state-map (kbd "C-,") nil)
(define-key evil-motion-state-map (kbd "C-b") nil)
(define-key evil-motion-state-map (kbd "C-e") nil)

;;** `other'
(define-key evil-normal-state-map "q" 'q-dwim)
(define-key evil-insert-state-map (kbd "C-w") 'C-w-dwim)
(define-key evil-motion-state-map (kbd "M-j") 'evil-scroll-line-down-dwim)
(define-key evil-motion-state-map (kbd "M-k") 'evil-scroll-line-up-dwim)
(define-key evil-motion-state-map (kbd "C-f") 'evil-avy-goto-char-2)
(define-key evil-normal-state-map (kbd "s") 'avy-goto-symbol-in-line)
(define-key evil-visual-state-map (kbd "S") 'avy-goto-symbol-in-line)

;; Do not override `ace-link' binding by motion-state `C-f' binding
(require 'slime)
(dolist (map (list helpful-mode-map help-mode-map compilation-mode-map grep-mode-map
                   sldb-mode-map slime-inspector-mode-map slime-xref-mode-map))
  (evil-make-overriding-map map 'motion))

;;** `move-text'
(defhydra move-text-hydra ()
  ("j" evil-move-forward "down")
  ("k" evil-move-backward "up")
  ("C-g" nil "exit"))

(defun evil-move-forward-and-hydra ()
  (interactive)
  (call-interactively #'evil-move-forward)
  (move-text-hydra/body))

(defun evil-move-backward-and-hydra ()
  (interactive)
  (call-interactively #'evil-move-backward)
  (move-text-hydra/body))

(define-key evil-visual-state-map (kbd "C-c j") 'evil-move-forward-and-hydra)
(define-key evil-visual-state-map (kbd "C-c k") 'evil-move-backward-and-hydra)


(provide 'configure-evil)
