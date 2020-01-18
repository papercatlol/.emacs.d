;;; -*- lexical-binding: t -*-
;; must be set before loading evil?
(setq evil-search-wrap t
      evil-want-Y-yank-to-eol t
      evil-regexp-search t
      evil-disable-insert-state-bindings t
      evil-move-beyond-eol t
      evil-want-integration t
      evil-want-keybinding t
      lispy-avy-style-paren 'pre)

(require 'evil)

(evil-mode t)

(defmacro evil-with-insert-state (command)
  (let ((name (intern (concat "evil-" (symbol-name command)))))
    (if (fboundp name)
        `#',name
      `(defun ,name (&rest args)
         (interactive)
         (call-interactively #',command)
         (evil-insert 1)))))

;; (dolist (mode '(slime-popup-buffer-mode slime-trace-dialog-mode))
;;   (evil-set-initial-state mode 'emacs))
(evil-set-initial-state 'slime-trace-dialog-mode 'emacs)
(evil-set-initial-state 'ibuffer-mode 'normal)


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

;;* `EVIL-MC'
(require 'evil-mc)

(setq evil-mc-undo-cursors-on-keyboard-quit t)
(setq evil-mc-custom-known-commands
      (mapcar (lambda (symbol)
                (cons symbol '((:default . evil-mc-execute-default-call-with-count))))
              mc/cmds-to-run-for-all))

;; (global-evil-mc-mode 1)

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

(defun avy-goto-symbol-end-in-line ()
  "Same as `avy-goto-symbol-in-line', but jump to symbol end instead of beginning."
  (interactive)
  (call-interactively #'avy-goto-symbol-in-line)
  (call-interactively #'forward-symbol))

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
;;** smarter `M-w'
(defun evil-append-symbol ()
  "Switch to Insert state just after next symbol."
  (interactive)
  (forward-symbol 1)
  (call-interactively #'evil-insert))

(defun M-w-dwim ()
  "Save region to kill ring if active, else call `evil-append-symbol'."
  (interactive)
  (call-interactively (if (region-active-p)
                          #'kill-ring-save
                        #'evil-append-symbol)))

(global-set-key [remap kill-ring-save] 'M-w-dwim)

;;** `evil-show-jumps-ivy'
;; TODO: move ivy-file-jumping-related logic to separate package
(defun evil-show-jumps-ivy ()
  "Use `ivy' to navigate evil jump list."
  (interactive)
  (let* ((jumps (cl-remove-duplicates (evil--jumps-savehist-sync)
                                      :test #'equal))
         (ivy-items (mapcar (lambda (jump)
                              (destructuring-bind (pos file) jump
                                (list (format "%s:%s" file pos) pos file)))
                            jumps))
         (initial-pos (point)))
    (cl-labels ((%jump (item)
                       (find-file (third item))
                       (goto-char (second item))))
      (ivy-read "Jump: " ivy-items
                :action #'%jump
                :update-fn (lambda ()
                             (with-ivy-window
                               ;; TODO: less ugly way to get current ivy item
                               (%jump (elt (ivy-state-collection ivy-last)
                                           (get-text-property 0 'idx (ivy-state-current ivy-last))))))
                :unwind (lambda ()
                          (unless (eq ivy-exit 'done)
                            (goto-char initial-pos)))))))

(defun evil-jump-backward-dwim (arg)
  (interactive "P")
  (if arg (evil-show-jumps-ivy)
    (call-interactively #'evil-jump-backward)))

(defun evil-jump-forward-dwim (arg)
  (interactive "P")
  (if arg (evil-show-jumps-ivy)
    (call-interactively #'evil-jump-forward)))

;;** tab bindings
(defun tab-indent ()
  "Like `indent-for-tab-command', but don't try completing regardless of `tab-always-indent' value."
  (interactive)
  (let ((tab-always-indent t))
    (call-interactively #'indent-for-tab-command)))

(define-key evil-motion-state-map (kbd "C-o") 'evil-jump-backward-dwim)
(define-key evil-motion-state-map (kbd "<C-i>") 'evil-jump-forward-dwim)
(define-key evil-motion-state-map (kbd "<tab>") 'tab-indent)

;;** `i3wm-frames'
;; (defun frame-list-for-i3-workspace ()
;;   "KLUDGE. List of frames in current i3 workspace. Doesn't return current frame because it is
;; sorted before other frames by `frame-list-z-order'."
;;   (let* ((current-frame (window-frame))
;;          (current-monitor (frame-monitor-attribute 'name))
;;          (frames (frame-list-z-order)))
;;     (loop for frame in frames
;;           when (and (string= current-monitor (frame-monitor-attribute 'name frame))
;;                     (eq 'icon (frame-visible-p frame)))
;;           collect frame)))

;; (defun switch-to-frame (n)
;;   (when-let* ((frames (frame-list-for-i3-workspace))
;;               (frame (elt frames (1- n))))
;;     (select-frame-set-input-focus frame)))

;;** avy-goto-symbol-2
(defun avy-goto-symbol-2 (char1 char2 &optional arg beg end word)
  "Like `avy-goto-word-1', but query for 2 chars and go to symbol start by default"
  (interactive (list (read-char "char 1: " t)
                     (read-char "char 2: " t)
                     current-prefix-arg))
  (avy-with avy-goto-symbol-2
    (let* ((str (string char1 char2))
           (symbol-start (rx symbol-start
                             ;; #:symbol :symbol *symbol
                             (or (and (? "#") (? ":"))
                                 (? "*"))))
           (regex (concat (if word "\\b" symbol-start)
                          (regexp-quote str))))
      (avy-jump regex
                :window-flip arg
                :beg beg
                :end end))))

(evil-define-avy-motion avy-goto-symbol-2 exclusive)

;;* `KEYS'
;; -----------------------------------------------------------------------------
;;** `lispyville'
(with-eval-after-load 'lispyville
  (evil-define-key '(normal insert) lispyville-mode-map
    (kbd "C-t") 'lispy-ace-paren)

  (evil-define-key '(insert) slime-repl-mode-map
    (kbd "C-t") 'evil-avy-goto-char-2)

  (evil-define-key 'motion lispyville-mode-map
    "[" #'lispyville-previous-opening
    "]" #'lispyville-next-closing
    "(" #'lispyville-backward-up-list
    ")" #'lispyville-up-list)

  (evil-define-key '(normal visual) lispyville-mode-map
    (kbd "<backspace>") 'lispyville-beginning-of-defun
    (kbd "<return>") 'lispyville-beginning-of-next-defun
    ;; (kbd "<return>") 'end-of-defun-spammable
    [remap paredit-comment-dwim] 'lispyville-comment-or-uncomment
    "gc" 'lispyville-comment-or-uncomment
    "gy" 'lispyville-comment-and-clone-dwim
    (kbd "M-R") 'lispyville-raise-list
    "H" 'lispyville-drag-backward
    "L" 'lispyville-drag-forward)

  (evil-define-key '(operator visual) lispyville-mode-map
    "s" 'evil-a-paren
    "x" 'lispyville-a-sexp))

;;** paredit
(with-eval-after-load 'paredit-mode
  (define-key evil-normal-state-map (kbd "M-(") (evil-with-insert-state paredit-wrap-round))
  (define-key evil-insert-state-map (kbd "M-e") 'paredit-forward)
  (define-key evil-motion-state-map (kbd "C-f") 'paredit-forward)
  (define-key evil-motion-state-map (kbd "C-b") 'paredit-backward))

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

;;** `leader'
(defvar leader-map (make-sparse-keymap))

(define-key evil-normal-state-map (kbd "SPC") leader-map)
(define-key evil-motion-state-map (kbd "SPC") leader-map)
(define-key evil-normal-state-map (kbd "S-SPC") 'evil-avy-goto-char-timer)
(define-key evil-motion-state-map (kbd "S-SPC") 'evil-avy-goto-char-timer)
(define-key evil-motion-state-map (kbd "C-S-SPC") 'avy-resume)

(with-eval-after-load 'cus-edit (define-key custom-mode-map (kbd "SPC") leader-map))
(with-eval-after-load 'markdown-mode (define-key markdown-view-mode-map (kbd "SPC") leader-map))
(with-eval-after-load 'simple (define-key special-mode-map (kbd "SPC") leader-map))
(with-eval-after-load 'tabulated-list (define-key tabulated-list-mode-map (kbd "SPC") leader-map))
(with-eval-after-load 'magit
  (define-key magit-diff-mode-map (kbd "SPC") leader-map)
  (define-key magit-status-mode-map (kbd "SPC") leader-map)
  (define-key magit-mode-map (kbd "SPC") leader-map))
(define-key splash-screen-keymap (kbd "SPC") leader-map)

(define-key leader-map (kbd "SPC") 'evil-avy-goto-char-timer)
(define-key leader-map "f" 'evil-avy-goto-symbol-2)
(define-key leader-map "b" 'counsel-ibuffer-or-recentf)
(define-key leader-map "c" 'ace-window)
(define-key leader-map (kbd "<tab>") 'other-window)
(define-key leader-map (kbd "o") 'counsel-outline)
(define-key leader-map (kbd "d") 'delete-other-windows-toggle)

;; `evil-mc'
;; (with-eval-after-load 'evil-mc
;;   (define-key evil-visual-state-map (kbd "I") 'evil-mc-make-cursor-in-visual-selection-beg)
;;   (define-key evil-visual-state-map (kbd "A") 'evil-mc-make-cursor-in-visual-selection-end))

;; `org'
(evil-define-key '(normal visual) org-mode-map
  "L" 'org-metaright
  "H" 'org-metaleft
  "o" (evil-with-insert-state org-insert-heading-respect-content)
  "O" (evil-with-insert-state org-meta-return)
  (kbd "<tab>") 'org-cycle)

;;** `unbind'
(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-normal-state-map (kbd "M-,") nil)
(define-key evil-normal-state-map (kbd "C-.") nil)
(define-key evil-normal-state-map (kbd "C-,") nil)
(define-key evil-motion-state-map (kbd "C-b") nil)
(define-key evil-motion-state-map (kbd "C-e") nil)

;;** insert state
(define-key evil-insert-state-map (kbd "M-o") 'evil-open-below)
;; TODO: think about it; maybe bind insert-state M-s to normal-state s etc.
;; (define-key evil-insert-state-map (kbd "M-a") 'evil-append-line)
;; (define-key evil-insert-state-map (kbd "M-i") 'evil-insert-line)

;;** `other'
;; (define-key evil-normal-state-map "q" 'q-dwim)
(define-key evil-normal-state-map "Q" "@q")
(define-key evil-visual-state-map "Q" ":norm @q RET")
(define-key evil-visual-state-map "." ":norm . RET")
(define-key evil-insert-state-map (kbd "C-w") 'C-w-dwim)
(global-set-key (kbd "M-j") 'evil-scroll-line-down-dwim)
(global-set-key (kbd "M-k") 'evil-scroll-line-up-dwim)
;; (define-key evil-motion-state-map (kbd "M-j") 'evil-scroll-line-down-dwim)
;; (define-key evil-normal-state-map (kbd "M-j") 'evil-scroll-line-down-dwim)
;; (define-key evil-motion-state-map (kbd "M-k") 'evil-scroll-line-up-dwim)
;; (define-key evil-normal-state-map (kbd "M-k") 'evil-scroll-line-up-dwim)
;; (define-key magit-mode-map (kbd "M-j") 'evil-scroll-line-down-dwim)
;; (define-key magit-mode-map (kbd "M-k") 'evil-scroll-line-up-dwim)
(define-key evil-normal-state-map (kbd "s") 'avy-goto-symbol-in-line)
(define-key evil-normal-state-map (kbd "S") 'avy-goto-symbol-end-in-line)
(define-key evil-visual-state-map (kbd "S") 'avy-goto-symbol-in-line)
(define-key evil-visual-state-map (kbd "C-c i") 'edit-indirect-region)
(define-key evil-normal-state-map (kbd "C-n") 'counsel-ibuffer-or-recentf-other-window)

;;** evil keymaps bullshit
(require 'slime)
(dolist (map (list helpful-mode-map help-mode-map
                   compilation-mode-map grep-mode-map
                   sldb-mode-map slime-inspector-mode-map slime-xref-mode-map
                   special-mode-map messages-buffer-mode-map
                   ))
  (evil-set-initial-state map 'normal)
  (evil-make-overriding-map map 'normal)
  (evil-add-hjkl-bindings map))


;;** ibuffer
;; old M-j: ibuffer-jump-to-filter-group
(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "M-j") 'evil-scroll-line-down-dwim)
  (define-key ibuffer-mode-map (kbd "M-k") 'evil-scroll-line-up-dwim))

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
