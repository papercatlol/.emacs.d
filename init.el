;; packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))
(server-start)

;;** GC hacks
(defun gc-with-time ()
  (let ((time (current-time)))
    (garbage-collect)
    (message "GC took %.06f sec" (float-time (time-since time)))))

(setq gc-cons-threshold #x40000000) ; 1GB

(defvar gc-timer (run-with-idle-timer 30 t #'gc-with-time))

;;;
(require 'avy)
(require 'ace-window)
(require 'beginend)
(require 'delsel)
(require 'dired-subtree)
(require 'expand-region)
(require 'helpful)
(require 'hl-todo)
(require 'magit)
(require 'magit-todos)
(require 'multiple-cursors)
(require 'paredit)
(require 'saveplace)
(require 'shell-pop)
(require 'string-edit)
(require 'uniquify)
(require 'wgrep)

;;* ./custom
;;** local packages
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "custom/ace-link/" user-emacs-directory))
(require 'ace-link)
(require 'dired+)
;;** configuration
(require 'configure-ace-window)
(require 'configure-evil)
(require 'configure-highlight)
(require 'configure-isearch)
(require 'configure-ivy)
(require 'configure-go)
(require 'configure-lisp)


(delete-selection-mode 1)
;; (global-linum-mode t)
(global-display-line-numbers-mode t)
(show-paren-mode 1)
(minibuffer-depth-indicate-mode 1)
(recentf-mode 1)
(global-hl-todo-mode 1)
(magit-todos-mode 1)
(setq-default save-place t)
(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      dired-dwim-target t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      frame-title-format "%b"
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      expand-region-fast-keys-enabled nil
      er--show-expansion-message t
      inhibit-startup-message t
      wgrep-auto-save-buffer t
      uniquify-buffer-name-style 'forward
      aw-scope 'frame
      aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      avy-style 'de-bruijn
      avy-keys (list ?f ?c ?d ?g ?s ?a ?e ?v)
      lispy-avy-keys avy-keys
      view-read-only t
      enable-recursive-minibuffers t
      slime-description-autofocus t
      show-paren-priority -1
      shell-pop-window-size 50
      shell-pop-window-position "bottom"
      recentf-max-saved-items 50
      magit-section-visibility-indicator (quote (magit-fringe-bitmap+ . magit-fringe-bitmap-))
      magit-todos-auto-group-items 1000
      magit-diff-buffer-file-locked t
      magit-log-arguments '("-n64" "--graph" "--decorate" "--patch")
      hl-todo-keyword-faces '(("TODO" . "#cc9393")
                              ("FAIL" . "#8c5353")
                              ("NOTE" . "#d0bf8f")
                              ("KLUDGE" . "#d0bf8f")
                              ("HACK" . "#d0bf8f")
                              ("TEMP" . "#d0bf8f")
                              ("FIXME" . "#cc9393")
                              ("DONE" . "#98fb98")
                              ("MAYBE" . "#d0bf8f")))

(shell-pop--set-universal-key 'shell-pop-universal-key "<f12>")
(shell-pop--set-shell-type 'shell-pop-shell-type  '("ansi-term" "*ansi-term*"
                                                    (lambda nil
                                                      (ansi-term shell-pop-term-shell))))
(defalias 'yes-or-no-p 'y-or-n-p)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'magit-clean 'disabled nil)

(defun magit-status-set-wide-fringe (&optional arg)
  (display-line-numbers-mode -1)
  (set-window-fringes nil 11 5))

(add-hook 'magit-status-sections-hook #'magit-status-set-wide-fringe)
(add-hook 'magit-refs-mode-hook #'magit-status-set-wide-fringe)
(add-hook 'magit-revision-sections-hook #'magit-status-set-wide-fringe)

;; custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; modeline
(defvar minor-mode-lighters
  '((paredit-mode " Par")
    (auto-revert-mode "")
    (undo-tree-mode "")
    (ivy-mode "")
    (slime-mode " slime")
    (anzu-mode "")))

(defun cleaner-minor-modes ()
  (mapcar (lambda (mode)
            (or (assoc (car mode) minor-mode-lighters)
                mode))
          minor-mode-alist))

(setq-default mode-line-format
              (list "%e"
                    mode-line-front-space
                    mode-line-mule-info
                    mode-line-client
                    mode-line-modified
                    mode-line-remote
                    '(:eval (ace-window-path-lighter))
                    (list (propertize "%b" 'face 'mode-line-buffer-id))
                    ":%l %p "
                    evil-mode-line-tag
                    ;; '(:eval (when slime-mode (slime-current-package)))
                    '(vc-mode vc-mode)
                    " ["
                    '(:eval mode-name)
                    "] -"
                    ;; "%f -"
                    '(:eval (cleaner-minor-modes))
                    " %-"))

;; kludges
(defun window-as-frame ()
  "Pop current window as a new frame."
  (interactive)
  (let ((frame (make-frame)))
    (delete-window (get-buffer-window (current-buffer)))
    (select-frame frame)))

;; Fix for i3wm(not gaps) and emacs 26 where display doesn't refresh when switching
;; to an existing frame in tabbed or stacked layout.
(defun make-frame-visible-advice (&rest args)
  (make-frame-visible))
(advice-add 'select-frame-set-input-focus :after #'make-frame-visible-advice)

(defun kill-buffer-file-name ()
  "Add current buffer file name to kill ring."
  (interactive)
  (when-let ((filename (if (eq major-mode 'dired-mode)
                           default-directory
                         (buffer-file-name))))
    (kill-new filename)
    (message "%s" filename)))

(defun magit-stage-buffer-file ()
  (interactive)
  (magit-stage-file (buffer-file-name)))

;;** `edit-indirect'
(require 'edit-indirect)

(defun edit-indirect-inherit-major-mode (buffer beg end)
  (let ((major-mode (with-current-buffer buffer major-mode)))
    (condition-case nil
        (funcall major-mode)
      (error nil))))

(defun edit-indirect-visit-indirect-buffer ()
  "Visit indirect buffer linked to overlay at point."
  (interactive)
  (when-let ((buffer (loop for ov in (overlays-at (point))
                           thereis (overlay-get ov 'edit-indirect-buffer))))
    (switch-to-buffer-other-window buffer)))

(defun edit-indirect--rename-buffer ()
  (let* ((parent-buffer (overlay-buffer edit-indirect--overlay))
         (ov-start (overlay-start edit-indirect--overlay))
         (line (with-current-buffer parent-buffer
                 (line-number-at-pos ov-start))))
    (rename-buffer (format "*[IND] %s:%d*" parent-buffer line) t)))

(defun edit-indirect--set-keymap ()
  (overlay-put edit-indirect--overlay 'keymap edit-indirect--overlay-map))

(setq edit-indirect-guess-mode-function #'edit-indirect-inherit-major-mode)
(add-hook 'edit-indirect-after-creation-hook #'edit-indirect--rename-buffer)
(add-hook 'edit-indirect-after-creation-hook #'edit-indirect--set-keymap)

(defvar edit-indirect--overlay-map
  (let ((map (make-sparse-keymap)))
    (define-key map "I" 'edit-indirect-visit-indirect-buffer)
    map))

(define-key edit-indirect-mode-map (kbd "C-c C-c") nil)
(define-key edit-indirect-mode-map (kbd "C-c C-k") nil)
(define-key edit-indirect-mode-map (kbd "C-x C-w") 'edit-indirect-commit)
(define-key edit-indirect-mode-map (kbd "C-x q") 'bury-buffer)
(define-key edit-indirect-mode-map (kbd "C-x ESC") 'edit-indirect-abort)

;;** hippie-expand + paredit fix
(require 'hippie-exp)

(defun he-paredit-fix (str &optional trans-case)
  "Remove extra paren when expanding line in paredit.
https://www.emacswiki.org/emacs/HippieExpand#toc9"
  (if (and paredit-mode (equal (substring str -1) ")"))
      (progn (backward-delete-char 1) (forward-char))))

(advice-add #'he-substitute-string :after #'he-paredit-fix)

;; keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-<tab>") 'completion-at-point)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-o") 'ace-window)
(global-set-key (kbd "C-x C-c") 'ace-window)
(global-set-key (kbd "C-x 5 5") 'window-as-frame)
(global-set-key (kbd "C-x M-w") 'kill-buffer-file-name)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)

(global-set-key (kbd "C-?") 'er/expand-region)
(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-,") 'er/contract-region)

(global-set-key (kbd "C-t") 'avy-goto-char-2)
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)

(global-unset-key (kbd "C-x m"))
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-:") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-x m l") 'mc/edit-lines)
(global-set-key (kbd "C-x m b") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-x m e") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-x m r") 'mc/mark-all-in-region-regexp)
(global-set-key (kbd "C-x m SPC") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-x m h") 'mc-hide-unmatched-lines-mode)


(global-set-key (kbd "C-x C-d") 'dired)
(define-key dired-mode-map (kbd "<backspace>") 'diredp-up-directory)
(define-key dired-mode-map (kbd "C-t") 'avy-goto-word-or-subword-1)
(define-key dired-mode-map (kbd "<tab>") 'dired-subtree-toggle)
(define-key dired-mode-map (kbd "i") 'dired-subtree-toggle)
(define-key dired-mode-map (kbd "I") 'dired-subtree-remove)


;;** magit
(define-key magit-file-mode-map (kbd "C-x =") 'magit-diff-buffer-file)
(define-key magit-file-mode-map (kbd "C-x G") 'magit-file-dispatch)
(define-key magit-mode-map (kbd "C-c C-l") 'magit-toggle-buffer-lock)

(define-prefix-command 'magit-file-prefix-map)
(define-key magit-file-mode-map (kbd "C-x g") 'magit-file-prefix-map)
(define-key magit-file-prefix-map "g" 'magit-status)
(define-key magit-file-prefix-map "l" 'magit-log-buffer-file)
(define-key magit-file-prefix-map "f" 'magit-find-file)
(define-key magit-file-prefix-map "b" 'magit-blame)
(define-key magit-file-prefix-map "s" 'magit-stage-buffer-file)
(define-key magit-file-prefix-map "d" 'magit-diff-buffer-file)
(define-key magit-file-prefix-map "D" 'vc-ediff)

;;
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f6>") 'counsel-org-capture)

;; TODO: keymap for M-z prefix
(global-unset-key (kbd "M-z"))
(global-set-key (kbd "M-z s") 'string-edit-at-point)

(ace-link-setup-default (kbd "C-f"))
(dolist (keymap (list help-mode-map package-menu-mode-map compilation-mode-map grep-mode-map))
  (define-key keymap (kbd "C-f") 'ace-link))

;;** Helpful
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
