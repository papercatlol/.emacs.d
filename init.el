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
(require 'configure-evil)
(require 'configure-highlight)
(require 'configure-isearch)
(require 'configure-ivy)
(require 'configure-slime)
(require 'configure-go)


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
      aw-ignore-current t
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
                    mode-line-frame-identification
                    (list (propertize "%b" 'face 'mode-line-buffer-id))
                    " at %l (%p) "
                    evil-mode-line-tag
                    '(:eval (when slime-mode (slime-current-package)))
                    '(vc-mode vc-mode)
                    " ["
                    '(:eval mode-name)
                    "] "
                    "%f -"
                    '(:eval (cleaner-minor-modes))
                    " %-"))

;; kludges
(defun window-as-frame ()
  "Pop current window as a new frame."
  (interactive)
  (let ((frame (make-frame)))
    (delete-window (get-buffer-window (current-buffer)))
    (select-frame frame)))

(defun kill-buffer-file-name ()
  "Add current buffer file name to kill ring."
  (interactive)
  (when-let ((filename (if (eq major-mode 'dired-mode)
                           default-directory
                         (buffer-file-name))))
    (kill-new filename)
    (message "%s" filename)))

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
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key [remap other-window] 'ace-window)
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
(define-key dired-mode-map (kbd "<tab>") 'other-window)
(define-key dired-mode-map (kbd "i") 'dired-subtree-toggle)
(define-key dired-mode-map (kbd "I") 'dired-subtree-remove)

(define-key magit-file-mode-map (kbd "C-x g") nil)
(global-set-key (kbd "C-x G") 'magit-file-dispatch)
(global-set-key (kbd "C-x g g") 'magit-status)
(global-set-key (kbd "C-x g l") 'magit-log-buffer-file)
(global-set-key (kbd "C-x g f") 'magit-find-file)
(global-set-key (kbd "C-x g b") 'magit-blame)
(global-set-key (kbd "C-x g d") 'magit-diff-buffer-file)
(global-set-key (kbd "C-x g D") 'vc-ediff)
(global-set-key (kbd "C-x =") 'magit-diff-buffer-file)

(define-key magit-mode-map (kbd "C-c C-l") 'magit-toggle-buffer-lock)

(global-set-key (kbd "<f5>") 'revert-buffer)

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
