;; packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))
(server-start)

(require 'avy)
(require 'ace-window)
(require 'expand-region)
(require 'delsel)
(require 'magit)
(require 'multiple-cursors)
(require 'wgrep)
(require 'paredit)
(require 'dired+)
(require 'beginend)
(require 'uniquify)
(require 'saveplace)

;; ./custom
(require 'configure-evil)
(require 'configure-highlight)
(require 'configure-isearch)
(require 'configure-ivy)
(require 'configure-slime)


(delete-selection-mode 1)
(global-linum-mode t)
(show-paren-mode 1)
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
      view-read-only t
      enable-recursive-minibuffers t
      slime-description-autofocus t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)


;; keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-<tab>") 'completion-at-point)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key [remap other-window] 'ace-window)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)

(global-set-key (kbd "C-?") 'er/expand-region)
(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-,") 'er/contract-region)

(global-set-key (kbd "C-t") 'avy-goto-char-timer)
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

(global-set-key (kbd "C-x g g") 'magit-status)
(global-set-key (kbd "C-x g l") 'magit-log)
(global-set-key (kbd "C-x g f") 'magit-log-buffer-file)
(global-set-key (kbd "C-x g b") 'magit-blame)

(global-set-key (kbd "C-7") 'point-to-register)
(global-set-key (kbd "C-8") 'jump-to-register)
