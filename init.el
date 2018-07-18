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

(require 'avy)
(require 'expand-region)
(require 'delsel)
;; (require 'magit)
(require 'multiple-cursors)
(require 'wgrep)
(require 'paredit)
(require 'dired+)
(require 'ido-at-point)
(require 'ido-vertical-mode)
(require 'ido-completing-read+)
(require 'beginend)
(require 'smex)
(require 'uniquify)
(require 'saveplace)
(require 'slime)
(require 'slime-autoloads)

;; ./custom
(require 'configure-highlight)
(require 'configure-isearch)

(unless (fboundp 'helm-mode)
  (ido-mode t)
  (ido-at-point-mode t)
  (ido-vertical-mode t)
  (ido-ubiquitous-mode t)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only
        ido-vertical-show-count t
        ido-max-prospects 7
        ido-enable-flex-matching t))

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
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      expand-region-fast-keys-enabled nil
      er--show-expansion-message t
      inhibit-startup-message t
      wgrep-auto-save-buffer t
      uniquify-buffer-name-style 'forward)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; lisp
(setq inferior-lisp-program (getenv "LISP_BINARY"))
(setq slime-contribs '(slime-repl
                       slime-autodoc
                       slime-editing-commands
                       slime-fancy-inspector
                       slime-fancy-trace
                       ;; slime-mdot-fu
                       ;;slime-macrostep
                       slime-presentations
                       ;; slime-scratch
                       slime-references
                       ;; slime-fontifying-fu
                       slime-trace-dialog))
(slime-setup slime-contribs)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; indentation  (c) igord
(defun lisp-add-keywords (face-name keyword-rules)
   (let* ((keyword-list (mapcar #'(lambda (x)
                                    (symbol-name (cdr x)))
                                keyword-rules))
          (keyword-regexp (concat "(\\("
                                  (regexp-opt keyword-list)
                                  "\\)[ \n]")))
     (font-lock-add-keywords 'lisp-mode
                             `((,keyword-regexp 1 ',face-name))))
   (mapc #'(lambda (x)
             (put (cdr x)
                  ;;'scheme-indent-function
                  'common-lisp-indent-function
                  (car x)))
         keyword-rules))
 
(lisp-add-keywords
 'font-lock-keyword-face
 '((1 . mv-let*)
   (1 . letvar)
   (1 . letvar*)
   (nil . deftrf)
   (2 . !~)
   (2 . !.)
   (2 . foreach)
   (2 . foreach)
   (2 . forsome)
   (2 . forthis)
   (2 . forthis!)
   (2 . /.)
   (2 . foreach-child)
   
   (0 . aif)
   (1 . awhen)

   (2 . defclass*)
   ))



;; keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-<tab>") 'completion-at-point)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "C-x C-/") 'rgrep)
(define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)

(global-set-key (kbd "C-?") 'er/expand-region)
(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-,") 'er/contract-region)

(global-set-key (kbd "C-t") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)

(global-unset-key (kbd "C-x m"))
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-x m a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-:") 'mc/mark-all-like-this)
(global-set-key (kbd "C-x m l") 'mc/edit-lines)
(global-set-key (kbd "C-x m b") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-x m e") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-x m r") 'mc/mark-all-in-region-regexp)
(global-set-key (kbd "C-x m SPC") 'set-rectangular-region-anchor)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x M-x") 'execute-extended-command)

(global-set-key (kbd "C-x f") 'find-dired)
(define-key dired-mode-map (kbd "<backspace>") 'diredp-up-directory)
(define-key dired-mode-map (kbd "C-t") 'avy-goto-word-or-subword-1)

(global-set-key (kbd "C-c <f5>") 'slime-restart-inferior-lisp)
