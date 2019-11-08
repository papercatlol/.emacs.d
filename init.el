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
    (unless (or (minibuffer-window-active-p (selected-window))
                (> (window-height (minibuffer-window)) 1))
      (garbage-collect)
      (message "GC took %.06f sec" (float-time (time-since time))))))

(setq gc-cons-threshold #x40000000) ; 1GB

(defvar gc-timer (run-with-idle-timer 30 t #'gc-with-time))

;;;
(require 'avy)
(require 'ace-window)
(require 'async)
(require 'beginend)
(require 'delsel)
(require 'dired-git-info)
(require 'dired-subtree)
(require 'dired-rsync)
(require 'expand-region)
(require 'helpful)
(require 'hl-todo)
(require 'ivy-xref)
(require 'magit)
(require 'magit-todos)
(require 'multiple-cursors)
(require 'paredit)
(require 'pcmpl-args)
(require 'reverse-im)
(require 'shell-pop)
(require 'string-edit)
(require 'uniquify)
(require 'vterm)
(require 'wgrep)

;;* ./custom
;;** local packages
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "custom/ace-link/" user-emacs-directory))
(require 'ace-link)
(require 'compilation-to-dired)
(require 'dired+)
;;** configuration
(require 'configure-ace-window)
(require 'configure-evil)
(require 'configure-highlight)
(require 'configure-isearch)
(require 'configure-ivy)
;; (require 'configure-go)
(require 'configure-go-lsp)
(require 'configure-lisp)
(require 'configure-python)


(delete-selection-mode 1)
;; (global-linum-mode t)
(global-display-line-numbers-mode t)
(global-hl-todo-mode 1)
(magit-todos-mode 1)
(dired-async-mode t)
(reverse-im-activate "russian-computer")
(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates t
      history-delete-duplicates t
      sentence-end-double-space nil
      apropos-do-all t
      dired-dwim-target t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      frame-title-format "%b"
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
      slime-description-autofocus t
      shell-pop-window-size 50
      shell-pop-window-position "bottom"
      magit-section-visibility-indicator (quote (magit-fringe-bitmap+ . magit-fringe-bitmap-))
      magit-todos-auto-group-items 1000
      magit-diff-buffer-file-locked t
      magit-log-arguments '("-n64" "--graph" "--decorate" "--patch")
      xref-show-xrefs-function #'ivy-xref-show-xrefs
      compilation-scroll-output t
      hl-todo-keyword-faces '(("TODO" . "#cc9393")
                              ("FAIL" . "#8c5353")
                              ("NOTE" . "#d0bf8f")
                              ("KLUDGE" . "#d0bf8f")
                              ("HACK" . "#d0bf8f")
                              ("TEMP" . "#d0bf8f")
                              ("FIXME" . "#cc9393")
                              ("DONE" . "#98fb98")
                              ("MAYBE" . "#d0bf8f")))

;;* persistence
(setq save-place-file (concat user-emacs-directory "places"))
(save-place-mode t)

(setq recentf-max-saved-items 500)
(recentf-mode t)

(require 'savehist)
(setq savehist-save-minibuffer-history t)
(setq savehist-additional-variables
      (append savehist-additional-variables
              '(kill-ring
                search-ring
                regexp-search-ring
                last-kbd-macro
                kmacro-ring
                shell-command-history)))
(savehist-mode)



;;* shell-pop
(shell-pop--set-universal-key 'shell-pop-universal-key "<f12>")
(shell-pop--set-shell-type 'shell-pop-shell-type  '("vterm" "*vterm*"
                                                    (lambda nil
                                                      (vterm shell-pop-term-shell))))
(define-key vterm-mode-map (kbd "<f12>") nil)

;;
(add-hook 'prog-mode-hook (lambda () (setq-local show-trailing-whitespace t)))

(defalias 'yes-or-no-p 'y-or-n-p)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'magit-clean 'disabled nil)


;;* magit fringe hacks
(defun magit-status-set-wide-fringe (&optional arg)
  (display-line-numbers-mode -1)
  (set-window-fringes nil 11 5))

(add-hook 'magit-status-sections-hook #'magit-status-set-wide-fringe)
(add-hook 'magit-refs-mode-hook #'magit-status-set-wide-fringe)
(add-hook 'magit-revision-sections-hook #'magit-status-set-wide-fringe)

;;* custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;;* modeline
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

;;* show-paren-mode
(show-paren-mode 1)
(setq show-paren-priority -1
      show-paren-delay 0)

;;* minibuffer
(setq resize-mini-windows t
      max-mini-window-height 0.4
      minibuffer-eldef-shorten-default t
      enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)


;;* kludges
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

;;** rename-file-and-buffer
;; Stolen from prelude.
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: ")))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;;** Toggle delete-other-windows
(defvar *delete-other-windows-prev-configurations* (make-hash-table)
  "Frame => window configuration prior to `delete-other-windows-toggle' call.")

(defun delete-other-windows--delete-frame-hook (frame)
  "Remove frame from `*delete-other-windows-prev-configurations*' if it is deleted."
  (remhash frame *delete-other-windows-prev-configurations*))

(add-to-list 'delete-frame-functions #'delete-other-windows--delete-frame-hook)

(defun delete-other-windows-toggle ()
  "Like `delete-other-windows', but restores previous window configuration
if there is a sole window."
  (interactive)
  (let ((frame (window-frame)))
    (if (cdr (window-list))
        (let ((winconf (current-window-configuration)))
          (setf (gethash frame *delete-other-windows-prev-configurations*)
                winconf)
          (call-interactively #'delete-other-windows))
      (when-let ((winconf (gethash frame *delete-other-windows-prev-configurations*)))
        (set-window-configuration winconf)))))

(global-set-key [remap delete-other-windows] 'delete-other-windows-toggle)

;;** edit-indirect
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

;;** tab competion and hippie-expand
(setq tab-always-indent 'complete)
;; Distinguish C-i and keyboard tab key
(global-set-key (kbd "C-i") 'hippie-expand)
(global-set-key (kbd "<tab>") 'indent-for-tab-command)
;; Global <tab> overrides minor-mode TAB binding(?) TODO: look into it
(define-key magit-mode-map (kbd "<tab>") 'magit-section-toggle)

;;** with-minor-mode-overriding - locally override minor mode keymap
(cl-defmacro with-minor-mode-map-overriding ((new-map minor-mode) &body body)
  "Create a keymap locally overriding MINOR-MODE keymap and bind it to NEW-MAP inside BODY"
  (let ((old-map (gensym)))
    `(when-let* ((,old-map (alist-get ',minor-mode minor-mode-map-alist))
                 (,new-map ;; (copy-keymap ,old-map)
                           (make-sparse-keymap)))
       (set-keymap-parent ,new-map ,old-map)
       (make-local-variable 'minor-mode-overriding-map-alist)
       (setf (alist-get ',minor-mode minor-mode-overriding-map-alist) ,new-map)
       ,@body)))

;;** narrowing
(setq narrow-to-defun-include-comments t)
(put 'narrow-to-page 'disabled nil)

(defun narrow-dwim (nest)
  (interactive "P")
  "Widen if buffer is narrowed and no prefix arg is supplied.
Else if region is active - narrow-to-region.
Else narrow-to-defun."
  ;; MAYBE: save nested narrows
  ;; MAYBE: restore screen position after `widen'
  ;; MAYBE: support `org-narrow-to-block'
  (cond ((and (not nest) (buffer-narrowed-p))
         (widen))
        ((region-active-p)
         (call-interactively #'narrow-to-region))
        (t (call-interactively #'narrow-to-defun))))

(global-set-key (kbd "C-x C-n") 'narrow-dwim)

;;* magit
(defun magit-forward-dwim ()
  (interactive)
  (if (region-active-p)
      (magit-next-line)
    (magit-section-forward)))

(defun magit-backward-dwim ()
  (interactive)
  (if (region-active-p)
      (magit-previous-line)
    (magit-section-backward)))

;;* dired-jump-other-frame
(defun dired-jump-other-frame (&optional file-name)
  "Like \\[dired-jump] (`dired-jump') but in other frame."
  (interactive
   (list (and current-prefix-arg
	      (read-file-name "Jump to Dired file: "))))
  (let ((pop-up-frames t))
    (dired-jump t file-name)))

;;* org
(setq org-default-notes-file (concat org-directory "/notes.org"))

(advice-add 'org-archive-default-command :after #'org-save-all-org-buffers)

(global-set-key (kbd "<f6>") 'counsel-org-capture)


;;* keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-<tab>") 'completion-at-point)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; experimental
;; (global-set-key (kbd "C-x C-o") 'ace-window)
(global-set-key (kbd "C-x C-o") 'ignore)
(global-set-key (kbd "C-x C-c") 'ace-window)
(global-set-key (kbd "C-x C-SPC") 'other-window)
(global-set-key (kbd "M-c") 'ace-window)
;;;
(global-set-key (kbd "C-x 5 5") 'window-as-frame)
(global-set-key (kbd "C-x M-w") 'kill-buffer-file-name)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)
(define-key grep-mode-map (kbd "C-x C-j") 'compilation-to-dired)
(define-key compilation-mode-map (kbd "C-x C-j") 'compilation-to-dired)
(global-set-key (kbd "C-x 4 j") 'dired-jump-other-window)
(global-set-key (kbd "C-x 5 j") 'dired-jump-other-frame)

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
(define-key dired-mode-map (kbd "r") 'dired-rsync)
(define-key dired-mode-map (kbd ")") 'dired-git-info-mode)
(define-key dired-mode-map (kbd "C-x g") 'magit-file-prefix-map)


;;** magit
(define-key magit-file-mode-map (kbd "C-x =") 'magit-diff-buffer-file)
(define-key magit-file-mode-map (kbd "C-x G") 'magit-file-dispatch)
(define-key magit-mode-map (kbd "C-c C-l") 'magit-toggle-buffer-lock)
(define-key magit-mode-map (kbd "j") 'magit-forward-dwim)
(define-key magit-mode-map (kbd "k") 'magit-backward-dwim)
(define-key magit-status-mode-map (kbd "j") 'magit-forward-dwim)
(define-key magit-status-mode-map (kbd "k") 'magit-backward-dwim)
(define-key magit-todos-section-map (kbd "j") 'magit-forward-dwim)
(define-key magit-todos-section-map (kbd "k") 'magit-backward-dwim)
(define-key magit-diff-mode-map (kbd "j") 'magit-forward-dwim)
(define-key magit-diff-mode-map (kbd "k") 'magit-backward-dwim)
(define-key magit-status-mode-map (kbd "C-k") 'magit-discard)
(define-key magit-diff-mode-map (kbd "C-k") 'magit-discard)

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

;; Keymap for random useful commands. MAYBE make it a hydra
(define-prefix-command 'cantrips-prefix-map)
(global-set-key (kbd "M-z") 'cantrips-prefix-map)
(define-key cantrips-prefix-map (kbd "s") 'string-edit-at-point)
(define-key cantrips-prefix-map (kbd "p") 'counsel-package)
(define-key cantrips-prefix-map (kbd "r") 'rename-file-and-buffer)
(define-key cantrips-prefix-map (kbd "d") 'describe-text-properties)
(define-key cantrips-prefix-map (kbd "f") 'describe-face)
(define-key cantrips-prefix-map (kbd "i") 'ielm)
(define-key cantrips-prefix-map (kbd "/") 'rg)
(define-key cantrips-prefix-map (kbd "b") 'counsel-descbinds)
(define-key cantrips-prefix-map (kbd "e") 'toggle-debug-on-error)

(ace-link-setup-default (kbd "C-f"))
(dolist (keymap (list help-mode-map package-menu-mode-map compilation-mode-map grep-mode-map))
  (define-key keymap (kbd "C-f") 'ace-link))

;;** Helpful
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h o") #'helpful-symbol)
