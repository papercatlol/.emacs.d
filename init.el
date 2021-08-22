;; -*- lexical-binding: t -*-

;; remove ui
(setq default-frame-alist
      '((menu-bar-lines 0)
        (tool-bar-lines 0)
        (vertical-scroll-bars)))

;; packages
(require 'package)

;; Precompute activation actions to speed up startup.
(setq package-quickstart t)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

;;* git-controlled packages
(let ((default-directory (expand-file-name "git" user-emacs-directory)))
  (when (file-exists-p default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

(server-start)

;;** GC hacks
(defun gc-with-time ()
  (let ((time (current-time)))
    (garbage-collect)
    (unless (or (minibuffer-window-active-p (selected-window))
                (> (window-height (minibuffer-window)) 1))
      (message "GC took %.06f sec" (float-time (time-since time))))))

(setq gc-cons-threshold #x40000000) ; 1GB

(defvar gc-timer (run-with-idle-timer 30 t #'gc-with-time))

;;* theme
(load-theme 'quasi-monochrome t)
(setq custom--inhibit-theme-enable nil)

;;* fill-column-indicator
(setq-default fill-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;;;
(require 'avy)
(require 'ace-window)
(require 'async)
(require 'beginend)
(require 'delsel)
(require 'dired-subtree)
(require 'expand-region)
(require 'exec-path-from-shell)
(require 'helpful)
(require 'hl-todo)
(require 'ivy-xref)
(require 'multiple-cursors)
(require 'paredit)
(require 'pcmpl-args)
(require 'pcmpl-git)
(require 'pdf-tools)
(require 'uniquify)
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
(require 'configure-org)
(require 'configure-email)
(require 'configure-git)
(require 'configure-go-lsp)
(require 'configure-highlight)
(require 'configure-isearch)
(require 'configure-ivy)
(require 'counsel-ripgrep)
;; (require 'configure-go)
(require 'configure-lisp)
(require 'configure-python)

;;* ./local-elisp - private/work configuration
(add-to-list 'load-path (expand-file-name "local-elisp" user-emacs-directory))
(require 'local-elisp-init)

;;* global minor modes
(delete-selection-mode 1)
(global-hl-todo-mode 1)
(tooltip-mode -1)
(global-so-long-mode 1)

;;* async
(dired-async-mode t)
(async-bytecomp-package-mode t)

;;* reverse-im
(require 'reverse-im)
(set-keymap-parent function-key-map
                   (make-composed-keymap
                    (list (reverse-im--im-to-keymap "russian-computer")
                          (reverse-im--im-to-keymap "ukrainian-computer"))))

;;* setq variables
(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates t
      sentence-end-double-space nil
      apropos-do-all t
      dired-dwim-target t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      frame-title-format "%b"
      expand-region-fast-keys-enabled nil
      er--show-expansion-message t
      inhibit-startup-message t
      wgrep-auto-save-buffer t
      uniquify-buffer-name-style 'forward
      ;; ace-window
      aw-scope 'frame
      aw-background (display-graphic-p)
      aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      ;; avy
      ;; I'd like to use 'all-frames, but this doesn't work properly with
      ;; i3wm tabbed layout.
      avy-all-windows t
      avy-style 'pre ;; 'de-bruijn
      avy-keys (list ?f ?c ?d ?g ?s ?a ?e ?v
                     ?F ?C ?D ?G ?S ?A ?E ?V)
      ;;
      comment-padding ""
      view-read-only nil
      slime-description-autofocus t
      xref-show-xrefs-function #'ivy-xref-show-xrefs
      compilation-scroll-output t
      initial-major-mode 'emacs-lisp-mode
      use-dialog-box nil
      comint-buffer-maximum-size 8192
      ;; fringe-mode '((4 . 4))
      fringe-mode '((8 . 0))
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
;;** save-place
(setq save-place-file (concat user-emacs-directory "places"))
(save-place-mode t)

;;** recentf
(setq recentf-max-saved-items 500)
(recentf-mode t)

(defun recentf-save-list-silently ()
  (let ((inhibit-message t)
        (save-silently t))
    (recentf-save-list)))

(defvar recentf-save-list-timer
  (run-with-idle-timer 120 t #'recentf-save-list-silently))

;;** savehist
(require 'savehist)
(setq history-length t)
(setq history-delete-duplicates t)
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

;;** backups
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq version-control t)
(setq delete-old-versions 'please-dont)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;;* equake
(require 'configure-equake)

;;
(add-hook 'prog-mode-hook (lambda () (setq-local show-trailing-whitespace t)))
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

(defalias 'yes-or-no-p 'y-or-n-p)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;* custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;;* modeline
(defvar minor-mode-lighters
  '((paredit-mode " (P)")
    (outline-minor-mode "")
    (lispyville-mode "")
    (auto-revert-mode "")
    (undo-tree-mode "")
    (ivy-mode "")
    (slime-mode " slime")
    (anzu-mode "")
    (elisp-slime-nav-mode "")
    (eldoc-mode "")
    (explain-pause-mode "")
    (page-break-lines-mode "")
    (org-roam-mode "")
    ))

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
                    '(:eval (string-trim evil-mode-line-tag))
                    '(:eval (when slime-mode (concat " " (slime-current-package))))
                    '(vc-mode vc-mode)
                    " ["
                    '(:eval mode-name)
                    "]"
                    ;; "%f -"
                    '(:eval (cleaner-minor-modes))
                    '(:eval org-mode-line-string)
                    " %-"))

;;* title format: list all visible windows in frame title
;; We don't use `frame-title-format' because it doesn't work for
;; frames in i3wm title layout for some reason(because they aren't visible?).
(defun set-frame-title-fn ()
  (let* ((window-names
           (loop for w in (window-list)
                 for b = (window-buffer w)
                 unless (minibufferp b)
                   collect (format "[%s]" (buffer-name b))))
         (title (string-join window-names " ")))
    (set-frame-parameter nil 'title title)))

(add-hook 'window-configuration-change-hook #'set-frame-title-fn)

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

;;* pdf-tools
;;(pdf-tools-install)

(define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
(define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)

(add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)

;;* exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;* explain-pause-mode
(add-to-list 'load-path (expand-file-name "custom/explain-pause-mode/" user-emacs-directory))
;; (require 'explain-pause-mode)
;; (explain-pause-mode 1)

;;* helpful
(add-hook 'helpful-mode-hook #'visual-line-mode)

(defvar helpful-last-buffer nil)

(cl-defun toggle-help-window ()
  (interactive)
  (or (loop for w in (window-list)
            for b = (window-buffer w)
            when (eq 'helpful-mode (buffer-local-value 'major-mode b))
              do (progn (setq helpful-last-buffer b)
                        (delete-window w)
                        (return t)))
      (and helpful-last-buffer (display-buffer helpful-last-buffer))))

(global-set-key (kbd "C-h h") 'toggle-help-window)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "<f2>") #'helpful-key)

;;** override default describe functions
(advice-add 'describe-function :override #'helpful-function)
(advice-add 'describe-variable :override #'helpful-variable)
(advice-add 'describe-command  :override #'helpful-callable)
(advice-add 'describe-key      :override #'helpful-key)
(advice-add 'describe-symbol   :override #'helpful-symbol)

;;* windows
;;** display-buffer-alist
;; Inspired by: https://protesilaos.com/codelog/2020-01-07-emacs-display-buffer/
(progn
  ;; bottom side window
  (map-put
   display-buffer-alist
   (rx "*" (or "Backtrace" "Warnings" "Compile-Log" "Messages") "*")
   '((display-buffer-in-side-window)
     (window-height . 0.16)
     (side . bottom)
     (slot . 1)
     (window-parameters . ((no-other-window . t))))
   #'string=)
  (map-put
   display-buffer-alist
   (rx (or (and (? "e") "shell") "vterm" "EQUAKE[") (* any))
   '((display-buffer-in-side-window)
     (window-height . 0.2)
     (side . bottom)
     (slot . 0)
     (window-parameters . ((no-other-window . t))))
   #'string=)
  ;; right side window
  (map-put
   display-buffer-alist
   "\\*Faces\\*"
   '((display-buffer-in-side-window)
     (window-width . 0.3)
     (side . right)
     (slot . 0)
     (window-parameters . ((no-other-window . t))))
   #'string=)
  ;; left side window
  (map-put
   display-buffer-alist
   (rx (or "*help" "*info" "*apropos" "*man" "*woman") (* any))
   '((display-buffer-in-side-window)
     (window-width . 0.2)
     (side . left)
     (slot . 0)
     (window-parameters . ((no-other-window . t))))
   #'string=)
  (map-put
   display-buffer-alist
   "\\*Custom.*"
   '((display-buffer-in-side-window)
     (window-width . 0.3)
     (side . left)
     (slot . 1)
     (window-parameters . ((no-other-window . t))))
   #'string=))

(setq even-window-sizes 'height-only)

(add-hook 'help-mode-hook #'visual-line-mode)
(add-hook 'Info-mode-hook #'visual-line-mode)
(add-hook 'apropos-mode #'visual-line-mode)
(add-hook 'custom-mode-hook #'visual-line-mode)
(add-hook 'woman-mode-hook #'visual-line-mode)
(add-hook 'man-mode-hook #'visual-line-mode)

(global-set-key (kbd "<f9>") 'window-toggle-side-windows)

;;** balance-windows-horizontally
(defun balance-windows-horizontally (&optional window-or-frame)
  "Same as `balance-windows', but only do it horizontally.
With prefix arg, call `balance-windows-area'."
  (interactive)
  (if current-prefix-arg
      (call-interactively #'balance-windows-area)
    (let* ((window
	     (cond
	       ((or (not window-or-frame)
		    (frame-live-p window-or-frame))
	        (frame-root-window window-or-frame))
	       ((or (window-live-p window-or-frame)
		    (window-child window-or-frame))
	        window-or-frame)
	       (t
	        (error "Not a window or frame %s" window-or-frame))))
	   (frame (window-frame window)))
      ;; Balance horizontally.
      (window--resize-reset (window-frame window) t)
      (balance-windows-1 window t)
      (when (window--resize-apply-p frame t)
        (window-resize-apply frame t)
        (window--pixel-to-total frame t)
        (run-window-configuration-change-hook frame)))))

(global-set-key (kbd "C-x +") 'balance-windows-horizontally)

;;** fit-window-to-buffer
(setq fit-window-to-buffer-horizontally t)

(global-set-key (kbd "C-x -") 'fit-window-to-buffer)

;;** winner-mode
(setq winner-dont-bind-my-keys t)
(winner-mode 1)

(global-set-key (kbd "C-S-q") 'delete-window)
(global-set-key (kbd "C-M-1") 'winner-undo)
(global-set-key (kbd "C-M-2") 'winner-redo)
(global-set-key (kbd "C-M-3") 'rotate-frame-anticlockwise)

;;** window-as-frame
(defun window-as-frame ()
  "Pop current window as a new frame."
  (interactive)
  (let ((frame (make-frame)))
    (delete-window (get-buffer-window (current-buffer)))
    (select-frame frame)))

(define-key ctl-x-5-map "5" 'window-as-frame)

;;** toggle *messages* buffer
(defun toggle-echo-area-messages ()
  (interactive)
  (if-let ((win (get-buffer-window (messages-buffer))))
      (quit-window nil win)
    (view-echo-area-messages)))

(define-key help-map "e" 'toggle-echo-area-messages)

;;* kill-buffer-dwim
(defun kill-buffer-dwim (select)
  "With prefix arg behave like regular `kill-buffer', otherwise
immediately kill current buffer."
  (interactive "P")
  (if select
      (call-interactively #'kill-buffer)
    (let ((name (buffer-name)))
      (kill-buffer)
      (message "Killed buffer %s" name))))

(global-set-key [remap kill-buffer] 'kill-buffer-dwim)

;;* kludges
;; Fix for i3wm(not gaps) and emacs 26 where display doesn't refresh when switching
;; to an existing frame in tabbed or stacked layout.
(defun make-frame-visible-advice (&rest args)
  (make-frame-visible))
(advice-add 'select-frame-set-input-focus :after #'make-frame-visible-advice)

(defun kill-buffer-file-name ()
  "Add current buffer file name to kill ring. With prefix arg,
return file name without directory."
  (interactive)
  (when-let ((filename (if (eq major-mode 'dired-mode)
                           default-directory
                         (if current-prefix-arg
                             (file-name-nondirectory (buffer-file-name))
                           (buffer-file-name)))))
    (kill-new filename)
    (message "%s" filename)))

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

(defvar he-need-paredit-fix? t)

(defun he-paredit-fix (str &optional trans-case)
  "Remove extra paren when expanding line in paredit.
https://www.emacswiki.org/emacs/HippieExpand#toc9"
  (and he-need-paredit-fix?
       paredit-mode
       (equal "(" (substring he-search-string 0 1))
       (equal ")" (substring str -1))
       (looking-at-p ")")
       (backward-delete-char 1)))

(advice-add #'he-substitute-string :after #'he-paredit-fix)

;;** tab key hacks
(setq tab-always-indent 'complete)

;; Distinguish C-i and keyboard tab key
(define-key input-decode-map (kbd "C-i") [C-i])

;;(global-set-key (kbd "<C-i>") 'hippie-expand)
(global-set-key (kbd "TAB") 'indent-for-tab-command)

;;** with-minor-mode-overriding - locally override minor mode keymap
(cl-defmacro with-minor-mode-map-overriding ((new-map minor-mode) &body body)
  "Create a keymap locally overriding MINOR-MODE keymap and bind it to NEW-MAP inside BODY"
  (declare (indent defun))
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

;;* dired-jump-other-frame
(defun dired-jump-other-frame (&optional file-name)
  "Like \\[dired-jump] (`dired-jump') but in other frame."
  (interactive
   (list (and current-prefix-arg
	      (read-file-name "Jump to Dired file: "))))
  (let ((pop-up-frames t))
    (dired-jump t file-name)))

;;* diff-buffer-with-file
(defun diff-current-buffer-with-file (prompt)
  (interactive "P")
  (let ((buffer (if prompt
                    (get-buffer
                     (completing-read "Buffer: "
                                      (loop for b in (buffer-list)
                                            when (buffer-file-name b)
                                                 collect it)))
                  (current-buffer))))
    (diff-buffer-with-file buffer)))

;;* dired-quick-sort
(require 'dired-quick-sort)
(dired-quick-sort-setup)

(setq dired-listing-switches "-laGh1v --group-directories-first")

;;* dired-rsync
;; TODO: move dired stuff to a separate file
(require 'dired-rsync)

(define-key dired-mode-map (kbd "r") 'dired-rsync)

;;** show rsync progress in modeline
(with-eval-after-load 'dired+
  (defun diredp-rsync-in-mode-name ()
    (unless (cl-member '(:eval dired-rsync-modeline-status) mode-name :test #'equal)
      (setq mode-name
            (append mode-name
                    `((:eval dired-rsync-modeline-status))))))

  (advice-add 'diredp-nb-marked-in-mode-name :after #'diredp-rsync-in-mode-name))

;;* enable read-only for emacs sources
(dir-locals-set-class-variables
 'emacs-src
 '((nil . ((buffer-read-only . t)
           (show-trailing-whitespace . nil)
           (tab-width . 8)
           (eval . (whitespace-mode -1))))))

(dir-locals-set-directory-class "/usr/local/src/emacs" 'emacs-src)
(dir-locals-set-directory-class "/usr/local/share/emacs" 'emacs-src)
(dir-locals-set-directory-class "/usr/share/emacs" 'emacs-src)

;;* yasnippet
(require 'yasnippet)

(yas-global-mode)

;;** use hippie-expand instead of TAB
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

;; (add-to-list 'hippie-expand-try-functions-list
;;              'yas-hippie-try-expand)

(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-expand-dabbrev-visible
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-list
        try-expand-line
        try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;;** hippie-expand: select from all completions
;; https://www.emacswiki.org/emacs/HippieExpand#toc11
(defun hippie-expand--all (&optional hippie-expand-function)
  "Return the full list of possible completions generated by `hippie-expand'.
    The optional argument can be generated with
    `make-hippie-expand-function'."
  (let ((this-command 'hippie-expand--all)
        (last-command last-command)
        (buffer-modified (buffer-modified-p))
        (hippie-expand-function (or hippie-expand-function 'hippie-expand))
        (he-need-paredit-fix? nil)
        (expansions nil))
    (flet ((ding)) ; avoid the (ding) when hippie-expand exhausts its options.
      (while (progn
               (funcall hippie-expand-function nil)
               (setq last-command 'hippie-expand--all)
               ;; Expanders like to reset markers, so we save them.
               (push (list (car he-tried-table)
                           he-search-string
                           (copy-marker he-string-beg)
                           (copy-marker he-string-end))
                     expansions)
               (not (equal he-num -1)))))
    ;; Evaluating the completions modifies the buffer, however we will finish
    ;; up in the same state that we began.
    (set-buffer-modified-p buffer-modified)
    (reverse expansions)))

(defun hippie-expand-completion ()
  (interactive)
  (when-let* ((expansions (hippie-expand--all))
              ;; MAYBE: use completion-in-region; see `dabbrev-completion' for reference
              (expansion (completing-read "Hippie expand: " (mapcar #'car expansions)))
              (metadata (alist-get expansion expansions nil nil #'string=))
              (he-search-string (car metadata))
              (he-string-beg (second metadata))
              (he-string-end (third metadata)))
    (he-substitute-string expansion t)))

(global-set-key [remap dabbrev-completion] 'hippie-expand-completion)

;; TODO: figure out how to add snippets to completion candidates
;; and how `completion-extra-properties' work
;; (defun yas-completion-at-point ()
;;   (let ((bounds (bounds-of-thing-at-point 'symbol)))
;;     (list (or (car bounds) (point))
;;           (point)
;;           (yas--table-hash (car (yas--get-snippet-tables)))
;;           (list :exclusive 'no :exit-function #'yas--completion-exit-function))))

;; (defun yas--completion-exit-function (string status)
;;   (yas-expand))


;;* copy for reddit
(defun copy-for-reddit ()
  "Copy and indent active region or current defun with 4 spaces."
  (interactive)
  (when-let* ((bounds (if (region-active-p)
                          (cons (region-beginning) (region-end))
                        (bounds-of-thing-at-point 'defun)))
              (text (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (setq deactivate-mark t)
    (kill-new (replace-regexp-in-string "^" "    " text))
    (message "Copied %d lines." (count-lines (car bounds) (cdr bounds)))))

;;* pulse-cursor
(cl-defun pulse-cursor (&key (width 1) (face 'cursor) (delay .01))
  (let* ((pulse-delay delay)
         (pos (point))
         (beg (max (line-beginning-position) (- pos width)))
         (end (min (line-end-position) (+ pos width))))
    (pulse-momentary-highlight-region beg end face)))

(cl-defun pulse-current-line (&key (face 'cursor) (delay .01))
  (let ((pulse-delay delay))
    (pulse-momentary-highlight-one-line (point) face)))

(defun pulse-cursor-after (&rest args)
  (pulse-cursor))

(defun pulse-current-line-after (&rest args)
  (pulse-current-line))

(advice-add 'other-window :after #'pulse-current-line-after)
(advice-add 'ace-window :after #'pulse-current-line-after)

;;*
(define-key package-menu-mode-map (kbd "j") 'next-line)
(define-key package-menu-mode-map (kbd "k") 'previous-line)

;;* iedit
(defun iedit-mode* ()
  "If iedit-mode is active, restrict to current region or defun,
otherwise activate iedit-mode."
  (interactive)
  (if (bound-and-true-p iedit-mode)
      (if (region-active-p)
          (iedit-restrict-region (region-beginning) (region-end))
        (iedit-restrict-function nil))
    (iedit-mode)))

(global-set-key (kbd "C-;") 'iedit-mode*)

;;* outline-mode, bicycle-mode
(require 'bicycle)

(add-hook 'prog-mode-hook #'outline-minor-mode)

;; change ellipsis to something more distinct
(set-display-table-slot standard-display-table
                        'selective-display
                        (string-to-vector " >"))

(defun bicycle-cycle-body ()
  "Cycle between showing and hiding function bodies."
  (interactive)
  (if (eq last-command 'outline-hide-body)
      (outline-show-all)
    (outline-hide-body)
    (setq this-command 'outline-hide-body)))

(defun bicycle-cycle* (&optional global)
  "With prefix arg cycle function bodies, otherwise cycle
current entry."
  (interactive "P")
  (if global
      (bicycle-cycle-body)
    (save-excursion
     ;; Fixes jumping to the first heading in a file from function body.
     (outline-back-to-heading)
     (bicycle-cycle-local))))

(define-key prog-mode-map (kbd "C-x C-<tab>") 'bicycle-cycle*)

;; fix a weird bug where `outline-regexp' matches a "(c)" at the start of the file
(defun bicycle--level-advice (fn &rest args)
  (if-let ((level (apply fn args)))
      level
    (save-excursion
     (and (re-search-forward outline-regexp nil t)
          (bicycle--level)))))
(advice-add 'bicycle--level :around #'bicycle--level-advice)

;;* tramp
(setq-default tramp-verbose 5)

(setq tramp-default-method "ssh" ; "ssh"/"scp"
      )

;; Try to speed things up
(setq remote-file-name-inhibit-cache nil)
(setq tramp-completion-reread-directory-timeout nil)

;; we use magit anyway, so this shouldn't change anything in theory(?)
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; eshell-tramp module
(require 'em-tramp)

;;* link-hint
(defvar link-hint-avy-all-windows t)
(defvar link-hint-avy-all-windows-alt 'all-frames)
(global-set-key (kbd "C-c C-SPC") 'link-hint-open-link)
(global-set-key (kbd "C-c o") 'link-hint-open-link)

;;** bug-reference-mode
(with-eval-after-load 'link-hint
  (defun bug-reference--find-overlay (overlays)
    (car (member-if (lambda (overlay)
                      (eq 'bug-reference (overlay-get overlay 'category)))
                    overlays)))

  (cl-defun link-hint--next-bug-reference-button (&optional bound)
    (when-let* ((bounds (link-hint--at-bug-reference-button))
                (end (1+ (cdr bounds))))
      (when (and bound (> end bound))
        (return-from link-hint--next-bug-reference-button nil))
      (goto-char end))

    (when-let* ((overlays (reverse (overlays-in (point) (or bound (window-end)))))
                (overlay (bug-reference--find-overlay overlays)))
      (goto-char (overlay-start overlay))))

  (defun link-hint--at-bug-reference-button ()
    (when-let ((overlay (bug-reference--find-overlay (overlays-at (point)))))
      (cons (overlay-start overlay) (overlay-end overlay))))

  (defun link-hint--bug-reference-push-button ()
    (bug-reference-push-button (point)))

  (link-hint-define-type
   'bug-reference-button
   :next #'link-hint--next-bug-reference-button
   :at-point-p #'link-hint--at-bug-reference-button
   :vars '(bug-reference-mode bug-reference-prog-mode)
   :open #'link-hint--bug-reference-push-button
   :open-multiple t
   :open-message "Opened"
   :copy #'kill-new)

  (add-to-list 'link-hint-types 'link-hint-bug-reference-button))

;;* page-break-lines
(when (fboundp 'page-break-lines-mode)
  (add-hook 'prog-mode-hook 'page-break-lines-mode)
  (add-hook 'help-mode-hook 'page-break-lines-mode)
  (add-hook 'compilation-mode-hook 'page-break-lines-mode)
  (add-hook 'man-mode-hook 'page-break-lines-mode)
  )

;;* rename-buffer
(defun rename-buffer-dwim (newname &optional unique)
  "Like `rename-buffer', but suggest current buffer name."
  (interactive
   (list (read-string "Rename buffer: " (buffer-name)) current-prefix-arg))
  (rename-buffer newname unique))

;; TODO: group all C-x bindings and use `ctl-x-map' instead of literal C-x
(define-key ctl-x-map (kbd "C-r") 'rename-buffer-dwim)

;;* TODO: hydra-resize-window, map M-_ & M-+ or smth to shrinking/growing windows

;;* TODO: helm-make: add cd(counsel-cd?) binding; same for cd to git root
(setq helm-make-completion-method 'ivy)

;;* scrolling
(require 'smooth-scroll)

(setq smooth-scroll/vscroll-step-size 4)

(cl-defun smooth-scroll/scroll-n (&optional (n 8))
  ((smooth-scroll/.vscroll-aux )))

(cl-defun smooth-scroll/scroll-up-8 (&optional (arg 8))
  (interactive)
  (smooth-scroll/.vscroll-aux
   (if current-prefix-arg
       (prefix-numeric-value)
     arg)
   t))

(cl-defun smooth-scroll/scroll-up-16 (&optional (arg 16))
  (interactive)
  (let ((smooth-scroll/vscroll-step-size 8))
    (smooth-scroll/.vscroll-aux
     (if current-prefix-arg
         (prefix-numeric-value)
       arg)
     t)))

(cl-defun smooth-scroll/scroll-down-8 (&optional (arg 8))
  (interactive)
  (smooth-scroll/.vscroll-aux
   (if current-prefix-arg
       (prefix-numeric-value)
     arg)
   nil))

(cl-defun smooth-scroll/scroll-down-16 (&optional (arg 16))
  (interactive)
  (let ((smooth-scroll/vscroll-step-size 8))
    (smooth-scroll/.vscroll-aux
     (if current-prefix-arg
         (prefix-numeric-value)
       arg)
     nil)))

(global-set-key (kbd "M-j") 'smooth-scroll/scroll-up-8)
(global-set-key (kbd "M-k") 'smooth-scroll/scroll-down-8)
(global-set-key (kbd "C-M-j") 'smooth-scroll/scroll-up-16)
(global-set-key (kbd "C-M-k") 'smooth-scroll/scroll-down-16)

;;** ibuffer
;; old M-j: ibuffer-jump-to-filter-group
(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "M-j") 'smooth-scroll/scroll-up-8)
  (define-key ibuffer-mode-map (kbd "M-k") 'smooth-scroll/scroll-down-8))

;;* eww
(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "C-c C-n") 'eww-next-url)
  (define-key eww-mode-map (kbd "C-c C-p") 'eww-previous-url)
  (define-key eww-mode-map (kbd "n") nil)
  (define-key eww-mode-map (kbd "H") 'eww-back-url)
  (define-key eww-mode-map (kbd "L") 'eww-forward-url)
  (define-key eww-mode-map (kbd "h") nil)
  (define-key eww-mode-map (kbd "l") nil)
  ;; TODO: counsel-eww-list-histories
  (define-key eww-mode-map (kbd "C-c h") 'eww-list-histories))

;;* which-key
(setq which-key-lighter ""
      which-key-show-transient-maps nil)
(which-key-mode)

;;* keyfreq
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(setq keyfreq-excluded-commands '(self-insert-command))

;;* dired-display-file-and-next/prev-line
;; TODO: group dired-related configuration
(defun dired-display-file-and-next-line (&optional n)
  (interactive "p")
  (dired-display-file)
  (next-line (or n 1)))

(defun dired-display-file-and-prev-line (&optional n)
  (interactive "p")
  (dired-display-file-and-next-line (- (or n 1))))

(define-key dired-mode-map (kbd "M-o") 'dired-display-file)
(define-key dired-mode-map (kbd "M-m") 'dired-display-file)
(define-key dired-mode-map (kbd "f") 'dired-display-file)
(define-key dired-mode-map (kbd "M-n") 'dired-display-file-and-next-line)
(define-key dired-mode-map (kbd "M-p") 'dired-display-file-and-prev-line)

;;* shrink-whitespace
(global-set-key (kbd "C-c SPC") 'shrink-whitespace)
(global-set-key (kbd "C-c S-SPC") 'grow-whitespace-around)

;;* string-edit
(require 'string-edit)

(add-to-list 'load-path (expand-file-name "custom/string-edit-regexp/" user-emacs-directory))

(defun se/string-at-point/escape-override (quote)
  "Same as orig, but don't escape newlines."
  (save-excursion
   (se/escape "\\")
   (se/escape quote)))
(advice-add 'se/string-at-point/escape :override #'se/string-at-point/escape-override)

(require 'string-edit-regexp)

(define-key string-edit-mode-map (kbd "C-c C-r") 'string-edit-toggle-regexp-mode)

;;* ansi-colors in compilation buffers
;; https://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;* kbd-helper
(defun kbd-helper ()
  "Read a key sequence and return it in a format suitable for `kbd'.
If called interactively, quote and insert it."
  (interactive)
  (let ((key (key-description (read-key-sequence "Key Sequence: "))))
    (if (interactive-p)
        (insert (format "\"%s\"" key))
      key)))

;;* json-pretty-print-dwim
(defun json-pretty-print-dwim ()
  "Call `json-pretty-print' on the region if it is active or on
the whole buffer otherwise."
  (interactive)
  (call-interactively
   (if (region-active-p)
       #'json-pretty-print
     #'json-pretty-print-buffer)))

;;* flyspell
;; TODO: ivy wrapper for `flyspell-correct-word-before-point'
;; (maybe https://github.com/d12frosted/flyspell-correct).
;; TODO: hydra heads to remember word at point for current session/forever
(setq flyspell-issue-message-flag nil)
(setq ispell-silently-savep t)          ; Save personal dict w/o confirmation

(add-hook 'git-commit-mode-hook 'flyspell-mode)

(define-key flyspell-mode-map (kbd "C-.") nil)
(define-key flyspell-mode-map (kbd "C-;") nil)
(define-key flyspell-mode-map (kbd "C-M-i") nil)

;; Need `evil' for `evil-prev-flyspell-error'. If you don't use `evil',
;; you will have to define that function manually.
(with-eval-after-load 'evil
  (defun flyspell-hydra ()
    "Enable `flyspell-prog-mode' then flyspell current buffer and
enable `hydra-flyspell'."
    (interactive)
    (flyspell-prog-mode)
    (flyspell-buffer)
    (hydra-flyspell/body))

  (defun flyspell-disable ()
    (interactive)
    (flyspell-mode -1)
    (message "Flyspell off."))

  (defhydra hydra-flyspell ()
    "Flyspell"
    ("j" #'evil-next-flyspell-error "next error")
    ("k" #'evil-prev-flyspell-error "prev error")
    ("SPC" #'flyspell-auto-correct-word "auto-correct word")
    ("g" #'flyspell-buffer "Flyspell buffer")
    ("C-f" #'flyspell-mode "Toggle flyspell mode")
    ("t" #'flyspell-mode "Toggle flyspell mode")
    ("a" #'ace-flyspell-jump-word "Ace flyspell")
    ("q" nil "quit")
    ("C-q" #'flyspell-disable "Disable flyspell and quit" :color blue))

  (define-key flyspell-mode-map (kbd "C-c C-f") 'hydra-flyspell/body)
  (define-key prog-mode-map (kbd "C-c C-f") 'flyspell-hydra))

;;* keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-<tab>") 'completion-at-point)

;;** image
(define-key image-mode-map "j" 'image-next-file)
(define-key image-mode-map "k" 'image-previous-file)

;;** ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(defhydra hydra-ibuffer-filters (:columns 3 :exit t)
  "Filter"
  ("<tab>" #'ibuffer-exchange-filters "ibuffer-exchange-filters")
  ("RET" #'ibuffer-filter-by-mode "ibuffer-filter-by-mode")
  ("!" #'ibuffer-negate-filter "ibuffer-negate-filter")
  ("&" #'ibuffer-and-filter "ibuffer-and-filter")
  ("*" #'ibuffer-filter-by-starred-name "ibuffer-filter-by-starred-name")
  ("." #'ibuffer-filter-by-file-extension "ibuffer-filter-by-file-extension")
  ("/" #'ibuffer-filter-disable "ibuffer-filter-disable")
  ("<" #'ibuffer-filter-by-size-lt "ibuffer-filter-by-size-lt")
  (">" #'ibuffer-filter-by-size-gt "ibuffer-filter-by-size-gt")
  ("D" #'ibuffer-decompose-filter-group "ibuffer-decompose-filter-group")
  ("M" #'ibuffer-filter-by-derived-mode "ibuffer-filter-by-derived-mode")
  ("P" #'ibuffer-pop-filter-group "ibuffer-pop-filter-group")
  ("R" #'ibuffer-switch-to-saved-filter-groups "ibuffer-switch-to-saved-filter-groups")
  ("S" #'ibuffer-save-filter-groups "ibuffer-save-filter-groups")
  ("X" #'ibuffer-delete-saved-filter-groups "ibuffer-delete-saved-filter-groups")
  ("\\" #'ibuffer-clear-filter-groups "ibuffer-clear-filter-groups")
  ("a" #'ibuffer-add-saved-filters "ibuffer-add-saved-filters")
  ("b" #'ibuffer-filter-by-basename "ibuffer-filter-by-basename")
  ("c" #'ibuffer-filter-by-content "ibuffer-filter-by-content")
  ("d" #'ibuffer-decompose-filter "ibuffer-decompose-filter")
  ("e" #'ibuffer-filter-by-predicate "ibuffer-filter-by-predicate")
  ("f" #'ibuffer-filter-by-filename "ibuffer-filter-by-filename")
  ("g" #'ibuffer-filters-to-filter-group "ibuffer-filters-to-filter-group")
  ("i" #'ibuffer-filter-by-modified "ibuffer-filter-by-modified")
  ("m" #'ibuffer-filter-by-used-mode "ibuffer-filter-by-used-mode")
  ("n" #'ibuffer-filter-by-name "ibuffer-filter-by-name")
  ("o" #'ibuffer-or-filter "ibuffer-or-filter")
  ("p" #'ibuffer-pop-filter "ibuffer-pop-filter")
  ("r" #'ibuffer-switch-to-saved-filters "ibuffer-switch-to-saved-filters")
  ("s" #'ibuffer-save-filters "ibuffer-save-filters")
  ("t" #'ibuffer-exchange-filters "ibuffer-exchange-filters")
  ("v" #'ibuffer-filter-by-visiting-file "ibuffer-filter-by-visiting-file")
  ("x" #'ibuffer-delete-saved-filters "ibuffer-delete-saved-filters")
  ("|" #'ibuffer-or-filter "ibuffer-or-filter"))

(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "/") 'hydra-ibuffer-filters/body))

;; experimental
;; (global-set-key (kbd "C-x C-o") 'ace-window)
;; (global-set-key (kbd "C-x C-o") 'ignore)
(global-set-key (kbd "C-x C-c") 'ace-window)
;; (global-set-key (kbd "C-x C-SPC") 'other-window)
(global-set-key (kbd "M-c") 'ace-window)
;;;
(global-set-key (kbd "C-x M-w") 'kill-buffer-file-name)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)
(define-key grep-mode-map (kbd "C-x C-j") 'compilation-to-dired)
(define-key compilation-mode-map (kbd "C-x C-j") 'compilation-to-dired)
(define-key ctl-x-4-map (kbd "j") 'dired-jump-other-window)
(define-key ctl-x-5-map (kbd "j") 'dired-jump-other-frame)

(global-set-key (kbd "C-?") 'er/expand-region)
(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-,") 'er/contract-region)

(global-set-key (kbd "C-S-SPC") 'avy-goto-char-timer)
(global-set-key (kbd "M-SPC") 'avy-goto-char-timer)
(global-set-key (kbd "C-x C-SPC") 'avy-goto-char-timer)
(global-set-key (kbd "<f13>") 'avy-goto-char-timer)
(global-set-key (kbd "M-<tab>") 'other-window)
(global-set-key (kbd "C-t") 'avy-goto-char-2)
;; (global-set-key (kbd "M-t") 'avy-goto-symbol-1)
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
(define-key dired-mode-map (kbd "i") 'dired-toggle-read-only)
(define-key dired-mode-map (kbd "I") 'dired-subtree-remove)
(define-key dired-mode-map (kbd "M-z") nil)
(define-key dired-mode-map (kbd "M-c") nil)
(define-key dired-mode-map (kbd "L") 'dired-do-symlink)
(define-key dired-mode-map (kbd "C-h") 'dired-up-directory)
(define-key dired-mode-map (kbd "C-f") 'forward-char)

;;
(global-set-key (kbd "<f5>") 'revert-buffer)

;;** hydra-cantrips with random useful commands.
(defhydra hydra-cantrips-prefix-map (:columns 1 :exit t)
  "cantrips"
  ("s" #'string-edit-at-point "string-edit-at-point")
  ("p" #'counsel-package "counsel-package")
  ("r" #'rename-file-and-buffer "rename-file-and-buffer")
  ("d" #'describe-text-properties "describe-text-properties")
  ;; ("f" #'describe-face "describe-face")
  ("F" #'counsel-faces "counsel-faces")
  ("i" #'ielm "ielm")
  ("/" #'rg-dwim "rg-dwim")
  ("b" #'counsel-descbinds "counsel-descbinds")
  ("e" #'toggle-debug-on-error "toggle-debug-on-error")
  ("=" #'diff-current-buffer-with-file "diff-current-buffer-with-file")
  ("c" #'counsel-colors-emacs "counsel-colors-emacs")
  ("C" #'rainbow-mode "rainbow-mode")
  ("k" #'helpful-key "Describe key")
  ("o" #'helpful-symbol "Describe symbol")
  ("v" #'counsel-describe-variable "Describe variable")
  ("f" #'counsel-describe-function "Describe function")
  ("T" #'explain-pause-mode "Explain pause mode")
  ("t" #'explain-pause-top "Explain pause top")
  ("a" #'align-regexp "Align regexp")
  ("l" #'org-store-link "Org store link")
  ("L" #'display-line-numbers-mode "Display line numbers mode")
  ("K" #'free-keys "Free keys in current buffer")
  ("g" #'magit-list-repositories "Magit list repositories")
  ("j" #'json-pretty-print-dwim "Json pretty print")
  ("m" #'mu4e "mu4e")
  ("M" #'mu4e-compose-new "mu4e compose")
  ("w" #'whitespace-mode "whitespace-mode")
  ("W" #'delete-trailing-whitespace "delete-trailing-whitespace")
  )

(defun hydra-cantrips-M-x ()
  (interactive)
  (ivy-exit-with-action
   (lambda (x)
     (hydra-cantrips-prefix-map/body))))

(global-set-key (kbd "M-z") 'hydra-cantrips-prefix-map/body)

;;** ace-link
(ace-link-setup-default (kbd "C-f"))
(dolist (keymap (list help-mode-map package-menu-mode-map compilation-mode-map grep-mode-map))
  (define-key keymap (kbd "C-f") 'ace-link))

;;** C-h as Backspace
(global-set-key (kbd "C-x h") 'help-command)
(define-key minibuffer-local-map (kbd "C-h") 'backward-delete-char)

;;** C-digit
(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-1") 'delete-other-windows-toggle)
(global-set-key (kbd "C-2") 'split-window-below*)
(global-set-key (kbd "C-3") 'split-window-right*)
(global-set-key (kbd "C-4") ctl-x-4-map)
(global-set-key (kbd "C-5") ctl-x-5-map)

;;** Hyper (although some of them are in other files as well)
(global-set-key (kbd "H-;") 'eval-expression)
