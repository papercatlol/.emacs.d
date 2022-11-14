;; -*- lexical-binding: t -*-

;; remove ui
(setq default-frame-alist
      '((menu-bar-lines 0)
        (tool-bar-lines 0)
        (vertical-scroll-bars)))
(setq initial-frame-alist default-frame-alist)

;; packages
(require 'package)

;; Precompute activation actions to speed up startup.
(setq package-quickstart t)

;; Native-compile packages
(setq package-native-compile t)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

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

;;*** test gc on frame focus-out
(defun gc-on-focus-out ()
  (unless (frame-focus-state)
    (gc-with-time)))

(advice-add 'after-focus-change-function :after #'gc-on-focus-out)

;;* theme
(load-theme 'quasi-monochrome t)
(setq custom--inhibit-theme-enable nil)

;;* fill-column-indicator
(setq-default fill-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;;* use deprecated CL lib because I cba to rename everything at the moment
(require 'cl)

;;;
(require 'avy)
(require 'ace-window)
(require 'async)
(require 'beginend)
(require 'delsel)
(require 'dired-x)
(require 'dired-subtree)
(require 'expand-region)
(require 'exec-path-from-shell)
(require 'helpful)
(require 'hl-todo)
(require 'ivy-xref)
(require 'multiple-cursors)
(require 'paredit)
(require 'pcmpl-gnu)
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
;; (require 'dired+)

;;** configuration
(require 'configure-ace-window)
(require 'configure-evil)
(require 'configure-org)
(when (require 'mu4e nil t)
  (require 'configure-email))
(require 'configure-git)
(require 'configure-highlight)
(require 'configure-isearch)
(require 'configure-ivy)
(require 'configure-compilation)
(require 'counsel-ripgrep)
(require 'configure-w3m)

;;* language-specific configs
;; TODO non-lisp configs when an a specific mode is enabled for the first time
;;** lisp (CL + Elisp)
(require 'configure-lisp)

;;** go
;; (require 'configure-go-lsp)

;;** python
(require 'configure-python)

;;** java
(defun configure-java-once ()
  (require 'configure-java)
  ;; (eglot-ensure)
  (remove-hook 'java-mode-hook 'configure-java-once))

(add-hook 'java-mode-hook 'configure-java-once)

;;** javascript
(require 'configure-js)

;;** html
(with-eval-after-load 'sgml-mode
  (require 'configure-html))

;;* ./local-elisp - private/work configuration
(add-to-list 'load-path (expand-file-name "local-elisp" user-emacs-directory))
(require 'local-elisp-init)

;;* global minor modes
(delete-selection-mode 1)
(global-hl-todo-mode 1)
(tooltip-mode -1)
(global-so-long-mode 1)

;; MAYBE hydra-vlf
(require 'vlf-setup)

;;* async
(dired-async-mode t)
(async-bytecomp-package-mode t)

;;* reverse-im
(setq reverse-im-avy-action-char nil)

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
      ;; prefer horizontally split windows (see `split-window-sensibly')
      split-width-threshold 80
      split-height-threshold nil
      ;; ace-window
      aw-scope 'frame
      aw-background (display-graphic-p)
      aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      ;; avy
      ;; I'd like to use 'all-frames, but this doesn't work properly with
      ;; i3wm tabbed layout.
      avy-all-windows t
      avy-style 'pre ;; 'de-bruijn
      avy-keys (list ?f ?c ?d ?g ?s ?a ?e ?v ?q ?w ?z ?x ?r
                     ?j ?n ?k ?h ?l ?o ?i ?u ?p ?\;
                     ?F ?C ?D ?G ?S ?A ?E ?V ?Q ?W ?Z ?X ?R
                     ?J ?N ?K ?H ?L ?O ?I ?U ?P)
      ;;
      comment-padding ""
      view-read-only nil
      slime-description-autofocus t
      xref-show-xrefs-function #'ivy-xref-show-xrefs
      compilation-scroll-output t
      ;; scratch
      initial-scratch-message ""
      initial-major-mode 'emacs-lisp-mode
      ;;
      use-dialog-box nil
      comint-buffer-maximum-size 8192
      ;; fringe-mode '((4 . 4))
      fringe-mode '((8 . 0))
      read-process-output-max (* 1024 1024)
      ;; this sometimes bugs out hydra's if set to T
      switch-to-buffer-preserve-window-point nil
      bookmark-set-fringe-mark nil
      ;; mark ring sizes
      mark-ring-max 32
      global-mark-ring-max 512
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

;;** backups & tmp files
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq version-control t)
(setq delete-old-versions 'please-dont)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq lock-file-name-transforms '((".*" "~/.emacs.d/lock-files/" t)))

;;** desktop (save buffers & window configuration)
;;(add-hook 'after-init-hook #'desktop-read)
;;(add-hook 'after-init-hook #'desktop-save-mode)

;;(setq desktop-restore-forces-onscreen nil)

;;* equake
(require 'configure-equake)

;;
(add-hook 'prog-mode-hook (lambda () (setq-local show-trailing-whitespace t)))

;;* paredit-everywhere
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

(with-eval-after-load 'paredit-everywhere
  (define-key paredit-everywhere-mode-map (kbd "M-]") 'nil))


;;* built-in commands
(setq use-short-answers t)
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
    (yas-minor-mode "")
    ))

(defun cleaner-minor-modes ()
  (mapcar (lambda (mode)
            (or (assoc (car mode) minor-mode-lighters)
                mode))
          minor-mode-alist))

(require 'org-clock)
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
                    '(:eval (string-trim (or evil-mode-line-tag "")))
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
           (cl-loop for w in (window-list)
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
      enable-recursive-minibuffers t
      minibuffer-follows-selected-frame nil)
(minibuffer-depth-indicate-mode 1)

(global-set-key (kbd "H-SPC") 'switch-to-minibuffer)

;;* pdf-tools
;;(pdf-tools-install)

(define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
(define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
(define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)

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
  (or (cl-loop for w in (window-list)
            for b = (window-buffer w)
            when (eq 'helpful-mode (buffer-local-value 'major-mode b))
              do (progn (setq helpful-last-buffer b)
                        (delete-window w)
                        (return t)))
      (and helpful-last-buffer (display-buffer helpful-last-buffer))))

(global-set-key (kbd "C-h h") 'toggle-help-window)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "<f2>") #'helpful-key)
(define-key help-map (kbd "M-k") #'describe-keymap)

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
  (setf
   (alist-get (rx "*" (or "Backtrace" "Warnings" "Compile-Log" "Messages") "*")
              display-buffer-alist nil nil #'equal)
   '((display-buffer-in-side-window)
     (window-height . 0.16)
     (side . bottom)
     (slot . 1)
     (window-parameters . ((no-other-window . t)))))
  (setf
   (alist-get (rx (or (and (? "e") "shell") "vterm" "EQUAKE[") (* any))
              display-buffer-alist nil nil #'equal)
   '((display-buffer-reuse-window display-buffer-pop-up-window)
     (direction . right)
     ;;(window-height . 0.2)
     ;;(side . bottom)
     ;;(slot . 0)
     (window-parameters . ((no-other-window . nil)))))
  ;; right side window
  (setf
   (alist-get (rx "*" (or "Faces" "Colors") (* any))
              display-buffer-alist nil nil #'equal)
   '((display-buffer-in-side-window)
     (window-width . 0.3)
     (side . right)
     (slot . 0)
     (window-parameters . ((no-other-window . t)))))
  ;; left side window
  (setf
   (alist-get (rx (or "*help" "*info" "*apropos" "*man" "*woman") (* any))
              display-buffer-alist nil nil #'equal)
   '((display-buffer-in-side-window)
     (window-width . 0.2)
     (side . left)
     (slot . 0)
     (window-parameters . ((no-other-window . t)))))
  (setf
   (alist-get "\\*Custom.*" display-buffer-alist nil nil #'equal)
   '((display-buffer-in-side-window)
     (window-width . 0.3)
     (side . left)
     (slot . 1)
     (window-parameters . ((no-other-window . t)))))
  (setf
   (alist-get (rx "*compilation" (* any)) display-buffer-alist nil nil #'equal)
   '((display-buffer-reuse-window)
     (reusable-frames . visible)
     (inhibit-switch-frame . t))))

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
(define-key ctl-x-5-map (kbd "C-5") 'window-as-frame)
(define-key ctl-x-5-map (kbd "C-2") 'make-frame-command)

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
  (let ((filename (or (buffer-file-name)
                      (directory-file-name default-directory))))
    (when current-prefix-arg
      (setq filename (file-name-nondirectory filename)))
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
      (let ((new-name (counsel--find-file-1
                       "New name: " filename nil 'rename-file-and-buffer)))
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

;;** previous-window
(defun other-window-backwards ()
  (interactive)
  (other-window -1))

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
  (when-let ((buffer (cl-loop for ov in (overlays-at (point))
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
(define-key edit-indirect-mode-map (kbd "C-c C-q") 'edit-indirect-abort)

;;** tab key hacks
(setq tab-always-indent 'complete)
(setq tab-first-completion 'word-or-paren)

;; Distinguish C-i and keyboard tab key
(define-key input-decode-map (kbd "C-i") [C-i])

;;(global-set-key (kbd "<C-i>") 'hippie-expand)
(global-set-key (kbd "<C-i>") 'completion-at-point)
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
  (cond ((and (not nest) (buffer-narrowed-p))
         (widen))
        ((region-active-p)
         (call-interactively #'narrow-to-region))
        ((eq major-mode 'org-mode)
         (call-interactively #'org-narrow-to-subtree))
        ((eq major-mode 'shell-mode)
         (call-interactively #'shell-narrow-to-prompt))
        ((eq major-mode 'slime-repl-mode)
         (call-interactively #'slime-repl-narrow-to-prompt))
        (t (call-interactively #'narrow-to-defun))))

(global-set-key (kbd "C-x C-n") 'narrow-dwim)

;;** narrow-indirect
(defun narrow-indirect-dwim ()
  (interactive)
  (call-interactively
   (if (region-active-p)
       #'ni-narrow-to-region-indirect-other-window
     #'ni-narrow-to-defun-indirect-other-window)))
(define-key ctl-x-4-map (kbd "C-x C-n") 'narrow-indirect-dwim)

;;** message-truncated
(defun string-truncate-height (str height)
  "Truncate string to be under HEIGHT lines."
  (with-temp-buffer
    (insert str)
    (if (> (count-screen-lines) height)
        (string-trim
         (buffer-substring-no-properties
          (point-min)
          (progn (goto-char (point-min)) (forward-visible-line height)
                 (backward-char) (point))))
      str)))

(defun display-truncated-message (format-string &rest args)
  "Like `message', but truncate displayed string if it doesn't fit."
  (let* ((max-height (cl-typecase max-mini-window-height
                       (float (* (frame-height) max-mini-window-height))
                       (integer max-mini-window-height)
                       (t 40)))
         (str (apply #'format format-string args)))
    (message (string-truncate-height str max-height))))


;;* dired
(setq dired-do-revert-buffer t)

(add-hook 'dired-mode-hook 'hl-line-mode)

;;** dired-jump-other-frame
(defun dired-jump-other-frame (&optional file-name)
  "Like \\[dired-jump] (`dired-jump') but in other frame."
  (interactive
   (list (and current-prefix-arg
	      (read-file-name "Jump to Dired file: "))))
  (let ((pop-up-frames t))
    (dired-jump t file-name)))

;;** diff-buffer-with-file
(defun diff-current-buffer-with-file (prompt)
  (interactive "P")
  (let ((buffer (if prompt
                    (get-buffer
                     (completing-read "Buffer: "
                                      (cl-loop for b in (buffer-list)
                                            when (buffer-file-name b)
                                                 collect it)))
                  (current-buffer))))
    (if (buffer-file-name buffer)
        (diff-buffer-with-file buffer)
      (error "Buffer isn't visiting a file"))))

;;** dired-quick-sort
(require 'dired-quick-sort)
(dired-quick-sort-setup)

(setq dired-listing-switches "-laGh1v")

;;** dired-rsync
;; TODO: move dired stuff to a separate file
(require 'dired-rsync)

(define-key dired-mode-map (kbd "r") 'dired-rsync)

;; show rsync progress in modeline
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

;;** helpful new snippet template
;; Inspired by: https://mjdiloreto.github.io/posts/yasnippet-helpful-buffer/
(setq yas-new-snippet-default
  (concat
   (yas-escape-text "\
# -*- mode: snippet -*-
# http://joaotavora.github.io/yasnippet/snippet-development.html#org9801aa7

## Embed Emacs-Lisp code by using back-quotes (\\`).
## DON'T MODIFY THE BUFFER INSIDE BACKQUOTES!
#
## Placeholders - ${N:default}
#
## Transforming Mirrors
# #define \"${1:mydefine$(upcase yas-text)}\"
#
## Transforming fields - ${N:name:$(elisp)} or ${N:$$(elisp)}
# #define \"${1:$$(upcase yas-text)}\"
#
## Choose a value from a list of options
# <div align=\"${2:$$(yas-choose-value '(\"right\" \"center\" \"left\"))}\">
#
## Useful things bound inside evaluated elisp
# | variable         | description                                             |
# |------------------+---------------------------------------------------------|
# | yas-text         | the current text of this field                          |
# | yas-field-value  | the current text of $1 (or $2, etc.)                    |
# | yas-modified-p   | whether the field is modified                           |
# | yas-choose-value | user chooses from a list of options                     |
# | yas-verify-value | Verify that the current field value is in POSSIBILITIES |
#
")
   "# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# --
$0`(yas-escape-text yas-selected-text)`"))

;;* hippie-expand
(require 'hippie-exp)

;;** use hippie-expand instead of TAB
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

;;** try-functions-list
(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-tempo-complete-tag
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
        (buffer-undo-list t)            ; Don't blow up undo-list
        (hippie-expand-function (or hippie-expand-function 'hippie-expand))
        (he-need-paredit-fix? nil)
        (expansions nil))
    (flet ((ding))   ; avoid the (ding) when hippie-expand exhausts its options.
      (while (progn
               (funcall hippie-expand-function nil)
               (setq last-command 'hippie-expand--all)
               ;; Expanders like to reset markers, so we save them.
               (when-let (expansion (car he-tried-table))
                 (push (cons expansion (length he-search-string))
                       expansions))
               (not (equal he-num -1)))))
    ;; Evaluating the completions modifies the buffer, however we will finish
    ;; up in the same state that we began.
    (set-buffer-modified-p buffer-modified)
    (reverse (remove-duplicates expansions :key #'car :test #'equal))))

(defun hippie-expand-completion ()
  (interactive)
  (when-let* ((expansions (hippie-expand--all))
              ;; MAYBE: use completion-in-region; see `dabbrev-completion' for reference
              (expansion
               (if (fboundp 'ivy-read)
                   (ivy-read "Hippie expand: " (mapcar #'car expansions)
                             :caller 'hippie-expand-completion)
                 (completing-read "Hippie expand: " (mapcar #'car expansions))))
              (len (alist-get expansion expansions nil nil #'string=)))
    (he-init-string (- (point) len) (point))
    (he-substitute-string expansion t)))

;; Truncate long candidates. `try-expand-list' in particular likes to suggest
;; page-long expansions.
(with-eval-after-load 'ivy
  (ivy-configure 'hippie-expand-completion
    :format-fn #'counsel--yank-pop-format-function))

(global-set-key [remap dabbrev-completion] 'hippie-expand-completion)

;;** hippie-expand-completion-visible
(defun hippie-expand-completion-visible (&optional all-buffers?)
  "Use hippie-expand to complete visible dabbrev/list/line. With
prefix arg expand from all buffers."
  (interactive "P")
  (let ((hippie-expand-try-functions-list
         (if all-buffers?
             '(try-expand-dabbrev-all-buffers
               try-expand-line-all-buffers
               try-expand-list-all-buffers)
           '(try-expand-dabbrev-visible
             try-expand-list
             try-expand-line))))
    (call-interactively #'hippie-expand-completion)))

(global-set-key (kbd "M-'") 'hippie-expand-completion-visible)

;;** hippie expand tempo tags
;; https://www.emacswiki.org/emacs/HippieExpand#h5o-10
(require 'tempo)

(defun try-tempo-complete-tag (old)
  (unless old
    (tempo-complete-tag)))

;;** paredit fix
(defvar he-need-paredit-fix? t)

(defun he-paredit-fix (str &optional trans-case)
  "Remove extra paren when expanding line in paredit.
https://www.emacswiki.org/emacs/HippieExpand#toc9"
  (and he-need-paredit-fix?
       paredit-mode
       (equal "(" (substring he-search-string 0 1))
       (equal ")" (substring str -1))
       (looking-at-p (rx (* space) ")"))
       (backward-delete-char 1)))

(advice-add #'he-substitute-string :after #'he-paredit-fix)

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
(cl-defun pulse-cursor (&key (width 1) (face 'cursor) (delay 0.01))
  (let* ((pulse-delay delay)
         (pos (point))
         (beg (max (line-beginning-position) (- pos width)))
         (end (min (line-end-position) (+ pos width))))
    (pulse-momentary-highlight-region beg end face)))

(cl-defun pulse-current-line (&key (face 'cursor) (delay 0.01))
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
;; TODO: hydra-iedit
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

(setq outline-blank-line t)

(add-hook 'prog-mode-hook #'outline-minor-mode)

;; change ellipsis to something more distinct
(defvar outline-ellipsis ;;" ↓" " ↳"
  " ⌄"
  "The ellipsis to use for outlines.")

(set-display-table-slot standard-display-table
                        'selective-display
                        (string-to-vector outline-ellipsis))

(defun outline--set-ellipsis ()
  (when-let ((display-table (or (window-display-table)
                                buffer-display-table)))
    (set-display-table-slot display-table
                            'selective-display
                            (string-to-vector outline-ellipsis))))

(add-hook 'outline-mode-hook #'outline--set-ellipsis)
(add-hook 'outline-minor-mode-hook #'outline--set-ellipsis)

;;
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

(dolist (map (list prog-mode-map emacs-lisp-mode-map lisp-mode-map))
  (define-key map (kbd "C-TAB") 'bicycle-cycle*)
  (define-key map (kbd "C-<tab>") 'bicycle-cycle*)
  (define-key map (kbd "C-M-i") 'bicycle-cycle*))

;; fix a weird bug where `outline-regexp' matches a "(c)" at the start of the file
(defun bicycle--level-advice (fn &rest args)
  (if-let ((level (apply fn args)))
      level
    (save-excursion
     (and (re-search-forward outline-regexp nil t)
          (bicycle--level)))))
(advice-add 'bicycle--level :around #'bicycle--level-advice)

;;* hideshow (mainly for json files)
(setf (alist-get 'js-mode hs-special-modes-alist)
      '("{" "}" "/[*/]" nil))
(define-key hs-minor-mode-map (kbd "C-<tab>") 'hs-toggle-hiding)
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
(setq helm-make-cache-targets t)

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

;;** scroll-other-window
(setq next-screen-context-lines 41)       ; originally 2
(global-set-key (kbd "C-M-j") 'scroll-other-window)
(global-set-key (kbd "C-M-k") 'scroll-other-window-down)

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

;;* xdg-open-file
;; https://www.reddit.com/r/emacs/comments/cgbpvl/opening_media_files_straight_from_gnu_emacs_dired/
(defun xdg-open-file (file)
  (interactive "fxdg-open: ")
  (let ((process-connection-type nil))
    (start-process
     "" nil shell-file-name
     shell-command-switch
     (format "nohup 1>/dev/null 2>/dev/null xdg-open %s"
             (shell-quote-argument (expand-file-name file))))))

(defun dired-xdg-open-file ()
  (interactive)
  (xdg-open-file (dired-file-name-at-point)))

(define-key dired-mode-map (kbd "<M-return>") 'dired-xdg-open-file)

;;* shrink-whitespace
(global-set-key (kbd "C-c SPC") 'shrink-whitespace)
(global-set-key (kbd "C-c S-SPC") 'grow-whitespace-around)
(global-set-key (kbd "M-\\")  'shrink-whitespace)

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

;;* display compilation command in header-line of the *compilation* buffer
(defun compilation-header-line ()
  (and compilation-arguments
       (stringp (car compilation-arguments))
       (car compilation-arguments)))

(defun compilation--set-up-header-line-format ()
  (setq header-line-format `(:eval (compilation-header-line))))

(add-hook 'compilation-mode-hook #'compilation--set-up-header-line-format)

;;* kbd-helper
(defun kbd-helper ()
  "Read a key sequence and return it in a format suitable for `kbd'.
If called interactively, quote and insert it."
  (interactive)
  (let ((key (key-description (read-key-sequence "Key Sequence: "))))
    (if (interactive-p)
        (insert (format "\"%s\"" key))
      key)))

(global-set-key (kbd "H-k") 'kbd-helper)

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

(define-key flyspell-mode-map (kbd "C-.") nil)
(define-key flyspell-mode-map (kbd "C-,") nil)
(define-key flyspell-mode-map (kbd "C-;") nil)
(define-key flyspell-mode-map (kbd "C-M-i") nil)

;; Need `evil' for `evil-prev-flyspell-error'. If you don't use `evil',
;; you will have to define that function manually.
(with-eval-after-load 'evil
  (defun flyspell-hydra ()
    "Enable `flyspell-prog-mode' then flyspell current buffer and
enable `hydra-flyspell'."
    (interactive)
    (if (derived-mode-p 'prog-mode)
        (flyspell-prog-mode)
      (flyspell-mode))
    (flyspell-buffer)
    (hydra-flyspell/body))

  (defun flyspell-disable ()
    (interactive)
    (flyspell-mode -1)
    (message "Flyspell off."))

  (defun flyspell-save-to-dictionary ()
    "Save misspelled word at point to private dictionary."
    ;; NOTE: aspell usually stores its private dict at ~/.aspell.<lang>.pws
    ;; Grep `$ aspell dump config` output if unsure.
    (interactive)
    (when-let* ((thing (flyspell-get-word))
                (word (car thing))
                (start (cl-second thing)))
      ;; From `ispell-command-loop'.
      (when (yes-or-no-p (format "Save \"%s\" to private dictionary?" word))
        (ispell-send-string (concat "*" word "\n"))
        (setq ispell-pdict-modified-p '(t))
        (ispell-pdict-save ispell-silently-savep)
        (flyspell-unhighlight-at start))))

  (defhydra hydra-flyspell ()
    "Flyspell"
    ("j" #'evil-next-flyspell-error "next error")
    ("k" #'evil-prev-flyspell-error "prev error")
    ("SPC" #'flyspell-auto-correct-word "auto-correct word")
    ("g" #'flyspell-buffer "Flyspell whole buffer")
    ("C-f" #'flyspell-mode "Toggle flyspell mode")
    ("t" #'flyspell-mode "Toggle flyspell mode")
    ("a" #'ace-flyspell-jump-word "Ace flyspell")
    ("i" #'flyspell-save-to-dictionary "Save word to private dictionary")
    ("u" #'undo-tree-undo "Undo")       ; Is there a generic way to call undo?
    ("$" #'ispell-word "ispell")
    ("q" nil "quit")
    ("C-q" #'flyspell-disable "Disable flyspell and quit" :color blue))

  ;; Make it seem like `flyspell-auto-correct-word' has been called normally.
  ;; Otherwise repeated calls wouldn't cycle through replacement candidates.
  ;; TODO a better interface to `flyspell-auto-correct-word' (ivy, avy-like
  ;; candidate selection by key or at least show available candidates inline).
  (defun hydra-flyspell--flyspell-auto-correct-word-advice (func &rest args)
    (setq this-command 'flyspell-auto-correct-word)
    (apply func args))
  (advice-add 'hydra-flyspell/flyspell-auto-correct-word :around
              #'hydra-flyspell--flyspell-auto-correct-word-advice)

  (define-key flyspell-mode-map (kbd "C-c C-f") 'hydra-flyspell/body)
  (define-key prog-mode-map (kbd "C-c C-f") 'flyspell-hydra)
  (define-key markdown-mode-map (kbd "C-c C-f") 'flyspell-hydra)
  (define-key org-mode-map (kbd "C-c C-f") 'flyspell-hydra))

;; Display flyspell corrections in a popup instead if the minibuffer.
(defvar flyspell-display-next-corrections-popup-height 10)

(defun flyspell-display-next-corrections--advice (corrections)
  (popup-tip (string-join (remove-duplicates
                           (subseq corrections 1
                                   (1+ flyspell-display-next-corrections-popup-height))
                           :test #'string=)
                          "\n")))
(advice-add 'flyspell-display-next-corrections :override
            #'flyspell-display-next-corrections--advice)

;;* define-word
;; https://github.com/abo-abo/define-word
(require 'define-word)

(defun define-word-dwim (word service)
  (interactive
   (list (read-string "Define word: "
                      (if (region-active-p)
                          (buffer-substring-no-properties
                           (region-beginning)
                           (region-end))
                        (word-at-point)))
         (if current-prefix-arg
             (intern
              (completing-read
               "Service: " define-word-services))
           define-word-default-service)))
  (define-word word service))

(global-set-key (kbd "H-d") 'define-word-dwim)

;;* dumb-jump
(require 'dumb-jump)

(setq dumb-jump-prefer-searcher 'rg)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;;* custom-mode
(define-key custom-mode-map "j" 'next-line)
(define-key custom-mode-map "k" 'previous-line)
(define-key custom-mode-map (kbd "C-M-j") 'widget-forward)
(define-key custom-mode-map (kbd "C-M-k") 'widget-backward)

(with-eval-after-load 'evil
  (define-key custom-mode-map "n" 'evil-search-next))

;;* resize windows horizontally
(defun get-window-resize-delta ()
  (if current-prefix-arg
      (prefix-numeric-value current-prefix-arg)
    4))

(defun window-right-aligned-p (&optional window)
  "T if WINDOW's center is horizontally to the right of the
center of the frame."
  (destructuring-bind (left top right bottom)
      (window-edges window)
    (let ((window-center (/ (+ left right) 2))
          (frame-center (/ (frame-width) 2)))
      (> window-center frame-center))))

(defun window-grow-horizontally (delta)
  (interactive (list (get-window-resize-delta)))
  (window-resize (selected-window) delta t))

(defun window-shrink-horizontally (delta)
  (interactive (list (get-window-resize-delta)))
  (window-grow-horizontally (- delta)))

(defun window-resize-left (delta)
  "Grow window horizontally if it is right-aligned, otherwise
shrink it. Think 'drag window's left border to the left' (this
isn't that intuitive if you have more that two horizontal
windows)."
  (interactive (list (get-window-resize-delta)))
  (if (window-right-aligned-p)
      (window-grow-horizontally delta)
    (window-shrink-horizontally delta)))

(defun window-resize-right (delta)
  "Inverse of `window-resize-left'."
  (interactive (list (get-window-resize-delta)))
  (if (window-right-aligned-p)
      (window-shrink-horizontally delta)
    (window-grow-horizontally delta)))

(global-set-key (kbd "C-M-}") 'window-resize-right)
(global-set-key (kbd "C-M-{") 'window-resize-left)

;;* resize windows vertically
(global-set-key (kbd "C-M-+") 'enlarge-window)
(global-set-key (kbd "C-M-_") 'shrink-window)

;;* show-toplevel
;; Inspired by `lispy-show-top-level' and `topsy'.
(defun show-toplevel ()
  "Show first line of the top-level form containing point."
  (interactive)
  (minibuffer-message
   (save-excursion
    (beginning-of-defun)
    (font-lock-ensure (point) (line-end-position))
    (buffer-substring (point) (line-end-position)))))

(define-key prog-mode-map (kbd "C-c w") 'show-toplevel)

;;* face-attributes-pretty-print
(defun face-attributes-pretty-print (face)
  "Insert a `defface'-style attribute list for FACE.
If there is a symbol at point, insert only the attribute list,
else insert the face name as well."
  (interactive (let* ((faces (mapcar #'symbol-name (face-list)))
                      (symbol-at-point (symbol-at-point))
                      (initial-input (and symbol-at-point
                                          (symbol-name symbol-at-point))))
                 (list (completing-read "Face: " faces nil t initial-input))))
  (when (stringp face)
    (setq face (intern face)))

  (let ((attributes `((t ,(face-attr-construct face))))
        (symbol-bounds (bounds-of-thing-at-point 'symbol)))
    (if (null symbol-bounds)
        (setq attributes `'(,face ,attributes))
      (goto-char (cdr symbol-bounds))
      (insert " "))
    (prin1 attributes (current-buffer))))

;;* space-after M-SPC/S-SPC
(defun space-after ()
  "Insert a space after cursor: '|' -> '| '."
  (interactive)
  (insert " ")
  (backward-char))
(global-set-key (kbd "M-SPC") 'space-after)
(global-set-key (kbd "S-SPC") 'space-after)

;;* previous/next-buffer
;; `this' means do not switch to a buffer shown on the frame that hosts the
;; window `switch-to-prev-buffer' is acting upon.
(setq switch-to-prev-buffer-skip 'this)

(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

;;* flymake config
(with-eval-after-load 'flymake
  (require 'configure-flymake))

;;* tldr
(with-eval-after-load 'tldr
  (setq tldr-use-word-at-point t)

  (with-eval-after-load 'evil
    (evil-set-initial-state 'tldr-mode 'emacs))

  (define-key tldr-mode-map (kbd "j") 'next-line)
  (define-key tldr-mode-map (kbd "k") 'previous-line))

;;* avy
;;TODO move avy-related configuration to a separate file
;;** avy-goto-char-2-special hacks
(defvar avy-key-translations
  (cl-loop for (key translation)
        ;; TODO allow regexps & shorter input
        in '(("C-r" "(")
             ;; Lisp common keywords.
             ("C-d" "(def")
             ("C-i" "(if")
             ("C-w" "(when")
             ("C-s" "(set")
             ("C-l" "(let")
             ("C-t" "then")
             ("C-e" "else")
             ;; RET = no char. Useful when you want to search for 1 char only.
             ("RET")
             )
        collect (cons (string-to-char (kbd key))
                      (listify-key-sequence translation))))

(defun avy-read-char (prompt)
  (let* ((char (read-char prompt))
         (translation (assoc char avy-key-translations)))
    (if translation
        (cdr translation)
      (list char))))

(defun avy-goto-char-2-special (&optional all-frames)
  "Like `avy-goto-char-2', but translate some keys. See
`avy-key-translations'."
  (interactive "P")
  (let* ((input-1 (avy-read-char "Char 1: "))
         (input-2 (unless (cl-second input-1)
                    (avy-read-char "Char 2: ")))
         (avy-all-windows (if all-frames 'all-frames t))
         ;; HACK prevent avy from flipping `avy-all-windows'
         (current-prefix-arg nil)
         ;; HACK prevent avy-with from overriding `avy-action'
         (avy-default-action avy-action))
    (avy-with avy-goto-char-2-special
      ;; TODO Make avy-jump filter out some obvious candidates
      ;; (current/prev/next line, beginning/end of defun/list, etc).
      (let ((avy-action (or avy-default-action avy-action)))
        (avy-jump
         (regexp-quote (concatenate 'string input-1 input-2)))))))

(setf (alist-get 'avy-goto-char-2 avy-styles-alist) 'at)
(setf (alist-get 'avy-goto-char-2-special avy-styles-alist) 'at)

(global-set-key (kbd "C-r") 'avy-goto-char-2-special)
(define-key minibuffer-local-map (kbd "C-r") 'avy-goto-char-2-special)

;;** avy-yank-char-2-special
(defun avy-yank-char-2-special ()
  "Like `avy-goto-char-2-special', but bind `avy-action' to `avy-action-yank'."
  (interactive)
  (let ((avy-action #'avy-action-yank))
    (call-interactively #'avy-goto-char-2-special)))
(global-set-key (kbd "C-S-R") 'avy-yank-char-2-special)

;;** avy-goto-symbol-definition-2
(defun avy-goto-symbol-definition-2 ()
  (interactive)
  (call-interactively #'avy-goto-symbol-2)
  ;; HACK different modes remap M-. to their xref equivalent, so we just emulate
  ;; M-. keypress event. LF a better way to do this...
  (setq unread-command-events (listify-key-sequence (kbd "M-."))))

(global-set-key (kbd "C-M-.") 'avy-goto-symbol-definition-2)

;;** avy-action-goto-definition
(defun avy-action-goto-definition (pt)
  "Goto definition of symbol at PT."
  (goto-char pt)
  (setq unread-command-events (listify-key-sequence (kbd "M-."))))

(setf (alist-get ?. avy-dispatch-alist) #'avy-action-goto-definition)
(setf (alist-get ?  avy-dispatch-alist) #'avy-action-goto-definition)

;;** fix evil + avy-goto-char-timer
;; Don't wrap with `evil-enclose-avy-for-motion' to allow movement between
;; windows in visual-state.
(with-eval-after-load 'evil
  (evil-define-command evil-avy-goto-char-timer (&optional count)
    "Evil motion for `avy-goto-char-timer'."
    :repeat abort :type inclusive
    :jump t :keep-visual t
    (interactive "<c>")
    (evil-without-repeat (call-interactively 'avy-goto-char-timer))))

;;** avy-action-donate
(defun avy-action-donate (pt)
  "Yank active region or sexp-at-point to the point after PT. Moves
the cursor to the new position as well."
  (when-let ((text
              (save-selected-window
               (select-window (cdr (ring-ref avy-ring 0)))
               (when-let ((bounds (or (and (region-active-p)
                                           (cons (region-beginning) (region-end)))
                                      (bounds-of-thing-at-point 'list)
                                      (bounds-of-thing-at-point 'sexp))))
                 (buffer-substring-no-properties (car bounds) (cdr bounds))))))
    (goto-char pt)
    (avy-forward-item)
    (insert text)))

(setf (alist-get ?m avy-dispatch-alist) #'avy-action-donate)
(setf (alist-get (aref (kbd "C-y") 0) avy-dispatch-alist) #'avy-action-donate)

;;** TODO avy + edebug (avy-action-toggle-breakpoint or smth)

;;* embark (trying it out)
(require 'configure-embark)

;;* highlight-tabs-mode
(defun highlight-tabs-mode ()
  "Enable `whitespace-mode' for tabs only."
  (interactive)
  (require 'whitespace)
  (let ((whitespace-style '(face tabs)))
    (whitespace-mode)))

(add-hook 'lisp-mode-hook #'highlight-tabs-mode)
(add-hook 'emacs-mode-hook #'highlight-tabs-mode)

;;* proced
;; f - filtering, F - format (# columns)
(global-set-key (kbd "H-p") 'proced)
(define-key ctl-x-map (kbd "P") 'proced)

(with-eval-after-load 'proced
  (define-key proced-mode-map (kbd "j") 'next-line)
  (define-key proced-mode-map (kbd "k") 'previous-line)
  (define-key proced-mode-map (kbd "C-k") 'proced-omit-processes)
  (define-key proced-mode-map (kbd "G") 'proced-toggle-auto-update)
  (define-key proced-mode-map (kbd "S") 'proced-sort-interactive))

;;* search code on github
;; https://dd.reddit.com/r/emacs/comments/also27/second_trial_for_a_weekly_tipstricksetc_thread/eg0iiga/
(defvar github-search-code-history nil)
(defvar github-search-code-default-lang "Emacs+Lisp")

(defun github-search-code (query &optional lang)
  ;; Search GitHub for Elisp code with QUERY as search string. The QUERY should
  ;; consist of words separated with a space.
  (interactive
   (let* ((elisp-tag "Emacs+Lisp")
          (lang (when current-prefix-arg
                  (read-string "Lang: " nil nil
                               github-search-code-default-lang))))
     (list (read-string "Search github: " nil 'github-search-code-history)
           lang)))
  (unless lang (setq lang github-search-code-default-lang))

  (let ((url (format "https://github.com/search?l=%s&q=%s&type=Code"
                     lang query)))
    (browse-url (url-encode-url url))))

;;* conf-mode
(defhydra hydra-conf-mode (:columns 1 :exit t)
  "Conf-mode"
  ("c" 'conf-colon-mode "conf-colon-mode")
  ("j" 'conf-javaprop-mode "conf-javaprop-mode")
  ("p" 'conf-ppd-mode "conf-ppd-mode")
  ("u" 'conf-unix-mode "conf-unix-mode")
  ("w" 'conf-windows-mode "conf-windows-mode")
  ("x" 'conf-xdefaults-mode "conf-xdefaults-mode")
  (":" 'conf-colon-mode "conf-colon-mode"))

(with-eval-after-load 'conf-mode
  (define-key conf-mode-map (kbd "C-c C-x") nil)
  (define-key conf-mode-map (kbd "C-c C-c") 'hydra-conf-mode/body))

;;* append-to-other-window
(defvar-local append-to-other-window-target nil)

(defun append-to-other-window (&optional force-select-window)
  "Append to specified (via `ace-window') WINDOW the text of the region.
With prefix-arg force window selection."
  (interactive "P")
  (require 'ace-window)
  (when-let ((oldbuf (current-buffer))
             (beg (region-beginning))
             (end (region-end))
             (target-window
              (or (and (null force-select-window)
                       (windowp append-to-other-window-target)
                       ;; Check if window is still visible.
                       (member append-to-other-window-target (window-list))
                       append-to-other-window-target)
                  (setq append-to-other-window-target
                        (aw-select " Ace - Append to Window")))))
    (with-selected-window target-window
      (barf-if-buffer-read-only)
      (insert-buffer-substring oldbuf beg end))))

(define-key ctl-x-r-map "a" 'append-to-other-window)

;;* mosey
(require 'mosey)

(defvar mosey-starting-point nil "Position of initial mosey invocation.")

(defun mosey-set-starting-point ()
  (unless (and mosey-starting-point
               (<= (line-beginning-position)
                   mosey-starting-point
                   (line-end-position)))
    (setq mosey-starting-point (point))))

(defun mosey-goto-starting-point ()
  (when mosey-starting-point
    (goto-char mosey-starting-point)
    (setq mosey-starting-point nil)))

(defun mosey-backward-bounce+ (go-back)
  "Like `mosey-backward-bounce', but go back to the point where
mosey was first called with prefix arg."
  ;; MAYBE just use `set-mark-command' instead?
  (interactive "P")
  (if (and go-back mosey-starting-point)
      (mosey-goto-starting-point)
    (mosey '(mosey-set-starting-point
             beginning-of-line
             back-to-indentation
             mosey-goto-beginning-of-comment-text)
           :bounce t :backward t)))

(global-set-key (kbd "C-a") 'mosey-backward-bounce+)

;; This is similar in spirit, so set it here instead of configure-lispy.el
(with-eval-after-load 'lispy
  (global-set-key (kbd "C-e") 'lispy-move-end-of-line))

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
  ("|" #'ibuffer-or-filter "ibuffer-or-filter")
  ("q" nil "quit"))

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
(global-set-key (kbd "<f15>") 'other-window)
(global-set-key (kbd "C-t") 'avy-goto-word-2)
;; (global-set-key (kbd "M-t") 'avy-goto-symbol-1)

;;** hydra-M-g
(defhydra hydra-M-g (global-map "M-g")
  "M-g"
  ("n" next-error)
  ("M-n" next-error)
  ("j" next-error)
  ("M-j" next-error)
  ("p" previous-error)
  ("M-p" previous-error)
  ("k" previous-error)
  ("M-k" previous-error)
  ("g" avy-goto-line :exit t)
  ("M-g" avy-goto-line :exit t)
  ("c" goto-char :exit t )
  ("M-c" goto-char :exit t)
  ("q" nil)
  ("RET" nil))

;;** multiple-cursors
;; TODO hydra-multiple-cursors
;; Looks interesting: https://hungyi.net/posts/hydra-for-evil-mc/
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

(setq mc/insert-numbers-default 1)
(global-set-key (kbd "C-x m 1") 'mc/insert-numbers)
(global-set-key (kbd "C-x m 2") 'mc/insert-letters)

;;** dired
(global-set-key (kbd "C-x C-d") 'dired)
(define-key dired-mode-map (kbd "<backspace>") 'diredp-up-directory)
(define-key dired-mode-map (kbd "C-h") 'dired-up-directory)
(define-key dired-mode-map (kbd "C-w") 'dired-up-directory)
(define-key dired-mode-map (kbd "C-t") 'avy-goto-word-or-subword-1)
(define-key dired-mode-map (kbd "<tab>") 'dired-subtree-toggle)
(define-key dired-mode-map (kbd "i") 'dired-toggle-read-only)
(define-key dired-mode-map (kbd "I") 'dired-subtree-remove)
(define-key dired-mode-map (kbd "M-z") nil)
(define-key dired-mode-map (kbd "M-c") nil)
(define-key dired-mode-map (kbd "L") 'dired-do-symlink)
(define-key dired-mode-map (kbd "C-f") 'forward-char)
(define-key dired-mode-map (kbd "C-k") 'dired-do-kill-lines)

;;** revert-buffer
(global-set-key (kbd "<f5>") 'revert-buffer)

;;** hydra-cantrips with random useful commands.
(defhydra hydra-cantrips (:columns 1 :exit t)
  "cantrips"
  ("/" #'rg-dwim "rg-dwim")
  ("=" #'describe-char "describe-char")
  ("a" #'align-regexp "Align regexp")
  ("b" #'counsel-descbinds "counsel-descbinds")
  ("c" #'counsel-colors-emacs "counsel-colors-emacs")
  ("C" #'rainbow-mode "rainbow-mode")
  ("d" #'describe-text-properties "describe-text-properties")
  ("D" #'shortdoc-display-group "shortdoc-display-group")
  ("e" #'toggle-debug-on-error "toggle-debug-on-error")
  ("f" #'describe-face "describe-face")
  ("F" #'counsel-faces "counsel-faces")
  ("M-f" #'face-attributes-pretty-print "pp face attributes")
  ("g" #'magit-list-repositories "Magit list repositories")
  ("i" #'ielm "ielm")
  ("j" #'json-pretty-print-dwim "Json pretty print")
  ("k" #'helpful-key "Describe key")
  ("K" #'free-keys "Free keys in current buffer")
  ("l" #'org-store-link "Org store link")
  ("L" #'display-line-numbers-mode "Display line numbers mode")
  ("m" #'mu4e "mu4e")
  ("M" #'mu4e-compose-new "mu4e compose")
  ("M-m" #'memory-report "Memory report")
  ("o" #'helpful-symbol "Describe symbol")
  ("p" #'counsel-package "counsel-package")
  ("P" #'list-processes "list-processes")
  ("q" nil "quit")
  ("r" #'rename-file-and-buffer "rename-file-and-buffer")
  ("s" #'string-edit-at-point "string-edit-at-point")
  ("t" #'tldr "TLDR")
  ("T" #'explain-pause-top "Explain pause top")
  ("v" #'counsel-describe-variable "Describe variable")
  ("w" #'whitespace-mode "whitespace-mode")
  ("W" #'delete-trailing-whitespace "delete-trailing-whitespace")
  ("<tab>" #'untabify "untabify")
  ("C-o" #'xdg-open-file "open file external")
  )

(global-set-key (kbd "M-z") 'hydra-cantrips/body)

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
(global-set-key (kbd "H-;") 'pp-eval-expression)
(global-set-key (kbd "<H-return>") 'eval-expression)
(global-set-key (kbd "H-w") 'org-store-link)

;;** comint
(define-key comint-mode-map (kbd "C-c C-x") nil)

;;** xref--xref-buffer-mode
(define-key xref--xref-buffer-mode-map (kbd "M-m") 'xref-show-location-at-point)
(define-key xref--xref-buffer-mode-map (kbd "f") 'xref-show-location-at-point)

;;** info-mode
(define-key Info-mode-map "j" 'next-line)
(define-key Info-mode-map "k" 'previous-line)
(define-key Info-mode-map "h" 'backward-char)
(define-key Info-mode-map "l" 'forward-char)
;;(define-key Info-mode-map (kbd "C-w") 'Info-backward-node)
(define-key Info-mode-map (kbd "C-w") 'Info-up)

;;** registers
;;*** dwim registers
(defun register-dwim (register)
  "Insert or jump to a register depending on its contents."
  (interactive (list (register-read-with-preview "DWIM register: ")))
  (if-let ((content (get-register register)))
      (typecase content
        ;; TODO rectangle selection is a cons - handle it somehow
        ((or string number)
         (insert-register register))
        ((or marker cons)
         (jump-to-register register))
        (otherwise
         (user-error "Don't know how to handle register %s of type %s. Content: %s."
                     (single-key-description register)
                     (type-of content) content)))
    (user-error "Register %s is empty." (single-key-description register))))

(defun save-to-register-dwim (register &optional arg)
  "With no prefix arg and region active call `copy-to-register',
otherwise forward to `point-to-register'."
  (interactive (list (register-read-with-preview
                      (format "Save %s to register: "
                              (if (region-active-p) "region" "point")))
                     current-prefix-arg))
  (cond ((and (not arg) (region-active-p))
         (copy-to-register register (region-beginning) (region-end))
         (message "Region copied to register %s."
                  (single-key-description register)))
        (t
         (point-to-register register arg)
         (message "Point%s stored in register %s."
                  (if arg " and frame configuration" "")
                  (single-key-description register)))))

(define-key global-map (kbd "H-`") 'save-to-register-dwim)

;;*** quick registers
(defvar quick-registers '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0))

(defmacro quick-registers-init ()
  (let ((defs
          (loop for r in quick-registers
                for str = (string r)
                for name = (make-symbol
                            (concat "quick-register-" str))
                collect `(defun ,name (&optional store)
                           ,(format "Call `register-dwim' on a register %s.\
 With universal arg call `save-to-register-dwim' instead." str)
                           (interactive "P")
                           (if store
                               (save-to-register-dwim ,r)
                             (register-dwim ,r)))
                collect `(define-key global-map (kbd ,(concat "H-" str))
                           ',name))))
    `(progn ,@defs)))

(quick-registers-init)
