;; -*- lexical-binding: t -*-

;; remove ui
(setq default-frame-alist
      '((menu-bar-lines 0)
        (tool-bar-lines 0)
        (vertical-scroll-bars)
        (width . 80)
        (height . 42)))
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

;;* nicer warning symbol
(setq icon-preference '(image symbol text emoji))

(when (require 'icons nil t)
  (define-icon warnings-suppress button
    `((emoji "⛔")
 ;; Many MS-Windows console fonts don't have good glyphs for U+25A0.
      (symbol ,(if (and (eq system-type 'windows-nt)
                        (null window-system))
                   " » "
                 " [!] "))
      (text " stop "))
    "Suppress warnings."
    :version "29.1"
    :help-echo "Click to suppress this warning type"))

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

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
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
;; TODO non-lisp configs when a specific mode is enabled for the first time
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

;;** clojure
(with-eval-after-load 'clojure-mode
  (require 'configure-clojure))

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
      kill-ring-max 256
      sentence-end-double-space nil
      apropos-do-all t
      dired-dwim-target t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      frame-title-format "%b"
      frame-inhibit-implied-resize t
      expand-region-fast-keys-enabled nil
      er--show-expansion-message t
      inhibit-startup-message t
      wgrep-auto-save-buffer t
      uniquify-buffer-name-style 'forward
      ;; vc
      vc-follow-symlinks t
      ;; prefer horizontally split windows (see `split-window-sensibly')
      window-min-width 60
      split-width-threshold 80
      split-height-threshold nil
      ;; avy
      ;; I'd like to use 'all-frames, but this doesn't work properly with
      ;; i3wm tabbed layout.
      avy-all-windows t
      avy-style 'pre ;; 'de-bruijn
      avy-keys (list ?f ?c ?d ?g ?s ?a ?e ?v ?q ?w ?z ?x ?r ?b
                     ?j ?n ?k ?h ?l ?o ?i ?u ?p ?\( ?- ?\;
                     ?1 ?2 ?3 ?4 ?5
                     ?F ?C ?D ?G ?S ?A ?E ?V ?Q ?W ?Z ?X ?R
                     ?J ?N ?K ?H ?L ?O ?I ?U ?P ?B ?M ?T ?\[ ?\]
                     ?/ ?? 32
                     ;;?6 ?7 ?8 ?9 ?0
                     )
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
      comint-buffer-maximum-size 4096
      ;; fringe-mode '((4 . 4))
      fringe-mode '(8 . 0)
      read-process-output-max (* 1024 1024)
      ;; this sometimes bugs out hydra's if set to T
      switch-to-buffer-preserve-window-point nil
      bookmark-set-fringe-mark nil
      ;; mark ring sizes
      mark-ring-max 32
      global-mark-ring-max 256
      vc-follow-symlinks t
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

;;*** persistent registers
(defvar register-alist-printable nil)

(defun savehist-fix-register-alist ()
  "Replace unprintable values in `register-alist' before saving."
  (setq register-alist-printable
        (loop for (register . val) in register-alist
              ;; See `register-val-jump-to'.
              when
              (cond
                ((frame-configuration-p (car-safe val))
                 ;; TODO
                 nil)
                ((window-configuration-p (car-safe val))
                 ;; TODO
                 nil)
                ((markerp val)
                 ;; See `register-swap-out'.
                 (cons register (list 'file-query
                                      (buffer-file-name (marker-buffer val))
                                      (marker-position val))))
                ((or (eq (car-safe val) 'file)
                     (eq (car-safe val) 'file-query)
                     (stringp val))
                 (cons register val)))
              collect it)))

(add-hook 'savehist-save-hook 'savehist-fix-register-alist)
(add-to-list 'savehist-additional-variables 'register-alist-printable)

(with-eval-after-load savehist-file
  (when (and register-alist-printable (null register-alist))
    (setq register-alist register-alist-printable)))

;; Load `savehist-file' after all the hooks.
(savehist-mode)

;;** backups & tmp files
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(require 'vc-backup nil t)              ; backup files as a vc backend
(setq version-control t)
(setq kept-new-versions 4)
(setq delete-old-versions 'please-dont)
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save-list/") t)))
(setq lock-file-name-transforms
      `((".*" ,(concat user-emacs-directory "lock-files/") t)))

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

(defun vc-mode-line-with-glyph ()
  (if vc-mode
      (replace-regexp-in-string (rx (1+ alnum) ":")
                                (concat (char-to-string #xe0a0) ":")
                                vc-mode)
    ""))

(defface mode-line-buffer-remote-face
    '((t (:inherit mode-line-buffer-id :foreground "#7B6BFF")))
  "Modeline face for names of buffer opened remotely."
  :group 'basic-faces)

(defface mode-line-buffer-sudo-face
    '((t (:inherit mode-line-buffer-id :foreground "OrangeRed")))
  "Modeline face for names of buffer opened as root."
  :group 'basic-faces)

(defun mode-line-rich-buffer-name ()
  "Colorize buffer names opened as root or remotely."
  (list
   (propertize "%b" 'face
               (cond ((when-let ((buf (buffer-file-name)))
                        ;; FIXME There must be a better way to check this:
                        (or (tramp-sudoedit-file-name-p buf)
                            (string-prefix-p "/su" (file-remote-p buf))))
                      'mode-line-buffer-sudo-face)
                     ((file-remote-p default-directory)
                      'mode-line-buffer-remote-face)
                     (t 'mode-line-buffer-id)))))

(require 'org-clock)
(setq-default mode-line-format
              (list '(:eval (ace-window-path-lighter))
                    "%e"
                    mode-line-front-space
                    mode-line-mule-info
                    mode-line-client
                    mode-line-modified
                    '(:eval (mode-line-rich-buffer-name))
                    ":%l %p "
                    '(:eval (string-trim (or evil-mode-line-tag "")))
                    '(:eval (when slime-mode (concat " " (slime-current-package))))
                    '(vc-mode (:eval (vc-mode-line-with-glyph)))
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
  ;; TODO handle the case when number of windows goes from >1 to 1
  (when (cdr (window-list))
    (let* ((window-names
             (cl-loop for w in (window-list)
                      for b = (window-buffer w)
                      unless (minibufferp b)
                      collect (format "[%s]" (buffer-name b))))
           (title (string-join window-names " ")))
      (set-frame-parameter nil 'title title))))

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

;; Set cursor style to bar in minibuffer.
(defun minibuffer-set-cursor-type ()
  (setq-local cursor-type
              (or (bound-and-true-p evil-insert-state-cursor)
                  '(bar 2))))

(add-hook 'minibuffer-setup-hook #'minibuffer-set-cursor-type)

;;(global-set-key (kbd "H-SPC") 'switch-to-minibuffer)

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
(setq switch-to-buffer-obey-display-actions t)
(progn
  ;; bottom side window
  (setf
   (alist-get (rx "*" (or "Backtrace" "Warnings" "Compile-Log" "Messages") "*")
              display-buffer-alist nil nil #'equal)
   '((display-buffer-in-side-window)
     (window-height . 0.16)
     (side . bottom)
     (slot . 1)
     (window-parameters . ((no-other-window . nil)))))
  (setf
   (alist-get (rx (or (and (? "e") "shell") "vterm" "EQUAKE[" "*slime-repl") (* any))
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
     (window-parameters . ((no-other-window . nil)))))
  ;; left side window
  (setf
   (alist-get (rx (or "*help" "*info" "*apropos" "*man" "*woman") (* any))
              display-buffer-alist nil nil #'equal)
   '((display-buffer-in-side-window)
     (window-width . 0.2)
     (side . left)
     (slot . 0)
     (window-parameters . ((no-other-window . nil)))))
  (setf
   (alist-get "\\*Custom.*" display-buffer-alist nil nil #'equal)
   '((display-buffer-in-side-window)
     (window-width . 0.3)
     (side . left)
     (slot . 1)
     (window-parameters . ((no-other-window . nil)))))
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
(setq tab-first-completion nil)

;; Distinguish C-i and keyboard tab key
(define-key input-decode-map (kbd "C-i") [C-i])

;;(global-set-key (kbd "<C-i>") 'hippie-expand)
(global-set-key (kbd "<C-i>") 'completion-at-point)
(global-set-key (kbd "TAB") 'indent-for-tab-command)

;; trigger elisp completion in any mode
(defun completion-at-point-elisp ()
  "Perform completion of an elisp symbol at point."
  (interactive)
  (let ((completion-at-point-functions
          '(elisp-completion-at-point t)))
    (call-interactively #'completion-at-point)))

(global-set-key (kbd "C-c <C-i>") 'completion-at-point-elisp)

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
;; TODO narrowing+indirect clone. See `narrow-indirect' for reference.
;; E.g.: "C-4 C-n" = narrow-indirect-dwim
;;       "C-4 C-4 ." = find-xref-narrow-indirect
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
    (message "%s" (string-truncate-height str max-height))))


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

(setq dired-listing-switches
  (combine-and-quote-strings '("-l"
                               "-v"
                               ;;"-g" ; skip owner
                               "--no-group"
                               "--human-readable"
                               ;;"--time-style=+%Y-%m-%d"
                               "--almost-all")))

;;** dired-rsync
;; TODO: move dired stuff to a separate file
(require 'dired-rsync)

(setq dired-rsync-passphrase-stall-regex
      (rx (or "Enter passphrase for key"
              "'s password:")))

(define-key dired-mode-map (kbd "r") 'dired-rsync)

;;** dired-copy-image
(defun dired-copy-image (pathname)
  "Copy image at point to clipboard using `xclip'.
 TODO support other mime types. See `file -b --mime-type`."
  (interactive (list (dired-get-file-for-visit)))
  (when (shell-command
         (format "nohup xclip -selection clipboard -target image/png -loops 1 -i \"%s\" >/dev/null 2>&1" pathname))
    (message "Copied image: %s." pathname)))

(define-key dired-mode-map (kbd "I") 'dired-copy-image)

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

;;** utils
(defvar-local yas--need-closing-paren-p nil)

(defun $op ()
  (if (looking-back "(")
      (setq yas--need-closing-paren-p nil)
    (setq yas--need-closing-paren-p t)
    "("))

(defun $cp ()
  (when yas--need-closing-paren-p ")"))

;;** use hippie-expand instead of TAB
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)


;;* hippie-expand
(require 'hippie-exp)

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
    (cl-flet ((ding (&rest _)
                nil))   ; avoid the (ding) when hippie-expand exhausts its options.
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
               (if (null (cdr expansions))
                   (caar expansions)
                 (if (fboundp 'ivy-read)
                     (ivy-read "Hippie expand: " (mapcar #'car expansions)
                               :caller 'hippie-expand-completion)
                   (completing-read "Hippie expand: " (mapcar #'car expansions)))))
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

;;** hydra-hippie-expand
(defhydra hydra-hippie-expand (:hint nil)
  "Hippie expand"
  ("w" #'hippie-expand)
  ("e" #'hippie-expand-completion :color blue)
  ("u" #'undo-tree-undo)
  ("q" nil))

(hydra-set-property 'hydra-hippie-expand :verbosity 0)

(global-set-key (kbd "M-/") 'hydra-hippie-expand/hippie-expand)

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
(define-key package-menu-mode-map (kbd "h") 'backward-char)
(define-key package-menu-mode-map (kbd "l") 'forward-char)

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
(with-eval-after-load 'iedit
  (define-key iedit-mode-keymap (kbd "C-h") nil))

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

(dolist (map (list prog-mode-map emacs-lisp-mode-map
                   lisp-mode-map markdown-mode-map))
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

(setq tramp-default-method "ssh") ; "ssh"/"scp"
(setq tramp-default-remote-shell "/bin/bash")

;; Try to speed things up
(setq remote-file-name-inhibit-cache nil)
(setq tramp-completion-reread-directory-timeout nil)

;; we use magit anyway, so this shouldn't change anything in theory(?)
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; Use Control* options from ssh config. Should speedup magit.
(setq tramp-use-ssh-controlmaster-options nil)

;; eshell-tramp module
(require 'em-tramp)

;;* link-hint
(defvar link-hint-avy-all-windows t)
(defvar link-hint-avy-all-windows-alt 'all-frames)

(defun link-hint-open-link-wrapper ()
  "Bind some avy variables, then forward to `link-hint-open-link'."
  ;; TODO custom dispatch alist (e.g. with copy-link)
  (interactive)
  (let ((avy-single-candidate-jump nil))
    (call-interactively #'link-hint-open-link)))

(global-set-key (kbd "C-c C-SPC") 'link-hint-open-link-wrapper)
;;(global-set-key (kbd "C-c o") 'link-hint-open-link)

;;** comint-osc-button
;; Support `ls --hyperlink=always`
;;(defun link-hint--next-comint-osc-button (bound)
;;  "Find the next button.
;;Only search the range between just after the point and BOUND."
;;  ;; MAYBE look for 'category 'comint-osc-hyperlink-button.
;;  (link-hint--next-property 'browse-url-data (point-max)))

;;(defun link-hint--comint-osc-button-open (&rest args)
;;  (browse-url-button-open))

;;(link-hint-define-type 'comint-osc-button
;;                       :next #'link-hint--next-comint-osc-button
;;                       :at-point-p #'link-hint--button-at-point-p
;;                       :vars '(comint-mode shell-mode) ; any mode derived from `comint-mode'
;;                       :open #'link-hint--comint-osc-button-open
;;                       :copy #'kill-new)

;;(pushnew 'link-hint-comint-osc-button link-hint-types)

;;** file-link hacks
(setq link-hint-maybe-file-regexp (rx (or
                                       (and
                                        (or bol blank)
                                        (? (or "~" (and alpha ":")))
                                        (or (and "/" (1+ not-newline))
                                            ;; Match 'file.ext' or 'file/'. Only
                                            ;; match filenames w/o whitespace
                                            ;; since ffap doesn't handle them
                                            ;; anyway. TODO support 'a b.txt'
                                            ;; with explicit single
                                            ;; quotes (e.g. ls output).
                                            (and (1+ (not (syntax whitespace)))
                                                 (or (and "." (repeat 1 5 alnum))
                                                     "/"))
                                            ))
                                       ;; scuffed ls -l output matching
                                       (and bol (or "-" alpha) (1+ any) ; perm
                                            (1+ num) (1+ blank) ; month
                                            (1+ num) (1+ blank) ; year
                                            (1+ not-newline)))))

;; disable link-hint's file-link in dired
(pushnew 'dired-mode (get 'link-hint-file-link :not-vars))

(cl-defun link-hint--find-file-link--override (start end)
  "Return the end pos of the next filename between START and END.
Overrides `link-hint--find-file-link' since I ran into issues with that function
and it's faster to rewrite it."
  (while (re-search-forward link-hint-maybe-file-regexp end t)
    (when (ffap-file-at-point)
      (return-from link-hint--find-file-link--override
        ;;(- (point) (length filename)) ; inf loop
        (point))))
  nil)

(advice-add 'link-hint--find-file-link :override #'link-hint--find-file-link--override)

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
  (global-page-break-lines-mode))

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

(define-key compilation-mode-map (kbd "C-c RET") 'helm-make)

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
(setq next-screen-context-lines 2)       ; originally 2
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

;;* dired-virtual-mode
(add-to-list 'auto-mode-alist '("[^/]\\.dired$" . dired-virtual-mode))

;;* xdg-open-file
;; https://www.reddit.com/r/emacs/comments/cgbpvl/opening_media_files_straight_from_gnu_emacs_dired/
(defun xdg-open-file (file)
  (interactive "fxdg-open: ")
  (let ((process-connection-type nil)
        (open-script (cond ((executable-find "mimeopen") "mimeopen")
                           ((executable-find "xdg-open") "xdg-open")
                           (t (user-error "xdg-open not in PATH.")))))
    (start-process
     "" nil shell-file-name
     shell-command-switch
     (format "nohup 1>/dev/null 2>/dev/null %s %s"
             open-script (shell-quote-argument (expand-file-name file))))))

(defun dired-xdg-open-file ()
  (interactive)
  (xdg-open-file (dired-file-name-at-point)))

(define-key dired-mode-map (kbd "<M-return>") 'dired-xdg-open-file)

;;* shrink-whitespace
(global-set-key (kbd "C-c SPC") 'shrink-whitespace)
(global-set-key (kbd "C-c S-SPC") 'grow-whitespace-around)
(global-set-key (kbd "C-M-\\")  'shrink-whitespace)

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
        (insert (if (nth 3 (syntax-ppss)) ; inside string
                    key
                  (format "\"%s\"" key)))
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

(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-.") nil)
  (define-key flyspell-mode-map (kbd "C-,") nil)
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (define-key flyspell-mode-map (kbd "C-M-i") nil))

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

  (with-eval-after-load 'flyspell
    (define-key flyspell-mode-map (kbd "C-c C-f") 'hydra-flyspell/body))
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

;;** define-word-emacslient
;; emacsclient -a "" -c -n -F "((name . \"(floating) *define word*\"))" -e "(define-word-emacsclient)"
(defvar define-word-emacslient-backend 'define-word-popup)

(defun define-word-emacsclient (&optional word)
  "Define a word using a popup frame."
  (unless word (setq word (gui-get-primary-selection)))
  (set-frame-parameter nil 'width 80)
  (set-frame-parameter nil 'height 20)
  (funcall define-word-emacslient-backend word))

(defun define-word-insert (text)
  (erase-buffer)
  (insert text)
  (evil-emacs-state)
  (local-set-key "q" 'delete-frame)
  (local-set-key "<return>" 'define-word)
  (local-set-key "RET" 'define-word)
  (local-set-key "m" 'define-word-popup))

(defun define-word-popup (&optional word choose-service)
  "Same as `define-word', but insert the definition in the current buffer."
  (interactive (list "" current-prefix-arg))
  (cl-letf (((symbol-function 'define-word-displayfn)
              (lambda (service)
                (declare (ignore service))
                #'define-word-insert)))
    (switch-to-buffer (get-buffer-create "*define word*"))
    (toggle-word-wrap 1)
    (setq-local mode-line-format nil)
    (condition-case err
      (let ((word (read-string "Define word: " (and word (string-trim word)))))
        (setq-local header-line-format word)
        (define-word word define-word-default-service choose-service))
      (quit (delete-frame)))))

;;* dictionary
(setq dictionary-server "localhost")
(setq dictionary-default-dictionary "wn")
(setq dictionary-use-single-buffer t)
(setq dictionary-post-buffer-hook 'delete-other-windows)

(with-eval-after-load 'dictionary
 (define-key dictionary-mode-map (kbd "q") 'delete-frame)

 (setq define-word-emacslient-backend 'dictionary-search))

;;* dumb-jump
(require 'dumb-jump)

(setq dumb-jump-prefer-searcher 'rg)
(setq xref-search-program 'ripgrep)

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

;;(add-to-list 'which-func-functions 'show-toplevel)

(define-key prog-mode-map (kbd "C-c w") 'show-toplevel)
(define-key markdown-mode-map (kbd "C-c w") 'show-toplevel)
(define-key outline-mode-map (kbd "C-c w") 'show-toplevel)

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
             ("SPC")
             ;; TODO ("SPC" "\b")
             )
        collect (cons (string-to-char (kbd key))
                      (listify-key-sequence translation))))

(defun avy-read-char (prompt)
  (let* ((char (read-char prompt))
         (translation (assoc char avy-key-translations)))
    (if translation
        (cdr translation)
      (list char))))

(cl-defun avy-goto-char-2-special (&optional all-frames
                                     (input-1 nil skip-input-1))
  "Like `avy-goto-char-2', but translate some keys. See
`avy-key-translations'."
  (interactive "P")
  (let* ((input-1 (if skip-input-1
                      input-1
                    (avy-read-char "Char 1: ")))
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

(setf (alist-get 'avy-goto-char-2 avy-styles-alist) 'pre)
(setf (alist-get 'avy-goto-char-2-special avy-styles-alist) 'pre)

(global-set-key (kbd "C-r") 'avy-goto-char-2-special)
(define-key minibuffer-local-map (kbd "C-r") 'avy-goto-char-2-special)

;;*** evil
(with-eval-after-load 'evil
  (evil-define-command evil-avy-goto-char-2-special (&optional count)
    "Evil motion for `avy-goto-char-2-special'."
    :repeat abort :type inclusive
    :jump t :keep-visual t
    (interactive "<c>")
    (evil-without-repeat (call-interactively 'avy-goto-char-2-special))))

;;** avy-yank-sexp-1
(defun avy-yank-sexp-1 (&optional all-frames)
  "Yank a sexp to current position."
  (interactive "P")
  (let ((avy-action #'avy-action-yank))
    (avy-goto-char-2-special all-frames (list (string-to-char "(")))))
(global-set-key (kbd "C-S-R") 'avy-yank-sexp-1)
(global-set-key (kbd "C-M-y") 'avy-yank-sexp-1)

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

;;** avy-goto-symbol-in-defun
(defun avy-goto-symbol-in-defun (&optional full-window)
  "Jump to a visible symbol in current defun (with prefix arg - current window)."
  (interactive "P")
  (let* ((avy-all-windows nil)
         (bounds (unless full-window (bounds-of-thing-at-point 'defun)))
         (beg (if bounds (max (window-start) (car bounds)) (window-start)))
         (end (if bounds (min (window-end) (cdr bounds)) (window-end)))
         ;; Don't jump to symbol at point.
         (redundant (append (list (point)
                                  (1- (point))
                                  (1+ (point))
                                  (+ (point) 2)
                                  (- (point) 2))
                            (when-let ((b (bounds-of-thing-at-point 'symbol)))
                              (list (car b) (cdr b))))))
    (avy-with avy-goto-symbol-in-defun
      (avy-jump (rx symbol-start any)
                :beg beg
                :end end
                :pred (lambda () (not (member (point) redundant)))))))

(global-set-key (kbd "C-t") 'avy-goto-symbol-in-defun)
(setf (alist-get 'avy-goto-symbol-in-defun avy-styles-alist) 'pre)

;;** avy-action-yank-multiple
;; Somewhat related - repeat action for `avy-goto-char-timer':
;; https://ag91.github.io/blog/2022/04/20/repeat-with-me-avy-actions-are-awesome/
(defun avy-action-yank-multiple (pt)
  "Repeatedly yank sexp."
  (let ((avy-action #'avy-action-yank-multiple))
    (avy-action-copy pt)
    (when-let ((bnd (bounds-of-thing-at-point 'symbol)))
      (goto-char (cdr bnd)))
    (when (or (looking-at-p (rx (or symbol-start "[" "(" "{")))
              (looking-back (rx (or symbol-end "]" ")" "}"))))
      (insert " "))
    (yank)
    (avy-resume)))

(setf (alist-get (aref (kbd "C-y") 0) avy-dispatch-alist) #'avy-action-yank-multiple)
;; Default binding is ?n, which I'd rather add to avy-keys.
(setf (alist-get (aref (kbd "M-w") 0) avy-dispatch-alist) #'avy-action-copy)

;;** invisible overlays fix
;; TODO merge into avy.el
(defun avy--visible-p (s)
  (let* ((prop-and-ov (get-char-property-and-overlay s 'invisible))
         (invisible (or (car prop-and-ov) (cdr prop-and-ov))))
    (or (null invisible)
        (and (listp buffer-invisibility-spec)
             (null (assoc invisible buffer-invisibility-spec))))))

;;** fix dispatch help for multi-character keys (e.g. M-w)
(defun avy-show-dispatch-help-override ()
  "Display action shortucts in echo area."
  (message
   (apply
    #'concat
    (loop for func in avy-dispatch-alist
          collect (format "%s: %s "
                          (propertize
                           (key-description (vector (car func)))
                           'face 'aw-key-face)
                          (string-trim (symbol-name (cdr func))
                                       "avy-action-"))))))
(advice-add 'avy-show-dispatch-help :override #'avy-show-dispatch-help-override)

;;** TODO avy-action-mc
;;(defun avy-action-mc (pt)
;;  "Create a cursor at pt."
;;  (let ((existing (mc/fake-cursor-at-point pt)))
;;    (if existing
;;        (mc/remove-fake-cursor existing)
;;      (save-excursion
;;       (goto-char pt)
;;       (mc/create-fake-cursor-at-point)
;;       (mc/maybe-multiple-cursors-mode)))))

;;(setf (alist-get (aref (kbd "C-;") 0) avy-dispatch-alist) #'avy-action-mc)

;;** TODO avy + edebug (avy-action-toggle-breakpoint or smth)

;;* embark (trying it out)
;; MAYBE fix after upgrade to emacs29
;;(require 'configure-embark)

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
(define-key ctl-x-map (kbd "P") 'proced)

(add-hook 'proced-mode-hook 'hl-line-mode)

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

;;* puni (soft deletion/paredit-like commands for non-lisp buffers)
(add-hook 'prog-mode-hook #'puni-mode)
(define-key puni-mode-map (kbd "C-h") 'puni-backward-delete-char)
(define-key puni-mode-map (kbd "M-r") 'puni-raise)
(define-key puni-mode-map (kbd "M-9") 'puni-wrap-round)
(define-key puni-mode-map (kbd "M-?") 'puni-convolute)
(define-key puni-mode-map (kbd "C-M->") 'puni-slurp-forward)
(define-key puni-mode-map (kbd "C-M-<") 'puni-barf-forward)
(define-key puni-mode-map (kbd "C-c s") 'puni-split)
;;(define-key puni-mode-map (kbd "") 'puni-slurp-backward)
;;(define-key puni-mode-map (kbd "") 'puni-barf-backward)
;;(define-key puni-mode-map (kbd "") 'puni-splice)
;;(define-key puni-mode-map (kbd "") 'puni-transpose)
;;(define-key puni-mode-map (kbd "") 'puni-wrap-angle)

;;* TODO json-par
;; Need to configure keybindings to be more lispy-like.
;; (add-hook ... 'json-par-mode)

;;* favourite buffers list
(defvar favourite-buffers-list (make-list 5 nil))
(add-to-list 'savehist-additional-variables 'favourite-buffers-list)

(defun switch-to-favourite-buffer (n right override)
  (labels ((%set-favourite-buffer ()
             (setf (nth n favourite-buffers-list) (current-buffer))
             (message "Favourite buffer %s set to %s."
                      (1+ n) (buffer-name (current-buffer)))))
    (if override
        (%set-favourite-buffer)
      (if-let ((buf (nth n favourite-buffers-list)))
          ;; HACK I couldn't figure out how to display the buffer in the
          ;; left/right window. Also need to ignore side-windows somehow.
          (if (eq buf (current-buffer))
              (previous-buffer)
            (ignore-error 'user-error (if right
                                          (windmove-right)
                                        (windmove-left)))
            (switch-to-buffer buf))
        (%set-favourite-buffer)))))

(defun switch-to-favourite-buffer-1 (override)
  (interactive "P")
  (switch-to-favourite-buffer 0 nil override))
(global-set-key (kbd "M-1") 'switch-to-favourite-buffer-1)

(defun switch-to-favourite-buffer-2 (override)
  (interactive "P")
  (switch-to-favourite-buffer 1 nil override))
;;(global-set-key (kbd "M-2") 'switch-to-favourite-buffer-2)

(defun switch-to-favourite-buffer-3 (override)
  (interactive "P")
  (switch-to-favourite-buffer 2 t override))
;;(global-set-key (kbd "M-3") 'switch-to-favourite-buffer-3)

(defun switch-to-favourite-buffer-4 (override)
  (interactive "P")
  (switch-to-favourite-buffer 3 t override))
;;(global-set-key (kbd "M-4") 'switch-to-favourite-buffer-4)

(defun switch-to-favourite-buffer-5 (override)
  (interactive "P")
  (switch-to-favourite-buffer 4 t override))
;;(global-set-key (kbd "M-5") 'switch-to-favourite-buffer-5)

;;* man
(global-set-key (kbd "H-m") 'man)

;;* counsel-project
;; TODO counsel-project with prettier project list and ivy actions for
;; remove/grep/magit/find-dir/etc.
(global-set-key (kbd "H-p") 'counsel-project)
(define-key project-prefix-map "A" 'project-remember-projects-under)

(cl-defun counsel-project (&key (action 'counsel-project--magit-action actionp))
  (interactive)
  (project--ensure-read-project-list)
  (ivy-read "Project: " project--list
            :history 'counsel-project-history
            :keymap counsel-project-map
            :caller 'counsel-project
            :action action))

(defun counsel-project--rg-action (project-root)
  (let ((default-directory (car-safe project-root)))
    (call-interactively #'counsel-rg-dwim)))

(defun counsel-project--dired-action (project-root)
  (let ((default-directory (car-safe project-root)))
    (call-interactively #'dired-jump)))

(defun counsel-project--magit-action (project-root)
  (let ((default-directory (car-safe project-root)))
    (call-interactively #'magit-project-status)))

(defun counsel-project--find-file-action (project-root)
  (let ((default-directory (car-safe project-root)))
    (call-interactively #'counsel-files)))

(defun counsel-project--find-dir-action (project-root)
  (let ((default-directory (car-safe project-root)))
    (call-interactively #'project-find-dir)))

(defun counsel-project--add-project-action (project-root)
  (when-let* ((dir (read-directory-name "Remember project: "))
              (pr (project--find-in-directory dir)))
    (project-remember-project pr)))

(defun counsel-project--forget-project-action (project-root)
  (project-forget-project (car-safe project-root)))

(ivy-set-actions
 'counsel-project
 '(("d" counsel-project--find-dir-action "find dir")
   ("/" counsel-project--rg-action "ripgrep")
   ("r" counsel-project--rg-action "ripgrep")
   ("j" counsel-project--dired-action "dired")
   ("f" counsel-project--find-file-action "find file")
   ("a" counsel-project--add-project-action "add project")
   ("D" counsel-project--forget-project-action "forget project")
   ("F" counsel-project--forget-project-action "forget project")))

(defvar counsel-project-history nil)

(defvar counsel-project-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m ivy-minibuffer-map)
    m))

;; Easier navigation between args. MAYBE setup imenu as well?
(defun man-mode-setup-outline ()
  (setq-local outline-regexp "^\s+-"))
(add-hook 'Man-mode-hook 'man-mode-setup-outline)

;;* journalctl-mode
(with-eval-after-load 'journalctl-mode
  (define-key journalctl-mode-map (kbd "j") 'next-line)
  (define-key journalctl-mode-map (kbd "k") 'previous-line)
  (define-key journalctl-mode-map (kbd "w") 'forward-word)
  (define-key journalctl-mode-map (kbd "b") 'backward-word)
  (define-key journalctl-mode-map (kbd "/") 'isearch-forward-regexp)
  (define-key journalctl-mode-map (kbd "C-v") nil)

  (transient-append-suffix 'journalctl-transient "c f"
    '(journalctl-transient:--priority))

  (defun journalctl-edit-args ()
    "Edit journalctl arglist and refresh output."
    (interactive)
    (setq journalctl-current-opts
          (read--expression "Args: " (prin1-to-string journalctl-current-opts)))
    (journalctl--run journalctl-current-opts journalctl-current-chunk))

  (define-key journalctl-mode-map (kbd "<f5>") 'journalctl-edit-args))

;;* artist-mode
(pretty-hydra-define hydra-artist (:hint nil :color pink)
  ("Draw"
   (("n" (artist-select-operation "Pen") "Pen")
    ("N" (artist-select-operation "Pen Line") "Pen Line")
    ("l" (artist-select-operation "line") "line")
    ("s" (artist-select-operation "straight line") "straight line")
    ("r" (artist-select-operation "rectangle") "rectangle")
    ;;("S" (artist-select-operation "square") "square")
    ("e" (artist-select-operation "poly-line") "poly-line")
    ("E" (artist-select-operation "straight poly-line") "straight poly-line")
    ("o" (artist-select-operation "ellipse") "ellipse")
    ;;("o" (artist-select-operation "circle") "circle")
    ("t" (artist-select-operation "text see-thru") "text see-thru")
    ("T" (artist-select-operation "text-overwrite") "text-overwrite")
    ("c" (artist-select-operation "spray-can") "spray-can"))
   "Erase/Cut/Copy/Paste"
   (;;("e" (artist-select-operation "erase char") "erase char")
    ;;("E" (artist-select-operation "erase rectangle") "erase rectangle")
    ("v" (artist-select-operation "vaporize line") "vaporize line")
    ("V" (artist-select-operation "vaporize lines") "vaporize lines")
    ("x" (artist-select-operation "cut rectangle") "cut rectangle")
    ;;("X" (artist-select-operation "cut square") "cut square")
    ("y" (artist-select-operation "copy rectangle") "copy rectangle")
    ;;("Y" (artist-select-operation "copy square") "copy square")
    ("C-y" (artist-select-operation "paste") "paste")
    ("f" (artist-select-operation "flood-fill") "flood-fill"))
   "Settings/Toggles"
   (("F" #'artist-select-fill-char "Select fill char")
    ("1" #'artist-toggle-first-arrow "Toggle first arrow")
    ("2" #'artist-toggle-second-arrow "Toggle second arrow"))
   "Misc"
   (("q" nil "quit hydra" :color blue)
    ("C-q" #'artist-mode-off "quit artist" :color blue)
    ("C-z" #'undo "undo")))
  )
(with-eval-after-load 'artist
  (define-key artist-mode-map (kbd "C-c C-c") 'hydra-artist/body))
(add-hook 'artist-mode-hook 'hydra-artist/body)

(with-eval-after-load 'evil
  (add-hook 'artist-mode-hook 'evil-emacs-state))

;;* newline-and-todo
(defvar todo-keywords-alist
  '((?t . "TODO")
    (?m . "MAYBE")
    (?f . "FIXME")
    (?k . "KLUDGE")
    (?n . "NOTE")
    (?t . "TEMP")
    (?b . "BUG"))
  "Alist CHAR -> KEYWORD for fast keyword selection in `newline-and-todo'.")

(defun newline-and-todo ()
  "Insert a todo comment after newline. When called again
immediately, prompt for a todo keyword to use."
  (interactive)
  (if (and (eq last-command 'newline-and-todo)
           (looking-back (rx-to-string
                          `(and (or ,@(mapcar #'cdr todo-keywords-alist))
                                ":" (* space)))))
      (when-let* ((prompt (format "Keyword[%s]:"
                                  (concat (mapcar #'car todo-keywords-alist))))
                  (keyword (loop for ch = (read-char prompt)
                                 when (assoc ch todo-keywords-alist)
                                   return (cdr it))))
        (delete-region (match-beginning 0) (match-end 0))
        (insert (concat keyword ": ")))
    (unless (and (looking-back (rx bol (* space)))
                 (looking-at-p (rx (* space) eol)))
      (end-of-line)
      (newline-and-indent))
    (comment-dwim nil)
    (unless (looking-back " ")
      (insert " "))
    (insert "TODO: ")
    (when (fboundp 'evil-insert-state)
      (evil-insert-state))))

(global-set-key (kbd "H-t") 'newline-and-todo)

;;* insert-pair-map
;; https://dd.reddit.com/r/emacs/comments/14r48b8/weekly_tips_tricks_c_thread/jrd49gw/
(defvar insert-pair-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] #'insert-pair)
    map))

(global-set-key (kbd "C-M-(") insert-pair-map)

;;* prx macro (rx to PCRE with additional syntax)
;; stolen from Howard Abrams
;; https://www.youtube.com/watch?v=9xLeqwl_7n0
;; https://howardism.org/Technical/Emacs/eshell-why.html
;; https://github.com/howardabrams/hamacs/blob/main/ha-eshell.org#regular-expressions
;; MAYBE `rx-define' some more stuff.
(defmacro prx (&rest expressions)
  "Convert the rx-compatible regular EXPRESSIONS to PCRE.
  Most shell applications accept Perl Compatible Regular Expressions."
  `(rx-let ((integer (1+ digit))
            (float (seq integer "." integer))
            (b256 (seq (optional (or "1" "2"))
                       (regexp "[0-9]\\{1,2\\}")))
            (ipaddr (seq b256 "." b256 "." b256 "." b256))
            (time (seq digit (optional digit) ":" (= 2 digit) (optional ":" (= 2 digit))))
            (email (seq (1+ (regexp "[^,< ]")) "@" (1+ (seq (1+ (any alnum "-"))) ".") (1+ alnum)))
            (date (seq (= 2 digit) (or "/" "-") (= 2 digit) (or "/" "-") (= 4 digit)))
            (ymd (seq (= 4 digit) (or "/" "-") (= 2 digit) (or "/" "-") (= 2 digit)))
            (uuid (seq (= 8 hex) "-" (= 3 (seq (= 4 hex) "-")) (= 12 hex)))
            (guid (seq uuid)))
     (rxt-elisp-to-pcre (rx ,@expressions))))

;;* epg
;; Prompt for password in the minibuffer instead of GUI.
(setq epg-pinentry-mode 'loopback)

;;* fast-reading mode (rsvp) spray.el
(with-eval-after-load 'spray
  (when (fboundp 'evil-mode)
    (evil-set-initial-state 'spray-mode 'emacs))
  (setq spray-wpm 500))

;;* elfeed-tube
;; FIXME Doesn't work with `with-eval-after-load'. Autoload?
(when (require 'elfeed-tube nil t)
  (elfeed-tube-setup)

  (define-key elfeed-show-mode-map (kbd "C-c RET") 'elfeed-tube-mpv)
  (define-key elfeed-show-mode-map (kbd "C-c C-l") 'elfeed-tube-mpv-follow-mode)
  (define-key elfeed-show-mode-map (kbd "C-c C-w") 'elfeed-tube-mpv-where))

;;* ebook support: calibredb + nov.el + nov-xwidget
;; https://github.com/chenyanming/calibredb.el
;; https://github.com/chenyanming/nov-xwidget
;; https://depp.brause.cc/nov.el/
(setq calibredb-root-dir (expand-file-name "~/books/calibre/"))
(setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))

(with-eval-after-load 'calibredb
  (when (fboundp 'evil-mode)
    (evil-set-initial-state 'calibredb-show-mode 'emacs)
    (evil-set-initial-state 'calibredb-search-mode 'emacs)))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(with-eval-after-load 'nov
  (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
  (define-key nov-mode-map (kbd "l") 'forward-char)
  (define-key nov-mode-map (kbd "h") 'backward-char)
  (define-key nov-mode-map (kbd "SPC") 'nov-scroll-up)
  (define-key nov-mode-map (kbd "C-SPC") 'nov-scroll-down)
  (define-key nov-mode-map (kbd "S-SPC") 'nov-scroll-down)

  (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files)

  (add-hook 'nov-mode-hook 'olivetti-mode))

;;* blimp (imagemagic interface)
(add-hook 'image-mode-hook 'blimp-mode)

;;* frog-jump-buffer
(defun frog-jump-buffer-filter-redundand (buf)
  "Filters buffers that can be easily accessed elsewhere. E.g.
current-buffer, visible buffers, user-init-file, *scratch*."
  (unless (or (eq buf (current-buffer))
              (get-buffer-window buf)
              (equal (buffer-file-name buf) user-init-file)
              (eq buf (get-buffer "*scratch*")))
    t))

(setq frog-jump-buffer-default-filter 'frog-jump-buffer-filter-redundand)

(global-set-key (kbd "C-<f16>") 'frog-jump-buffer)
(global-set-key (kbd "C-M-v") 'frog-jump-buffer)


;;* keybindings
(global-unset-key (kbd "C-z"))
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
(define-key grep-mode-map (kbd "m") 'compilation-display-error)
(define-key grep-mode-map (kbd "M-m") 'compilation-display-error)
(define-key compilation-mode-map (kbd "C-x C-j") 'compilation-to-dired)
(define-key ctl-x-4-map (kbd "j") 'dired-jump-other-window)
(define-key ctl-x-5-map (kbd "j") 'dired-jump-other-frame)

(global-set-key (kbd "C-?") 'er/expand-region)
(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-,") 'er/contract-region)

(global-set-key (kbd "C-S-SPC") 'avy-goto-char-timer)
(global-set-key (kbd "M-SPC") 'avy-goto-char-timer)
(global-set-key (kbd "H-SPC") 'avy-goto-char-2-special)
(global-set-key (kbd "C-x C-SPC") 'avy-goto-char-timer)
(global-set-key (kbd "<f13>") 'avy-goto-char-timer)
(global-set-key (kbd "M-<tab>") 'other-window)
(global-set-key (kbd "<f15>") 'other-window)
;;(global-set-key (kbd "C-t") 'avy-goto-word-2)
;; (global-set-key (kbd "M-t") 'avy-goto-symbol-1)

;;** hydra-M-g
(defhydra hydra-M-g (global-map "M-g")
  "M-g"
  ("n" next-error)
  ("M-n" next-error)
  ("j" next-error)
  ("C-j" compilation-next-error)
  ("M-j" next-error)
  ("p" previous-error)
  ("M-p" previous-error)
  ("k" previous-error)
  ("C-k" compilation-previous-error)
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
(global-set-key (kbd "C-:") 'mc/mark-all-dwim)
(global-set-key (kbd "C-x m l") 'mc/edit-lines)
(global-set-key (kbd "C-x m b") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-x m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-x m r") 'mc/mark-all-in-region-regexp)
(global-set-key (kbd "C-x m SPC") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-x m h") 'mc-hide-unmatched-lines-mode)

(setq mc/insert-numbers-default 1)
(global-set-key (kbd "C-x m 1") 'mc/insert-numbers)
(global-set-key (kbd "C-x m 2") 'mc/insert-letters)

;;** dired
(global-set-key (kbd "C-x C-d") 'dired)
(define-key dired-mode-map (kbd "<backspace>") 'diredp-up-directory)
(define-key dired-mode-map (kbd "SPC") 'avy-goto-char-2-special)
(define-key dired-mode-map (kbd "C-h") 'dired-up-directory)
(define-key dired-mode-map (kbd "C-w") 'dired-up-directory)
(define-key dired-mode-map (kbd "C-t") 'avy-goto-word-or-subword-1)
(define-key dired-mode-map (kbd "<tab>") 'dired-subtree-toggle)
(define-key dired-mode-map (kbd "<backtab>") 'dired-subtree-cycle)
(define-key dired-mode-map (kbd "i") 'dired-toggle-read-only)
(define-key dired-mode-map (kbd "I") 'dired-subtree-remove)
(define-key dired-mode-map (kbd "M-z") nil)
(define-key dired-mode-map (kbd "M-c") nil)
(define-key dired-mode-map (kbd "L") 'dired-do-symlink)
(define-key dired-mode-map (kbd "C-f") 'forward-char)
(define-key dired-mode-map (kbd "C-k") 'dired-do-kill-lines)
(define-key dired-mode-map (kbd "C-c C-l") 'dired-do-load)
(define-key dired-mode-map (kbd "C-c C-b") 'dired-do-byte-compile)

(define-key dired-mode-map (kbd "j") 'dired-next-line)
(define-key dired-mode-map (kbd "k") 'dired-previous-line)
(define-key dired-mode-map (kbd "M-j") nil)
(define-key dired-mode-map (kbd "M-k") nil)

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
  ("d" #'describe-char "describe-char")
  ("D" #'shortdoc-display-group "shortdoc-display-group")
  ("e" #'toggle-debug-on-error "toggle-debug-on-error")
  ("E" #'ediff-buffers "ediff buffers")
  ("f" #'describe-face "describe-face")
  ("F" #'counsel-faces "counsel-faces")
  ("M-f" #'face-attributes-pretty-print "pp face attributes")
  ("g" #'magit-list-repositories "Magit list repositories")
  ("i" #'ielm "ielm")
  ("j" #'json-pretty-print-dwim "Json pretty print")
  ("J" #'counsel-jq "counsel-jq")
  ("k" #'helpful-key "Describe key")
  ("K" #'free-keys "Free keys in current buffer")
  ("l" #'org-store-link "Org store link")
  ("L" #'display-line-numbers-mode "Display line numbers mode")
  ("m" #'mu4e "mu4e")
  ("M" #'mu4e-compose-new "mu4e compose")
  ("M-m" #'memory-report "Memory report")
  ("o" #'helpful-symbol "Describe symbol")
  ("O" #'org-web-tools-read-url-as-org "Read url in org")
  ("p" #'counsel-package "counsel-package")
  ("P" #'list-processes "list-processes")
  ("q" nil "quit")
  ("r" #'rename-file-and-buffer "rename-file-and-buffer")
  ("R" #'spray-mode "spray-mode (rsvp)")
  ("s" #'string-edit-at-point "string-edit-at-point")
  ("t" #'tldr "TLDR")
  ("T" #'orgtbl-mode "org table minor mode")
  ("v" #'counsel-describe-variable "Describe variable")
  ("w" #'whitespace-mode "whitespace-mode")
  ("W" #'delete-trailing-whitespace "delete-trailing-whitespace")
  ("y" #'elfeed-tube-fetch "elfeed-tube-fetch")
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
(global-set-key (kbd "C-h") 'backward-delete-char)
;;(define-key text-mode-map (kbd "C-h") 'backward-delete-char)
;;(define-key comint-mode-map (kbd "C-h") 'backward-delete-char)
;;(define-key minibuffer-local-map (kbd "C-h") 'backward-delete-char)

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
(global-set-key (kbd "H-D") 'shortdoc-display-group)

;;** comint
(define-key comint-mode-map (kbd "C-c C-x") nil)
(with-eval-after-load 'sh-script
  (define-key sh-mode-map (kbd "C-c C-x") nil))

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
(define-key Info-mode-map (kbd "H") 'Info-history-back)
(define-key Info-mode-map (kbd "<mouse-8>") 'Info-history-back)
(define-key Info-mode-map (kbd "<mouse-9>") 'Info-history-forward)

;;** registers
;;*** dwim registers
(defun register-dwim (register)
  "Insert, jump or store to a register depending on its contents."
  (interactive (list (register-read-with-preview "DWIM register: ")))
  (if-let ((content (get-register register)))
      (typecase content
        ;; TODO rectangle selection is a cons - handle it somehow
        ((or string number)
         (insert-register register t))
        ((or marker cons)
         (jump-to-register register))
        (otherwise
         (user-error "Don't know how to handle register %s of type %s. Content: %s."
                     (single-key-description register)
                     (type-of content) content)))
    ;;(user-error "Register %s is empty." (single-key-description register))
    (save-to-register-dwim register)))

(defun save-to-register-dwim (register &optional arg)
  "With no prefix arg and region active call `copy-to-register',
otherwise forward to `point-to-register'."
  (interactive (list (register-read-with-preview
                      (format "Save %s to register: "
                              (if (region-active-p) "region" "point")))
                     current-prefix-arg))
  (cond ((and (not arg) (region-active-p))
         (copy-to-register register (region-beginning) (region-end)
                           nil t)
         (message "Region copied to register %s."
                  (single-key-description register)))
        (t
         (point-to-register register arg)
         (message "Point%s stored in register %s."
                  (if arg " and frame configuration" "")
                  (single-key-description register)))))

(define-key global-map (kbd "H-`") 'save-to-register-dwim)
(define-key global-map (kbd "H--") 'counsel-register)

(defun delete-register (register)
  (setf (alist-get register register-alist nil t) nil))

(defun ivy-action--delete-register (ivy-item)
  (when-let ((register (get-text-property 0 'register ivy-item)))
    (delete-register register)))

(with-eval-after-load 'counsel
  (ivy-add-actions
   'counsel-register
   '(("d" ivy-action--delete-register "delete register"))))

;;*** quick registers
(defmacro define-quick-key-register (map key register)
  (let* ((str (string register))
         (name (make-symbol
                (concat "quick-register-" str))))
    `(progn
       (defun ,name (&optional store)
         ,(format "Call `register-dwim' on a register %s.\
 With universal arg call `save-to-register-dwim' instead." str)
         (interactive "P")
         (if store
             (save-to-register-dwim ,register)
           (register-dwim ,register)))
       (define-key ,map ,key ',name))))

;; H-0..9
(define-quick-key-register global-map (kbd "H-0") ?0)
(define-quick-key-register global-map (kbd "H-1") ?1)
(define-quick-key-register global-map (kbd "H-2") ?2)
(define-quick-key-register global-map (kbd "H-3") ?3)
(define-quick-key-register global-map (kbd "H-4") ?4)
(define-quick-key-register global-map (kbd "H-5") ?5)
(define-quick-key-register global-map (kbd "H-6") ?6)
(define-quick-key-register global-map (kbd "H-7") ?7)
(define-quick-key-register global-map (kbd "H-8") ?8)
(define-quick-key-register global-map (kbd "H-9") ?9)
;; C-H-a..z
(loop for register from ?a to ?z
      for key-hyper = (kbd (concat "C-H-" (string register)))
      do (eval `(define-quick-key-register global-map ,key-hyper ,register))
      for key-super = (kbd (concat "s-" (string register)))
      do (eval `(define-quick-key-register global-map ,key-super ,register)))

;;*** jump-to-next-register
(defvar last-visited-register nil)

(defun jump-to-next-register (&optional clear-last-visited)
  "Jumps to next register that contains a buffer position.
With prefix arg clear `last-visited-register' instead."
  (interactive "P")
  (when register-alist
    (if clear-last-visited
        (if (null last-visited-register)
            (message "last-visited-register is nil")
          (setf (alist-get last-visited-register register-alist nil 'remove)
                nil)
          (message "Cleared register %s" (char-to-string last-visited-register))
          ;; MAYBE set `last-visited-register' to the one before last.
          (setq last-visited-register nil))
      (let* ((found (null last-visited-register))
             (register
               (loop for (register . val) in register-alist
                     do (cond ((eq register last-visited-register)
                               (setq found t))
                              ((and found
                                    (or (markerp val)
                                        (eq (car-safe val) 'file-query)))
                               (setq last-visited-register register)
                               (return register))))))
        (if register
            (progn (message "Register %s" (char-to-string register))
                   (jump-to-register register))
          (setq last-visited-register nil)
          (jump-to-next-register))))))

(global-set-key (kbd "<f16>") 'jump-to-next-register)

;;** mouse events
;;*** increase/decrease-number-at-mouse (similar to acme)
(defmacro with-event-point (event &rest body)
  "Execute BODY with window and point temporarily set to that of
EVENT."
  (let ((event-pos (gensym "event-pos")))
    `(when-let ((,event-pos (event-start ,event)))
       (with-selected-window (posn-window ,event-pos)
         (save-excursion
          (goto-char (posn-point ,event-pos))
          ,@body)))))

(defun increase-number-at-mouse (e)
  (interactive "e")
  (with-event-point e
    (org-increase-number-at-point)))

(defun decrease-number-at-mouse (e)
  (interactive "e")
  (with-event-point e
    (org-decrease-number-at-point)))

(global-unset-key (kbd "C-<down-mouse-1>"))
(global-unset-key (kbd "C-<down-mouse-3>"))
(global-set-key (kbd "C-<mouse-1>") 'increase-number-at-mouse)
(global-set-key (kbd "C-<mouse-3>") 'decrease-number-at-mouse)

;;*** toggle multiple cursors on click
(global-unset-key (kbd "C-<down-mouse-2>"))
(global-set-key (kbd "C-<mouse-2>") 'mc/toggle-cursor-on-click)

;;** archive mode
(define-key archive-mode-map (kbd "j") 'archive-next-line)
(define-key archive-mode-map (kbd "k") 'archive-previous-line)

;;** profiler
(with-eval-after-load 'profiler
 (define-key profiler-report-mode-map (kbd "C-j") 'profiler-report-find-entry)
 ;; TODO m = other window
 (define-key profiler-report-mode-map (kbd "m") 'xref-find-definitions-other-window)
 (define-key profiler-report-mode-map (kbd "o") 'xref-find-definitions-other-window)
 (define-key profiler-report-mode-map (kbd "j") 'profiler-report-next-entry)
 (define-key profiler-report-mode-map (kbd "k") 'profiler-report-previous-entry))

;;* toggles
(define-key ctl-x-map (kbd "x w") 'toggle-word-wrap)
(define-key ctl-x-map (kbd "x s") 'toggle-scroll-bar)
(define-key ctl-x-map (kbd "x m") 'toggle-enable-multibyte-characters)

;;* TODO bind something to "C-'"
