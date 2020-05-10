;; -*- lexical-binding: t -*-
;; packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(setq package-enable-at-startup nil)
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
(require 'dired-subtree)
(require 'dired-rsync)
(require 'expand-region)
(require 'helpful)
(require 'hl-todo)
(require 'ivy-xref)
(require 'multiple-cursors)
(require 'paredit)
(require 'pcmpl-args)
(require 'string-edit)
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
(require 'configure-git)
(require 'configure-go-lsp)
(require 'configure-highlight)
(require 'configure-isearch)
(require 'configure-ivy)
(require 'counsel-ripgrep)
;; (require 'configure-go)
(require 'configure-lisp)
(require 'configure-python)


(delete-selection-mode 1)
;; (global-linum-mode t)
;; (global-display-line-numbers-mode t)
(global-hl-todo-mode 1)
(dired-async-mode t)

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
      avy-style 'pre ;; 'de-bruijn
      avy-keys (list ?f ?c ?d ?g ?s ?a ?e ?v)
      lispy-avy-keys avy-keys
      view-read-only t
      slime-description-autofocus t
      xref-show-xrefs-function #'ivy-xref-show-xrefs
      compilation-scroll-output t
      initial-major-mode 'emacs-lisp-mode
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


;;* equake shell
(require 'equake)

;; show current directory in tab name
(cl-defun equake-add-current-dir-to-tab-name (&optional (dir default-directory))
  "Rename equake tab buffer to display current directory."
  (when equake-mode
    (let* ((regex (rx line-start (* anything)
                      (group "%" (? (or "~" "/" ".") (* anything) (? "/") "::"))
                      (* anything)))
           (template
             (replace-regexp-in-string regex "%%%s::" (buffer-name) nil nil 1)))
      (rename-buffer (format template dir)))))

(defun equake-default-directory-watcher (symbol new-value operation buffer)
  (when buffer
    (with-current-buffer buffer
      (equake-add-current-dir-to-tab-name new-value))))

(add-variable-watcher 'default-directory #'equake-default-directory-watcher)
(advice-add #'equake-new-tab :after #'equake-add-current-dir-to-tab-name)
(advice-add #'equake-rename-etab :after #'equake-add-current-dir-to-tab-name)

;; remove spaces from tab-name: [ tab-name ] -> [tab-name]
(defun equake-fix-tab-names-advice (tab-name)
  (replace-regexp-in-string "\\[ " "[" (replace-regexp-in-string " \\]" "]" tab-name)))

(advice-add #'equake-extract-format-tab-name :filter-return #'equake-fix-tab-names-advice)

;; add ace-window-path to modeline
(defun equake-modeline-add-ace-window-lighter-advice (modeline)
  (cons `(:eval (ace-window-path-lighter)) modeline))

(advice-add #'equake-mode-line :filter-return #'equake-modeline-add-ace-window-lighter-advice)

;; equake-kill-tab
(defun equake-kill-tab ()
  (interactive)
  (let ((buff (current-buffer)))
    (if (< (equake-count-tabs (equake-get-monitor-name) (buffer-list) 0) 2)
        (delete-window)
      (equake-prev-tab))
    (kill-buffer buff)))

(define-key equake-mode-map (kbd "C-Q") 'equake-kill-tab)

;; modeline colors
(face-spec-set 'equake-tab-inactive '((t (:foreground "gray70" :background "black"))))
(face-spec-set 'equake-tab-active '((t (:foreground "black" :background "gray70" :weight bold))))
(face-spec-set 'equake-shell-type-eshell '((t (:foreground "white" :background "black"))))
(face-spec-set 'equake-shell-type-term '((t (:foreground "white" :background "black"))))
(face-spec-set 'equake-shell-type-rash '((t (:foreground "white" :background "black"))))
(face-spec-set 'equake-shell-type-shell '((t (:foreground "white" :background "black"))))

;; equake-pop
(defun equake-pop ()
  "Open equake tab for current directory in other window."
  (interactive)
  (if-let* ((dir default-directory)
            (tab (find-if (lambda (buffer)
                            (with-current-buffer buffer
                              (and equake-mode
                                   (string= default-directory dir))))
                          (buffer-list)))
            (pop-up-windows t))
      (pop-to-buffer tab t)
    (select-window (or (split-window-sensibly) (split-window)))
    (equake-new-tab)))

(define-key equake-mode-map (kbd "<f12>") 'quit-window)
(global-set-key (kbd "<f12>") 'equake-pop)

;;
(add-hook 'prog-mode-hook (lambda () (setq-local show-trailing-whitespace t)))

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

;;** tab key hacks
(setq tab-always-indent 'complete)

;; Distinguish C-i and keyboard tab key
(define-key input-decode-map (kbd "C-i") [C-i])

(global-set-key (kbd "<C-i>") 'hippie-expand)
(global-set-key (kbd "TAB") 'indent-for-tab-command)

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

;;* dired-jump-other-frame
(defun dired-jump-other-frame (&optional file-name)
  "Like \\[dired-jump] (`dired-jump') but in other frame."
  (interactive
   (list (and current-prefix-arg
	      (read-file-name "Jump to Dired file: "))))
  (let ((pop-up-frames t))
    (dired-jump t file-name)))

;;* org
(setq org-default-notes-file (concat org-directory "/notes.org")
      org-startup-indented t
      org-hide-leading-stars t)

(advice-add 'org-archive-default-command :after #'org-save-all-org-buffers)

(global-set-key (kbd "<f6>") 'counsel-org-capture)

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

;; TODO: figure out how to add snippets to completion candidates
;; and how `completion-extra-properties' work
;; (defun yas-completion-at-point ()
;;   (let ((bounds (bounds-of-thing-at-point 'symbol)))
;;     (list (or (car bounds) (point))
;;           (point)
;;           (yas--table-hash (car (yas--get-snippet-tables)))
;;           (list :exclusive 'no :exit-function #'yas--completion-exit-function))))

(defun yas--completion-exit-function (string status)
  (yas-expand))

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
(cl-defun pulse-cursor (&key (width 1) (face 'cursor) (delay .05))
  (let* ((pulse-delay delay)
         (pos (point))
         (beg (max (line-beginning-position) (- pos width)))
         (end (min (line-end-position) (+ pos width))))
    (pulse-momentary-highlight-region beg end face)))

(cl-defun pulse-current-line (&key (face 'cursor) (delay .02))
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

;;* keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-<tab>") 'completion-at-point)

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

(global-set-key (kbd "C-S-SPC") 'avy-goto-char-timer)
(global-set-key (kbd "M-SPC") 'avy-goto-char-timer)
(global-set-key (kbd "M-<tab>") 'other-window)
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

;;
(global-set-key (kbd "<f5>") 'revert-buffer)

;;** Hydra for random useful commands.
(defhydra hydra-cantrips-prefix-map (:columns 1 :exit t)
  "cantrips"
  ("s" #'string-edit-at-point "string-edit-at-point")
  ("p" #'counsel-package "counsel-package")
  ("r" #'rename-file-and-buffer "rename-file-and-buffer")
  ("d" #'describe-text-properties "describe-text-properties")
  ("f" #'describe-face "describe-face")
  ("F" #'counsel-faces "counsel-faces")
  ("i" #'ielm "ielm")
  ("/" #'rg-dwim "rg-dwim")
  ("b" #'counsel-descbinds "counsel-descbinds")
  ("e" #'toggle-debug-on-error "toggle-debug-on-error")
  ("=" #'diff-current-buffer-with-file "diff-current-buffer-with-file")
  ("c" #'counsel-colors-emacs "counsel-colors-emacs")
  ("C" #'rainbow-mode "rainbow-mode")
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

;;** Helpful
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h o") #'helpful-symbol)
