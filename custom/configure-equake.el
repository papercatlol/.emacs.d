(require 'equake)

;;* kill frames instead of hiding (otherwise they lose floating in i3wm)
(setq equake-use-frame-hide nil)

;;* update modeline when default-directory changes
(defun equake-default-directory-watcher (symbol new-value operation buf)
  (when (equake--tab-p buf)
    (with-current-buffer buf
      (let ((default-directory new-value))
        (equake--update-mode-line (equake--get-tab-property 'monitor buf))))))

(add-variable-watcher 'default-directory #'equake-default-directory-watcher)

;;* equake-kill-tab
(defun equake-kill-tab ()
  (interactive)
  (let* ((buff (current-buffer))
         (monitor (equake--get-monitor))
         (tabs (alist-get monitor equake--tab-list)))
    (if (< (length tabs) 2)
        (quit-window)
      (equake-prev-tab))
    (kill-buffer buff)))

(define-key equake-mode-map (kbd "C-c C-q") 'equake-kill-tab)
(define-key equake-mode-map (kbd "C-M-_") 'nil)
(define-key equake-mode-map (kbd "C-M-+") 'nil)

;;* equake-pop
(setq equake-default-shell 'shell)

(defun equake-pop (&optional new-tab initial-input)
  "Pop to equake buffer. With prefix arg open a new equake tab.
With double prefix arg override the default shell type with vterm."
  (interactive "P")
  (cond ((= 16 (prefix-numeric-value current-prefix-arg))
         (equake-new-tab 'vterm))
        ((or new-tab (null (equake-find-buffer)))
         (equake-new-tab))
        (t
         (if-let ((buf (or (equake-find-visible-buffer)
                           (equake-find-buffer))))
             (pop-to-buffer buf)
           (when-let* ((dir default-directory)
                       (tab (equake-find-buffer
                             (lambda (buf)
                               (string= (buffer-local-value 'default-directory buf)
                                        dir)))))
             (pop-to-buffer tab)))))
  (when initial-input
    (goto-char (point-max))
    (insert initial-input)))

(defun equake-pop-and-yank (&optional new-tab)
  "Pop to equake buffer and yank active region or current line.
With prefix arg open a new equake tab."
  (interactive "P")
  (equake-pop new-tab (or (thing-at-point 'region)
                          ;; No (thing-at-point 'line) as it grabs newline.
                          (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position)))))

(define-key equake-mode-map (kbd "<f12>") 'quit-window)
(global-set-key (kbd "<f12>") 'equake-pop)
(global-set-key (kbd "S-<f12>") 'equake-pop-and-yank)

;;* equake-new-tab-dwim
(defun equake-new-tab-dwim (&optional shell)
  "Like `equake-new-tab', but query for shell type with prefix arg."
  (interactive (when current-prefix-arg
                 (list (intern
                        (completing-read "Choose shell: "
                                         equake-available-shells
                                         nil t nil nil
                                         (symbol-name equake-default-shell))))))
  (equake-new-tab shell))

(define-key equake-mode-map [remap equake-new-tab] 'equake-new-tab-dwim)

;;* utils for finding equake buffers
(defun equake-buffer-p (buf)
  "Check if BUF as `equake-mode' enabled."
  (buffer-local-value 'equake-mode buf))

(defun equake-find-buffer (&optional predicate)
  "Find an equake buffer."
  (cl-loop for buf in (buffer-list)
        when (and (equake-buffer-p buf)
                  (or (null predicate)
                      (and (functionp predicate)
                           (funcall predicate buf))))
        return buf))

(defun equake-find-visible-buffer (&optional return-window)
  "Find an equake buffer among windows on current frame."
  (cl-loop for win in (window-list)
        for buf = (window-buffer win)
        when (equake-buffer-p buf)
        return (if return-window win buf)))

;;* hacks to make modeline nicer
;;** show CWD in tab name
(defun equake--format-tab-override (tab)
  (-let* ((tab-name
           (abbreviate-file-name
            (buffer-local-value 'default-directory tab)))
          (tab-active-p (eq tab (current-buffer)))
          ;; TODO make inactive tabs change bg with (in)active modeline
          (face (if tab-active-p 'equake-tab-active 'equake-tab-inactive))
          (shell-type (let* ((mode (buffer-local-value 'major-mode tab))
                             (shell-name (string-remove-suffix
                                          "-mode" (symbol-name mode))))
                        ;; HACK: This relies on the convention that SHELLNAME
                        ;; buffers will be in SHELLNAME-mode major mode.
                        (if (equal (symbol-name equake-default-shell) shell-name)
                            "" (concat shell-name ":")))))
         (propertize (concat "[" shell-type tab-name "]") 'font-lock-face face)))

(advice-add 'equake--format-tab :override #'equake--format-tab-override)

;;** less spacing, fix background color, don't show shell type
(defun equake--update-mode-line-override (monitor)
  (let* ((etab-list (alist-get monitor equake--tab-list))
         (separator (propertize " " 'font-lock-face 'equake-tab-inactive))
         (tabs-part (mapconcat #'equake--format-tab etab-list separator))
         (format (if equake-show-monitor-in-mode-line
                     (format "%s: %s" monitor tabs-part)
                   tabs-part)))
    (when (fboundp 'ace-window)
      (setq format (list `(:eval (ace-window-path-lighter)) format)))
    ;;(setq header-line-format format)
    (setq mode-line-format format)
    (force-mode-line-update)))

(advice-add 'equake--update-mode-line :override #'equake--update-mode-line-override)

;;* fix for 'shell and buffer pop-ups
(defun equake--launch-shell-around (fn launchshell)
  ;; Select visible equake window, but remember original default-directory.
  (let ((dir default-directory))
    (unless (bound-and-true-p equake-mode)
      (when-let ((win (equake-find-visible-buffer t)))
        (select-window win)))
    ;; We don't want to popup new windows if we're in an equake window already.
    (let ((display-buffer-alist
            (if (bound-and-true-p equake-mode)
                '((".*" (display-buffer-same-window display-buffer-in-side-window)))
              display-buffer-alist)))
      (let ((default-directory dir))
        ;; Equake tries to delete other windows when launching `shell'.
        (if (eq launchshell 'shell)
            (shell)
          (funcall fn launchshell))))))

(advice-add 'equake--launch-shell :around #'equake--launch-shell-around)

;;* equake-kill-tab: don't ask for confirmation
(defun equake-kill-tab-advice ()
  (when-let ((proc (get-buffer-process (current-buffer))))
    (set-process-query-on-exit-flag proc nil)))

(advice-add 'equake-kill-tab :before #'equake-kill-tab-advice)

;;* bookmark support
(defun bookmark-make-record-equake-shell ()
  `(,default-directory
    (filename . ,default-directory)
    (handler . equake-shell-bookmark-jump)))

(defun equake-shell-bookmark-jump (bmk-record)
  (let ((default-directory (bookmark-get-filename bmk-record)))
    (equake-pop t)))

(defun equake-enable-bookmarks ()
  (setq-local bookmark-make-record-function #'bookmark-make-record-equake-shell))

(add-hook 'equake-mode-hook #'equake-enable-bookmarks)

;;* HACK make shell understand ..+ aliases
(defun shell-directory-tracker-handle-up-dir-alias (old-fn str)
  "A hack to make shell understand ..+ aliases."
  (if (string-match (rx bol (* blank) (group "." (1+ ".")) (* blank) eol) str)
      (let* ((match (match-string-no-properties 1 str))
             (cmd (concat ".." (string-replace "." "/.." (subseq match 2)))))
        (shell-process-cd cmd))
    (funcall old-fn str)))

(advice-add 'shell-directory-tracker :around
            #'shell-directory-tracker-handle-up-dir-alias)

;;* compilation-shell-minor-mode
(add-hook 'shell-mode-hook #'compilation-shell-minor-mode)
(define-key compilation-shell-minor-mode-map (kbd "C-c j") 'compilation-next-error)
(define-key compilation-shell-minor-mode-map (kbd "C-c C-j") 'compilation-next-error)
(define-key compilation-shell-minor-mode-map (kbd "C-c k") 'compilation-previous-error)
(define-key compilation-shell-minor-mode-map (kbd "C-c C-k") 'compilation-previous-error)
(define-key compilation-shell-minor-mode-map (kbd "M-<return>") nil)
(define-key compilation-shell-minor-mode-map (kbd "M-RET") nil)
(define-key compilation-shell-minor-mode-map (kbd "C-c C-m") 'compile-goto-error)
(define-key compilation-shell-minor-mode-map (kbd "M-m") 'compilation-display-error)

;; MAYBE try `dirtrack-mode' instead of `shell-dirtrack-mode'
;; https://www.masteringemacs.org/article/running-shells-in-emacs-overview

;;* shell-change-dir
(defun shell-change-dir (dir)
  "Send 'cd DIR' to the current shell process."
  (interactive
   (list (read-directory-name "Change directory: "
                              default-directory default-directory t)))
  (when-let ((proc (get-buffer-process (current-buffer))))
    (comint-send-string proc (format "cd \"%s\"\n" (expand-file-name dir)))
    (shell-process-cd dir)))

(define-key shell-mode-map (kbd "C-c C-k") 'comint-send-eof) ; previous binding
(define-key shell-mode-map (kbd "C-c C-d") 'shell-change-dir)

;;* dired-shell-cd
(defun dired-shell-cd ()
  (interactive)
  (let ((shell-buffer
          (loop for window in (window-list)
                for buf = (window-buffer window)
                when (eq 'shell-mode
                         (buffer-local-value 'major-mode buf))
                  return buf))
        (dir default-directory))
    (unless shell-buffer
      (user-error "No shell buffer in current window."))
    (with-current-buffer shell-buffer
      (shell-change-dir dir))))

(define-key dired-mode-map (kbd "C-c C-d") 'dired-shell-cd)


;;* coterm (terminal emulation for comint)
(coterm-mode)
(add-hook 'coterm-mode-hook 'coterm-auto-char-mode)

(with-eval-after-load 'comint
  (defun coterm-char-mode-cycle-and-echo ()
    (interactive)
    (coterm-char-mode-cycle)
    (message "Coterm: %s."
             (cond (coterm-auto-char-mode 'coterm-auto-char-mode)
                   (coterm-char-mode 'coterm-char-mode)
                   (t "default mode"))))

  (define-key comint-mode-map (kbd "<f11>") 'coterm-char-mode-cycle-and-echo))

;;* copy-to-equake
(defun copy-to-equake (text)
  (interactive (list (when-let ((bounds (if (region-active-p)
                                            (region-bounds)
                                          (bounds-of-thing-at-point 'defun))))
                       (buffer-substring-no-properties
                        (car bounds) (cdr bounds)))))
  (equake-pop)
  (goto-char (point-max))
  (insert text))

(with-eval-after-load 'sh-script
  (define-key sh-mode-map (kbd "C-c C-y") 'copy-to-equake))

;;* capf-autosuggest
(defun maybe-capf-autosuggest-mode ()
  "Enable `capf-autosuggest-mode' unless we're in a remote directory."
  (unless (file-remote-p default-directory)
    (capf-autosuggest-mode 1)))

(add-hook 'comint-mode-hook #'maybe-capf-autosuggest-mode)

(with-eval-after-load 'capf-autosuggest
  (setq capf-autosuggest-dwim-next-line nil)

  (capf-autosuggest-define-partial-accept-cmd capf-autosuggest-lispy-move-end-of-line lispy-move-end-of-line)
  (define-key capf-autosuggest-active-mode-map (kbd "C-e") 'capf-autosuggest-lispy-move-end-of-line))

;;* vterm
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-h") 'vterm-send-backspace))


(provide 'configure-equake)
