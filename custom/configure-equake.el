(require 'equake)


;;* update modeline when default-directory changes
(defun equake-default-directory-watcher (symbol new-value operation buffer)
  (when (equake--tab-p buffer)
    (with-current-buffer buffer
      (let ((default-directory new-value))
        (equake--update-mode-line (equake--get-tab-property 'monitor buffer))))))

(add-variable-watcher 'default-directory #'equake-default-directory-watcher)

;;* equake-kill-tab
(defun equake-kill-tab ()
  (interactive)
  (let* ((buff (current-buffer))
         (monitor (equake--get-monitor))
         (tabs (alist-get monitor equake--tab-list)))
    (if (< (length tabs) 2)
        (delete-window)
      (equake-prev-tab))
    (kill-buffer buff)))

(define-key equake-mode-map (kbd "C-c C-q") 'equake-kill-tab)

;;* modeline colors
(face-spec-set 'equake-tab-inactive '((t (:foreground "gray70" :background "black"))))
(face-spec-set 'equake-tab-active '((t (:foreground "black" :background "gray70" :weight bold))))
(face-spec-set 'equake-shell-type-eshell '((t (:foreground "white" :background "black"))))
(face-spec-set 'equake-shell-type-term '((t (:foreground "white" :background "black"))))
(face-spec-set 'equake-shell-type-rash '((t (:foreground "white" :background "black"))))
(face-spec-set 'equake-shell-type-shell '((t (:foreground "white" :background "black"))))

;;* equake-pop
(setq equake-default-shell 'shell)

(defun equake-pop ()
  "Open equake tab for current directory in other window."
  (interactive)
  (if-let* ((dir default-directory)
            (tab (find-if (lambda (buffer)
                            (with-current-buffer buffer
                              (and equake-mode
                                   (string= default-directory dir))))
                          (buffer-list))))
      (pop-to-buffer tab)
    (equake-new-tab)))

(define-key equake-mode-map (kbd "<f12>") 'delete-window)
(global-set-key (kbd "<f12>") 'equake-pop)

;;* hacks to make modeline nicer
;;** show CWD in tab name
(defun equake--format-tab-override (tab)
  (-let* ((tab-name
           (abbreviate-file-name
            (buffer-local-value 'default-directory tab)))
          (tab-active-p (eq tab (current-buffer)))
          (face (if tab-active-p 'equake-tab-active 'equake-tab-inactive)))
    (propertize (concat "[" tab-name "]") 'font-lock-face face)))

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
      (setq format
            (list `(:eval (ace-window-path-lighter)) format)))
    (setq mode-line-format format)
    (force-mode-line-update)))

(advice-add 'equake--update-mode-line :override #'equake--update-mode-line-override)

;;* fix for 'shell
(defun equake--launch-shell-around (fn launchshell)
  (if (eq launchshell 'shell)
      (shell)
    (funcall fn launchshell)))

(advice-add 'equake--launch-shell :around #'equake--launch-shell-around)

;;* equake-kill-tab: don't ask for confirmation
(defun equake-kill-tab-advice ()
  (when-let ((proc (get-buffer-process (current-buffer))))
    (set-process-query-on-exit-flag proc nil)))

(advice-add 'equake-kill-tab :before #'equake-kill-tab-advice)


(provide 'configure-equake)
