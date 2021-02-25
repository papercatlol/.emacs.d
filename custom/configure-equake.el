(require 'equake)

;; show current directory in tab name
(cl-defun equake-add-current-dir-to-tab-name (&optional (dir default-directory))
  "Rename equake tab buffer to display current directory."
  (when equake-mode
    (let* ((regex (rx line-start (* anything)
                      (group "%" (? (or "~" "/" ".") (* anything) (? "/")))
                      (* anything)))
           (template
             (replace-regexp-in-string regex "%%%s" (buffer-name) nil nil 1)))
      (rename-buffer (format template dir)))))

(defun equake-default-directory-watcher (symbol new-value operation buffer)
  (when buffer
    (with-current-buffer buffer
      (equake-add-current-dir-to-tab-name new-value))))

(add-variable-watcher 'default-directory #'equake-default-directory-watcher)
(advice-add #'equake-new-tab :after #'equake-add-current-dir-to-tab-name)
(advice-add #'equake-rename-etab :after #'equake-add-current-dir-to-tab-name)

;; add ace-window-path to modeline
(defun equake-modeline-add-ace-window-lighter-advice (modeline)
  (cons `(:eval (ace-window-path-lighter)) modeline))

(advice-add #'equake-mode-line :filter-return #'equake-modeline-add-ace-window-lighter-advice)

;; equake-kill-tab
(defun equake-kill-tab ()
  (interactive)
  (let ((buff (current-buffer)))
    (if (< (equake--count-tabs (equake-get-monitor-name)) 2)
        (delete-window)
      (equake-prev-tab))
    (kill-buffer buff)))

(define-key equake-mode-map (kbd "C-c C-q") 'equake-kill-tab)

;; modeline colors
(face-spec-set 'equake-tab-inactive '((t (:foreground "gray70" :background "black"))))
(face-spec-set 'equake-tab-active '((t (:foreground "black" :background "gray70" :weight bold))))
(face-spec-set 'equake-shell-type-eshell '((t (:foreground "white" :background "black"))))
(face-spec-set 'equake-shell-type-term '((t (:foreground "white" :background "black"))))
(face-spec-set 'equake-shell-type-rash '((t (:foreground "white" :background "black"))))
(face-spec-set 'equake-shell-type-shell '((t (:foreground "white" :background "black"))))

;; equake-pop
(setq equake-default-shell 'shell)
;; TODO: make it work with `shell'
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

;; PATCH: don't propertize inactive tab names
(defun equake-extract-format-tab-name* (tab-no)
  "Extract name of an Equake tab #TAB-NO and format it for the modeline."
  (-let* (((&alist 'monitor 'tab-no active-tab-no) (equake--get-tab-properties))
          (tab (equake--find-tab monitor tab-no))
          ((&alist 'tab-name) (equake--get-tab-properties tab))
          (tab-active-p (equal tab-no active-tab-no))
          (face (and tab-active-p 'equake-tab-active)))
    (when (string-empty-p tab-name)
      (setq tab-name (number-to-string tab-no))) ; set name to tab number
    (concat (propertize (concat "[" tab-name "]") 'font-lock-face face) " ")))
(advice-add 'equake-extract-format-tab-name :override #'equake-extract-format-tab-name*)

(defun equake-kill-tab-advice ()
  (when-let ((proc (get-buffer-process (current-buffer))))
    (set-process-query-on-exit-flag proc nil)))

(advice-add 'equake-kill-tab :before #'equake-kill-tab-advice)

(define-key equake-mode-map (kbd "<f12>") 'delete-window)
(global-set-key (kbd "<f12>") 'equake-pop)


(provide 'configure-equake)
