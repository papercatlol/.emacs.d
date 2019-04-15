(require 'ivy)
(require 'ivy-hydra)
(require 'counsel)
(require 'ivy-prescient)

(ivy-mode t)
(ivy-prescient-mode t)
(setq ivy-use-virtual-buffers t
      ivy-count-format "[%d/%d] "
      ivy-re-builders-alist '((t . ivy--regex-ignore-order))
      ivy-magic-tilde nil
      ivy-initial-inputs-alist (delete (assoc 'counsel-M-x ivy-initial-inputs-alist)
                                       ivy-initial-inputs-alist))

(defun ivy-add-prompt-count* (next-fn prompt)
  "fix alignment of current match number"
  (if (string-match "%.*d" ivy-count-format)
      (concat ivy-count-format prompt)
    (funcall next-fn prompt)))

(advice-add 'ivy-add-prompt-count :around #'ivy-add-prompt-count*)

;; counsel-files
(defcustom counsel-files-base-cmd "fd"
  "command to list files in the project"
  :type 'string
  :group 'ivy)

(defcustom counsel-dirs-base-cmd "fd -t d"
  "command to list files in the project"
  :type 'string
  :group 'ivy)

(defun counsel-fd-occur (cmd)
  "Occur function for `counsel-fd' using `counsel-cmd-to-dired'."
  (lambda ()
    (cd (ivy-state-directory ivy-last))
    (counsel-cmd-to-dired
     (counsel--expand-ls
      (format "%s %s | xargs -d \"\\n\" ls"
              cmd
              (if (string= "" ivy--old-re)
                  ""
                (format "| grep -i -E '%s'" (counsel-unquote-regex-parens ivy--old-re)))))
     ;; set dired properties(e.g. for dired-hide-details-mode)
     (lambda (proc string)
       (let ((inhibit-read-only t)
             (point (point)))
         (insert string "\n")
         (dired-insert-set-properties point (point)))))))

(ivy-set-occur 'counsel-files (counsel-fd-occur counsel-files-base-cmd))
(ivy-set-occur 'counsel-dirs (counsel-fd-occur counsel-dirs-base-cmd))

(defun counsel-files (&optional initial-input initial-directory)
  "Recursively list all files within the `default-directory'.
With prefix arg list all tracked files.
With double prefix arg prompt for INITIAL-DIRECTORY."
  (interactive
   (list nil (when (= 16 (prefix-numeric-value current-prefix-arg))
               (read-directory-name "From directory: "))))
  (let ((default-directory (expand-file-name (if (= 4 (prefix-numeric-value current-prefix-arg))
                                                 (or initial-directory
                                                     ;; (vc-root-dir) emacs 25+?
                                                     (locate-dominating-file default-directory ".git")
                                                     default-directory)
                                               default-directory))))
    (counsel-fd counsel-files-base-cmd
                 :prompt (format "Find file(%s): " default-directory)
                 :caller 'counsel-files)))

(defun counsel-dirs (&optional initial-input initial-directory)
  "Same as `counsel-files' but for directories."
  (interactive
   (list nil (when (= 16 (prefix-numeric-value current-prefix-arg))
               (read-directory-name "From directory: "))))
  (let ((default-directory (expand-file-name (if (= 4 (prefix-numeric-value current-prefix-arg))
                                                 default-directory
                                               (or initial-directory
                                                   ;; (vc-root-dir) emacs 25+?
                                                   (locate-dominating-file default-directory ".git")
                                                   default-directory)))))
    (counsel-fd counsel-dirs-base-cmd
                :prompt (format "Find directory(%s): " default-directory)
                :caller 'counsel-dirs)))

(cl-defun counsel-fd (cmd &key prompt caller)
  (counsel-require-program (car (split-string cmd " ")))
  (ivy-read prompt
            (split-string
             (shell-command-to-string cmd)
             "\n" t)
            :matcher #'counsel--find-file-matcher
            :initial-input initial-input
            :action (lambda (x)
                      (with-ivy-window
                        (find-file (expand-file-name x ivy--directory))))
            :preselect (counsel--preselect-file)
            :require-match 'confirm-after-completion
            :history 'file-name-history
            :keymap counsel-find-file-map
            :caller caller))

(defun swiper-at-point ()
  (interactive)
  (let ((isearch-forward t)) ; evil search direction
    (counsel-grep-or-swiper
     (cond (current-prefix-arg nil)
           ((region-active-p)
            (buffer-substring (point) (mark)))
           ((symbol-at-point)
            (symbol-name (symbol-at-point)))))))

(defun ivy-yank-symbol-at-point ()
  (interactive)
  (let ((symbol))
    (with-ivy-window
      (setq symbol (symbol-at-point)))
    (when symbol
      (insert (symbol-name symbol)))))

(defun counsel-rg-dir (&optional initial-input initial-directory extra-rg-args rg-prompt)
  "Same as `counsel-rg' but search starting from current directory instead of the repo root."
  (interactive)
  (counsel-rg (or initial-input (and (symbol-at-point)
                                     (symbol-name (symbol-at-point))))
              (if current-prefix-arg
                  default-directory
                (read-directory-name "rg in directory: "))
              (or extra-rg-args "")
              rg-prompt))

(defun ivy-new-view (name)
  "Same as `ivy-push-view' but don't prompt for name."
  (let ((view (cl-labels
                  ((ft (tr)
                       (if (consp tr)
                           (if (eq (car tr) t)
                               (cons 'vert
                                     (mapcar #'ft (cddr tr)))
                             (cons 'horz
                                   (mapcar #'ft (cddr tr))))
                         (with-current-buffer (window-buffer tr)
                           (cond (buffer-file-name
                                  (list 'file buffer-file-name (point)))
                                 ((eq major-mode 'dired-mode)
                                  (list 'file default-directory (point)))
                                 (t
                                  (list 'buffer (buffer-name) (point))))))))
                (ft (car (window-tree))))))
    (push (list name view) ivy-views)))

(defun counsel-ibuffer-or-recentf (&optional name where)
  "Switch to buffer using `ibuffer' or find a file on `recetf' list.
NAME specifies the name of the `ibuffer' buffer (defaults to \"*Ibuffer*\").
Now also supports ivy-views."
  (interactive)
  (require 'recentf)
  (recentf-mode)
  (setq counsel-ibuffer--buffer-name (or name "*Ibuffer*"))
  (let ((alive-buffers (cdr (counsel-ibuffer--get-buffers))) ; remove current buffer
        (recent-buffers (mapcar (lambda (filename)
                                  (let ((filename (substring-no-properties filename)))
                                    (cons (format "Recentf: %s" filename)
                                          filename)))
                                recentf-list)))
    (lexical-let ((where where))
      (ivy-read "Switch to buffer: "
                (append alive-buffers recent-buffers ivy-views)
                :history 'counsel-ibuffer-or-recentf-history
                :action (lambda (item)
                          (typecase item
                            (string (if (string-prefix-p "{}" item)
                                        (ivy-new-view item)
                                      (visit-buffer-or-file item where)))
                            (cons (visit-buffer-or-file (cdr item) where))))
                :caller 'counsel-ibuffer-or-recentf))))

(defun counsel-ibuffer-or-recentf-other-window (&optional name where)
  (interactive)
  (counsel-ibuffer-or-recentf name :window))

(defun counsel-ibuffer-or-recentf-other-frame (&optional name where)
  (interactive)
  (counsel-ibuffer-or-recentf name :frame))

(defun visit-buffer (buffer &optional where)
  (case where
    (:window (switch-to-buffer-other-window item))
    (:frame (switch-to-buffer-other-frame item))
    (t (switch-to-buffer item))))

(defun visit-buffer-or-file (item &optional where)
  (typecase item
    (buffer (visit-buffer item where))
    (string (if (file-exists-p item)
                (case where
                  (:window (find-file-other-window item))
                  (:frame (find-file-other-frame item))
                  (t (find-file item)))
              (visit-buffer item where)))
    (cons (case where
            (:window (other-window 1))
            (:frame (select-frame-set-input-focus (make-frame)))
            (t (delete-other-windows)))
          (ivy-set-view-recur (car item)))))


(defvar symbol-start-regex (rx symbol-start))

;; MAYBE: make more generic and handle symbol-end the same way
(defun ivy-minibuffer-toggle-symbol-start ()
  "Insert or remove `symbol-start' regex. If there is a symbol at point,
prepend regex to it, otherwise insert at point. With prefix arg
always insert at point."
  (interactive)
  (let* ((point (point))
         (re-start (rx symbol-start))
         (re-end (rx symbol-end))
         (re-start-literal (rx-to-string re-start))
         (re-end-literal (rx-to-string re-end)))
    (if current-prefix-arg
        (insert re-start)
      (when (looking-back re-end-literal)
        (backward-char (length re-end)))
      (when-let ((symbol-bounds (bounds-of-thing-at-point 'symbol)))
        (goto-char (1- (car symbol-bounds))))
      (cond ((looking-at re-start-literal)
             (delete-char (length re-start))
             (goto-char (- point (length re-start))))
            (t (forward-char)
               (insert re-start)
               (goto-char (+ point (length re-start))))))))

(defun ivy-minibuffer-insert-symbol-end ()
  (interactive)
  (let ((regex (rx symbol-end)))
    (if (looking-back (rx-to-string regex))
        (backward-delete-char (length regex))
      (insert regex))))

;;* `KEYS'
(defhydra hydra-M-g (global-map "M-g")
  "M-g"
  ("n" next-error)
  ("M-n" next-error)
  ("p" previous-error)
  ("M-p" previous-error)
  ("g" avy-goto-line)
  ("M-g" avy-goto-line)
  ("c" goto-char))

(global-set-key (kbd "C-x b") 'counsel-ibuffer-or-recentf)
(global-set-key (kbd "C-x 4 b") 'counsel-ibuffer-or-recentf-other-window)
(global-set-key (kbd "C-x 5 b") 'counsel-ibuffer-or-recentf-other-frame)
(global-set-key (kbd "C-s") 'swiper-at-point)
(global-set-key (kbd "C-r") 'counsel-grep-or-swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x f") 'counsel-files)
(global-set-key (kbd "C-x d") 'counsel-dirs)
(global-set-key (kbd "C-x /") 'counsel-rg)
(global-set-key (kbd "C-x C-/") 'counsel-rg-dir)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-c C-s") 'counsel-imenu)
(global-set-key (kbd "C-c s") 'counsel-imenu)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c C-v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
(global-set-key (kbd "C-h C-i") 'counsel-info-lookup-symbol)

;; (global-set-key (kbd "C-h f") 'counsel-describe-function)
;; (global-set-key (kbd "C-h v") 'counsel-describe-variable)

(define-key swiper-map (kbd "C-:") 'swiper-mc)
(define-key swiper-map (kbd "C-t") 'swiper-avy)
;; (define-key ivy-minibuffer-map (kbd "C-,") 'ivy-yank-symbol-at-point)
;; (define-key ivy-minibuffer-map (kbd "C-.") 'ivy-yank-word)
(define-key ivy-minibuffer-map (kbd "C-,") 'ivy-minibuffer-toggle-symbol-start)
(define-key ivy-minibuffer-map (kbd "C-.") 'ivy-minibuffer-insert-symbol-end)
(define-key ivy-minibuffer-map (kbd "M-o") 'ivy-occur)
(define-key ivy-minibuffer-map (kbd "M-a") 'ivy-dispatching-done)


(provide 'configure-ivy)
