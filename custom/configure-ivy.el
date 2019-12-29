;;; -*- lexical-binding: t -*-
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
(setq counsel-find-file-at-point t)

;;* prompt count
(defun ivy-add-prompt-count* (next-fn prompt)
  "fix alignment of current match number"
  (if (string-match "%.*d" ivy-count-format)
      (concat ivy-count-format prompt)
    (funcall next-fn prompt)))

(advice-add 'ivy-add-prompt-count :around #'ivy-add-prompt-count*)

;;* counsel-files
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
  (lambda (&optional cands)
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
                :initial-input initial-input
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
                :initial-input initial-input
                :prompt (format "Find directory(%s): " default-directory)
                :caller 'counsel-dirs)))

(cl-defun counsel-fd (cmd &key prompt caller initial-input)
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
  (if (= 16 (prefix-numeric-value current-prefix-arg))
      (call-interactively #'ivy-resume)
    (setq isearch-forward t)            ; evil search direction
    (counsel-grep-or-swiper
     (cond (current-prefix-arg nil)
           ((region-active-p)
            (buffer-substring (point) (mark)))
           ((symbol-at-point)
            (symbol-name (symbol-at-point)))))))

(defun ivy-yank-symbol-at-point ()
  (interactive)
  (when-let ((symbol (with-ivy-window (symbol-at-point))))
    (insert (symbol-name symbol))))

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
                                recentf-list))
        (prompt-where (if where (format " other %s" (subseq (symbol-name where) 1)) "")))
    (ivy-read (format "Switch to buffer%s: " prompt-where)
              (append alive-buffers recent-buffers ivy-views)
              :history 'counsel-ibuffer-or-recentf-history
              :action (lambda (item)
                        (typecase item
                          (string (if (string-prefix-p "{}" item)
                                      (ivy-new-view item)
                                    (visit-buffer-or-file item where)))
                          (cons (visit-buffer-or-file (cdr item) where))))
              :caller 'counsel-ibuffer-or-recentf)))

(defun counsel-ibuffer-or-recentf-other-window (&optional name where)
  (interactive)
  (if (= 4 (prefix-numeric-value current-prefix-arg))
      (counsel-ibuffer-or-recentf name :frame)
    (counsel-ibuffer-or-recentf name :window)))

(defun counsel-ibuffer-or-recentf-other-frame (&optional name where)
  (interactive)
  (counsel-ibuffer-or-recentf name :frame))

(defun visit-buffer (buffer &optional where)
  (case where
    (:window (switch-to-buffer-other-window buffer))
    (:frame (switch-to-buffer-other-frame buffer))
    (t (switch-to-buffer buffer))))

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
            ((= point (line-end-position))
             (insert re-start))
            (t (forward-char)
               (insert re-start)
               (goto-char (+ point (length re-start))))))))

(defun ivy-minibuffer-insert-symbol-end ()
  (interactive)
  (let ((regex (rx symbol-end)))
    (if (looking-back (rx-to-string regex))
        (backward-delete-char (length regex))
      (insert regex))))

;;* ivy-fast-keys
;; TODO: make it into a package, allow custom keys and stuff
(defface ivy-fast-keys-face
  '((t :inherit font-lock-comment-face :background "black"))
  "Face for `ivy' fast keys hint.")

;; TODO: use ivy-state-display-transformer-fn instead
(defun ivy-format-function-fast-keys (cands)
  (let ((i 0))
    (ivy--format-function-generic
     (lambda (str)
       (concat "  " (ivy--add-face str 'ivy-current-match)))
     (lambda (str)
       (concat (propertize (number-to-string (incf i))
                           'face 'ivy-fast-keys-face)
               " "
               str))
     cands
     "\n")))

(add-to-list 'ivy-format-functions-alist '((t . ivy-format-function-fast-keys)))

(cl-defun ivy--make-fast-keys-action (n &optional (action #'ivy--done))
  (lambda ()
    (interactive)
    (when (and (numberp n) (plusp n) (<= n ivy-height))
      (let ((delta (- (if (>= ivy--window-index n) (1- n) n)
                      ivy--window-index)))
        (ivy-set-index (+ ivy--index delta))
        (ivy--exhibit)
        (ivy-done)
        (ivy-call)))))

(define-key ivy-minibuffer-map (kbd "M-1") (ivy--make-fast-keys-action 1))
(define-key ivy-minibuffer-map (kbd "M-2") (ivy--make-fast-keys-action 2))
(define-key ivy-minibuffer-map (kbd "M-3") (ivy--make-fast-keys-action 3))
(define-key ivy-minibuffer-map (kbd "M-4") (ivy--make-fast-keys-action 4))
(define-key ivy-minibuffer-map (kbd "M-5") (ivy--make-fast-keys-action 5))
(define-key ivy-minibuffer-map (kbd "M-6") (ivy--make-fast-keys-action 6))
(define-key ivy-minibuffer-map (kbd "M-7") (ivy--make-fast-keys-action 7))
(define-key ivy-minibuffer-map (kbd "M-8") (ivy--make-fast-keys-action 8))
(define-key ivy-minibuffer-map (kbd "M-9") (ivy--make-fast-keys-action 9))

;;* ivy-calling
(defvar ivy--initial-pos-marker nil "Position before ivy invocation.")

(defun ivy--enable-calling ()
  (setq ivy-calling t
        ivy--initial-pos-marker (with-ivy-window (point-marker))))

(defun ivy--restore-initial-pos ()
  ;; MAYBE: Add a flag similar to swiper-stay-on-quit.
  ;; Although in that case, simply adding (lambda () (setq ivy-calling t))
  ;; to ivy-hooks would yield the same behaviour.
  (unless (eq ivy-exit 'ivy-done)
    (goto-char ivy--initial-pos-marker))
  (setq ivy--initial-pos-marker nil))

(defun ivy-enable-calling-for-func (func)
  "Add appropriate hooks to make FUNC behave more like `swiper':
enable `ivy-calling' by default and restore original position on exit."
  (setf (alist-get func ivy-hooks-alist) #'ivy--enable-calling)
  (setf (alist-get func ivy-unwind-fns-alist) #'ivy--restore-initial-pos)
  nil)

(defun ivy-disable-calling-for-func (func)
  "Undo `ivy-enable-calling-for-func'."
  (setq ivy-hooks-alist (assq-delete-all func ivy-hooks-alist))
  (setq ivy-unwind-fns-alist (assq-delete-all func ivy-unwind-fns-alist)))

;; TODO: properly integrate imenu-anywhere
(ivy-enable-calling-for-func 'counsel-imenu)
(ivy-enable-calling-for-func 'ivy-xref-show-xrefs)
(ivy-enable-calling-for-func 'counsel-outline)

;; counsel-imenu-anywhere
(require 'imenu-anywhere)

(defun counsel-imenu-dwim (anywhere)
  (interactive "P")
  (call-interactively
   (if anywhere
       #'counsel-imenu-anywhere
       #'counsel-imenu)))

(defun counsel-imenu-anywhere ()
  (interactive)
  (let ((candidates (imenu-anywhere-candidates)))
    (ivy-read "imenu items: " candidates
              :preselect (thing-at-point 'symbol)
              :require-match t
              :action (lambda  (candidate)
                        (switch-to-buffer (marker-buffer (cdr candidate)))
                        (goto-char (cdr candidate)))
              :keymap counsel-imenu-map
              :caller 'counsel-imenu)))

;; other-window/frame
(ivy-enable-calling-for-func 'counsel-outline)
;;* ivy-done-other-window/frame
(defun ivy-done-other-window ()
  (interactive)
  (when-let* ((ivy-win (ivy--get-window ivy-last))
              (state (window-state-get ivy-win)))
    (let ((action (ivy--get-action ivy-last)))
      (ivy-exit-with-action (lambda (x)
                              (funcall action x)
                              (when-let ((new-win (or (split-window-sensibly ivy-win)
                                                      (split-window ivy-win nil nil))))
                                (select-window new-win)
                                (window-state-put state ivy-win)))))))

(defun ivy-done-other-frame ()
  (interactive)
  (let* ((ivy-win (ivy--get-window ivy-last))
         (state (window-state-get ivy-win)))
    (call-interactively #'ivy-call)
    (make-frame-command)
    (window-state-put state ivy-win)
    (exit-minibuffer)))

;;* ivy-xref-action
(defun ivy-xref-action ()
  (interactive)
  (ivy-exit-with-action
   (lambda (x)
     (let ((symbol (intern x)))
       (when (boundp symbol)
         (with-ivy-window
             (if (functionp symbol)
                 (find-function symbol)
               (find-variable symbol))))))))

;;* [DISABLED] ivy-enhanced eval-expression
(defun ivy--read-expression (prompt &optional initial-contents)
  "Like `read-expression' but use ivy to read from minibuffer."
  (let ((minibuffer-completing-symbol t)
        (inhibit-message t))
    (let ((s (ivy-read prompt read-expression-history
                            :initial-input initial-contents
                            :keymap ivy-read-expression-map
                            :caller #'ivy--read-expression)))
      (car (read-from-string s)))))

;; TODO: rewrite to always show possible completion
;; (advice-add 'read--expression :override #'ivy--read-expression)

(defun ivy--read-expression-hook ()
  (add-function :before-until (local 'eldoc-documentation-function)
                #'elisp-eldoc-documentation-function)
  (eldoc-mode 1)
  (add-hook 'completion-at-point-functions
            #'elisp-completion-at-point nil t)
  (run-hooks 'eval-expression-minibuffer-setup-hook)
  (with-minor-mode-map-overriding (map paredit-mode)
    (define-key map (kbd "C-j") nil)))

(setf (alist-get #'ivy--read-expression ivy-hooks-alist) #'ivy--read-expression-hook)

(defvar ivy-read-expression-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ivy-minibuffer-map)
    (define-key map (kbd "<tab>") 'completion-at-point)
    (define-key map (kbd "C-j") 'ivy-immediate-done)
    map))

;;* counsel-M-x+
;; Squeeze in a few more bindings into `counsel-M-x'.
;; TODO: remove <remap>s from `counsel-descbinds'
(defun counsel-M-x+ (&optional initial-input)
  "Like `counsel-M-x', but call `counsel-descbinds' with prefix arg."
  (interactive)
  (call-interactively
   (if current-prefix-arg
       #'counsel-descbinds
     #'counsel-M-x)))

(global-set-key (kbd "M-x") 'counsel-M-x+)

;;* avy-goto-char-timer-or-swiper
(setq avy-handler-function #'avy-handler-swiper)

(defun avy-handler-swiper (char)
  (case char
    ;; hardcoded char since avy uses `read-char' instead of keymaps
    (?\C-s
     (avy-resume-swiper)
     (throw 'done 'exit))
    (t (avy-handler-default))))

(defun avy-resume-swiper ()
  "Call `swiper' with last `avy-goto-char-timer' input."
  (interactive)
  (swiper (avy--goto-char-timer-text)))

(defun avy--goto-char-timer-text ()
  "An extremely hacky way to get current avy-goto-char-timer text
since it isn't stored anywhere apparently?"
  (when-let* ((buffer-cands (find-if (lambda (c)
                                    (eq (current-buffer) (window-buffer (cdr c))))
                                  avy--old-cands))
              (bounds (car buffer-cands)))
    (buffer-substring-no-properties (car bounds) (cdr bounds))))

;;* KEYS
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
(global-set-key (kbd "C-M-s") 'swiper-all)
(global-set-key (kbd "C-r") 'counsel-grep-or-swiper)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x f") 'counsel-files)
(global-set-key (kbd "C-x d") 'counsel-dirs)
(global-set-key (kbd "C-x /") 'counsel-rg)
(global-set-key (kbd "C-x C-/") 'counsel-rg-dir)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-c C-s") 'counsel-imenu-dwim)
(global-set-key (kbd "C-c s") 'counsel-imenu-dwim)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c C-v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

;;** C-h bindings
(setq counsel-describe-function-function #'helpful-function)
(setq counsel-describe-variable-function #'helpful-variable)

(define-key counsel-describe-map (kbd "M-.") #'counsel-find-symbol)
(define-key counsel-describe-map (kbd "M-,") #'counsel--info-lookup-symbol)

(global-set-key (kbd "C-h <C-i>") 'counsel-info-lookup-symbol)
(global-set-key (kbd "C-h b") 'counsel-descbinds)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h C-l") 'counsel-find-library)

;; (global-set-key (kbd "C-h f") 'counsel-describe-function)
;; (global-set-key (kbd "C-h v") 'counsel-describe-variable)

(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(define-key swiper-map (kbd "C-:") 'swiper-mc)
(define-key swiper-map (kbd "C-t") 'swiper-avy)
;; (define-key ivy-minibuffer-map (kbd "C-,") 'ivy-yank-symbol-at-point)
;; (define-key ivy-minibuffer-map (kbd "C-.") 'ivy-yank-word)
(define-key ivy-minibuffer-map (kbd "C-,") 'ivy-minibuffer-toggle-symbol-start)
(define-key ivy-minibuffer-map (kbd "C-.") 'ivy-minibuffer-insert-symbol-end)
(define-key counsel-describe-map (kbd "C-,") 'ivy-minibuffer-toggle-symbol-start)
(define-key counsel-describe-map (kbd "C-.") 'ivy-minibuffer-insert-symbol-end)
(define-key ivy-minibuffer-map (kbd "M-o") 'ivy-occur)
(define-key ivy-minibuffer-map (kbd "M-a") 'ivy-dispatching-done)
(define-key ivy-minibuffer-map (kbd "M-j") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "M-k") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "C-n") 'ivy-next-line-and-call)
(define-key ivy-minibuffer-map (kbd "C-p") 'ivy-previous-line-and-call)
(define-key ivy-minibuffer-map (kbd "C-t") 'ivy-avy)
(define-key ivy-minibuffer-map (kbd "C-x 4 RET") 'ivy-done-other-window)
(define-key ivy-minibuffer-map (kbd "C-x 5 RET") 'ivy-done-other-frame)
(define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-done-other-window)
;; (define-key ivy-minibuffer-map (kbd "M-.") 'ivy-xref-action)
(define-key ivy-minibuffer-map (kbd "M-.") 'counsel-find-symbol)
(define-key ivy-minibuffer-map (kbd "M-,") 'counsel--info-lookup-symbol)



(provide 'configure-ivy)
