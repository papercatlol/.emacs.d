;;; -*- lexical-binding: t -*-
(require 'ivy)
(require 'ivy-hydra)
(require 'counsel)

;; ivy-prescient is bugged? TODO: look into it
;; (require 'ivy-prescient)
;; (ivy-prescient-mode t)

;;* general
(setq ivy-use-virtual-buffers t
      ivy-use-selectable-prompt t
      ivy-count-format "[%d/%d] "
      ivy-re-builders-alist '((t . ivy--regex-ignore-order))
      ivy-magic-tilde nil
      ivy-initial-inputs-alist (delete (assoc 'counsel-M-x ivy-initial-inputs-alist)
                                       ivy-initial-inputs-alist))
(setq counsel-find-file-at-point t)
(map-put ivy-height-alist 'counsel-find-file 20)

(ivy-mode 1)

;;* ivy-rich
(require 'ivy-rich)
(setq ivy-rich-path-style 'absolute)

;;** counsel-M-x
(plist-put ivy-rich-display-transformers-list
           'counsel-M-x
           '(:columns
             ((counsel-M-x-transformer (:width 80))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))))

;;** counsel-package
(defun ivy-rich-counsel-package-version (candidate)
  (ivy-rich-package-version (substring candidate 1)))

(defun ivy-rich-counsel-package-archive-summary (candidate)
  (ivy-rich-package-archive-summary (substring candidate 1)))

(defun ivy-rich-counsel-package-install-summary (candidate)
  (ivy-rich-package-install-summary (substring candidate 1)))

(plist-put ivy-rich-display-transformers-list
           'counsel-package
           '(:columns
             ((ivy-rich-candidate (:width 30))
              (ivy-rich-counsel-package-version (:width 16 :face font-lock-comment-face))
              (ivy-rich-counsel-package-archive-summary (:width 7 :face font-lock-builtin-face))
              (ivy-rich-counsel-package-install-summary (:face font-lock-doc-face)))))

;;** counsel-find-file
(defun ivy-rich-counsel-find-file-truename* (candidate)
  (if (file-remote-p default-directory)
      ""
    (let ((type (car (file-attributes (directory-file-name (expand-file-name candidate ivy--directory))))))
      (if (stringp type)
          (concat "-> " (expand-file-name type ivy--directory))
        ""))))

(plist-put ivy-rich-display-transformers-list
           'counsel-find-file
           '(:columns
             ((ivy-read-file-transformer)
              (ivy-rich-counsel-find-file-truename* (:face font-lock-doc-face)))))

;;** counsel-describe-function
(plist-put ivy-rich-display-transformers-list
           'counsel-describe-function
           '(:columns
             ((counsel-describe-function-transformer (:width 50))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))))

;;** counsel-describe-variable
(plist-put ivy-rich-display-transformers-list
           'counsel-describe-variable
           '(:columns
             ((counsel-describe-variable-transformer (:width 50))
              (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face)))))

;;** counsel-describe-symbol
(defun counsel-describe-symbol-transformer (symbol-name)
  (let ((symbol (intern-soft symbol-name)))
    (if (or (custom-variable-p symbol)
            (commandp symbol))
        (ivy-append-face symbol-name 'ivy-highlight-face)
      symbol-name)))

(defun ivy-rich-counsel-describe-symbol-docstring (candidate)
  (when-let ((symbol (intern-soft candidate)))
    (if (functionp symbol)
        (ivy-rich-counsel-function-docstring symbol)
      (ivy-rich-counsel-variable-docstring symbol))))

(plist-put ivy-rich-display-transformers-list
           'counsel-describe-symbol
           '(:columns
             ((counsel-describe-symbol-transformer (:width 50))
              (ivy-rich-counsel-describe-symbol-docstring (:face font-lock-doc-face)))))

;;** counsel-buffers
(cl-defun ivy-rich-counsel-buffers-dispatch (candidate &key buffer bookmark recentf ivy-view dired-recent)
  (cl-labels ((%call (fn arg)
                (if fn (funcall fn arg) "")))
    (ecase (counsel-buffers--buffer-type candidate)
      (:buffer (%call buffer candidate))
      (:recentf (%call recentf candidate))
      (:bookmark (%call bookmark candidate))
      (:ivy-view (%call ivy-view candidate))
      (:dired-recent (%call dired-recent candidate)))))

(defun ivy-rich-ivy-view-buffers (candidate)
  (let ((buffers))
    (cl-labels ((%flatten (tree)
                  (when (consp tree)
                    (case (car tree)
                      (buffer (push (second tree) buffers))
                      (file (push (file-name-nondirectory (second tree)) buffers))
                      (t (mapc #'%flatten (cdr tree)))))))
      (%flatten (cadr (assoc candidate ivy-views)))
      buffers)))

(defun ivy-rich-ivy-view-buffers-count (candidate)
  (int-to-string (length (ivy-rich-ivy-view-buffers candidate))))

(defun ivy-rich-ivy-view-buffers-list (candidate)
  (string-join (ivy-rich-ivy-view-buffers candidate) ", "))

(defun ivy-rich-counsel-buffers-recentf-filename (candidate)
  (file-name-nondirectory candidate))

(defun ivy-rich-counsel-buffers-dired-recent-dirname (candidate)
  (propertize (file-name-nondirectory (directory-file-name candidate))
              'face 'ivy-subdir))

(defun ivy-rich-counsel-buffers-buffer-path (candidate)
  (buffer-file-name (get-buffer candidate)))

(defun constantly (x)
  (lambda (&rest args)
    x))

;; -------------------------------------------------------- ;;
;; BUFFER(30) INDICATORS(4) MAJOR-MODE(10) PROJECT(15) PATH ;;
;; BOOKMARK   BOOK          TYPE                       PATH ;;
;; RECENTF    RECE          LAST-MODIFIED                   ;;
;; IVY-VIEW   VIEW          BUFFERS                         ;;
;; -------------------------------------------------------- ;;

(defun ivy-rich-counsel-buffers-0 (candidate)
  (ivy-rich-counsel-buffers-dispatch
   candidate
   :buffer #'ivy-switch-buffer-transformer
   :recentf #'ivy-rich-counsel-buffers-recentf-filename
   :bookmark #'identity
   :ivy-view #'identity
   :dired-recent #'ivy-rich-counsel-buffers-dired-recent-dirname))

(defun ivy-rich-counsel-buffers-1 (candidate)
  (ivy-rich-counsel-buffers-dispatch
   candidate
   :buffer #'ivy-rich-switch-buffer-indicators
   :bookmark (constantly "BOOK")
   :recentf (constantly "REC")
   :ivy-view (constantly "VIEW")
   :dired-recent (constantly "DIR")))

(defun ivy-rich-counsel-buffers-2 (candidate)
  (ivy-rich-counsel-buffers-dispatch
   candidate
   :buffer #'ivy-rich-switch-buffer-major-mode
   :bookmark #'ivy-rich-bookmark-type
   :recentf #'ivy-rich-file-last-modified-time
   :dired-recent #'ivy-rich-file-last-modified-time
   :ivy-view #'ivy-rich-ivy-view-buffers-count))

(defun ivy-rich-counsel-buffers-3 (candidate)
  (ivy-rich-counsel-buffers-dispatch
   candidate
   :buffer #'ivy-rich-counsel-buffers-buffer-path
   :bookmark #'ivy-rich-bookmark-filename
   :recentf #'identity
   :dired-recent #'identity
   :ivy-view #'ivy-rich-ivy-view-buffers-list))

(defface ivy-rich-counsel-buffers-project-face
    '((t :foreground "#8fbc8f" :bold t))
  "Face for project name in `counsel-buffers' list.")

(plist-put
 ivy-rich-display-transformers-list
 'counsel-buffers
 '(:columns
   ((ivy-rich-counsel-buffers-0 (:width 60))
    (ivy-rich-counsel-buffers-1 (:width 4 :face error :align right))
    (ivy-rich-counsel-buffers-2 (:width 20 :face warning))
    (ivy-rich-counsel-buffers-3 (:width (lambda (x)
                                          (ivy-rich-switch-buffer-shorten-path
                                           x (ivy-rich-minibuffer-width 0.5))))))))

(ivy-rich-mode +1)


;;* prompt count
(defun ivy-add-prompt-count* (next-fn prompt)
  "Fix alignment of current match number."
  (if (string-match "%.*d" ivy-count-format)
      (concat ivy-count-format prompt)
    (funcall next-fn prompt)))

(advice-add 'ivy-add-prompt-count :around #'ivy-add-prompt-count*)

;;* ivy completion height
(map-put ivy-height-alist 'ivy-completion-in-region 20)

;;* counsel-files
;; (defvar counsel-rg-delay (* 0.1 1000)
;;   "Delay in ms before rg is called.")

;; (defun counsel-rg-with-delay-advice (counsel-rg &rest args)
;;   (let ((ivy-dynamic-exhibit-delay-ms counsel-rg-delay))
;;     (apply counsel-rg args)))

;; (advice-add #'counsel-rg :around #'counsel-rg-with-delay-advice)

(defcustom counsel-files-base-cmd "fd"
  "Command to list files in the project."
  :type 'string
  :group 'ivy)

(defcustom counsel-dirs-base-cmd "fd -t d"
  "Command to list dirs in the project."
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
    (if (counsel--git-root)
        (counsel-git initial-input)
      (counsel-fd counsel-files-base-cmd
                  :initial-input initial-input
                  :prompt "Find file: "
                  :caller 'counsel-files))))

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
                :prompt "Find directory: "
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

;;* swiper-dwim
(map-put ivy-height-alist 'swiper 15)

(defun swiper-dwim ()
  "If region is active and no prefix arg is supplied, search for selected text,
otherwise call `counsel-grep-or-swiper'. With double prefix arg call `ivy-resume'."
  (interactive)
  (if (= 16 (prefix-numeric-value current-prefix-arg))
      (call-interactively #'ivy-resume)
    (setq isearch-forward t)            ; evil search direction
    (swiper
     (cond (current-prefix-arg nil)
           ((region-active-p)
            (buffer-substring-no-properties (point) (mark)))))))

(defun swiper-next-line-or-thing-at-point (&optional arg)
  "Move cursor vertically down ARG candidates.
If the input is empty, insert active region or symbol-at-point."
  (interactive "p")
  (if (string= ivy-text "")
      (insert
       (or (with-ivy-window
               (when-let ((bounds (if (region-active-p)
                                      (cons (region-beginning) (region-end))
                                    (bounds-of-thing-at-point 'symbol))))
                 (buffer-substring-no-properties (car bounds) (cdr bounds))))
           ""))
    (ivy-next-line arg)))

(define-key swiper-map (kbd "C-s") 'swiper-next-line-or-thing-at-point)
(define-key ivy-minibuffer-map (kbd "C-s") 'swiper-next-line-or-thing-at-point)
(global-set-key (kbd "C-s") 'swiper-dwim)
(global-set-key (kbd "C-M-r") 'ivy-resume)

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

;;* counsel-buffers
(defvar counsel-buffers-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m ivy-minibuffer-map)
    m)
  "Keymap for `counsel-buffers'.")

(require 'bookmark)
(require 'recentf)

(defvar counsel-buffers--prop :counsel-buffer-type)

(defun counsel-buffers--buffer-type (str)
  (get-text-property 0 counsel-buffers--prop str))

(defun counsel-buffers-all-candidates ()
  (labels ((%cand (str prop)
             ;; We use text properties to store metadata mostly for
             ;; the sake of `ivy-rich' which only gets a list of
             ;; strings as an input from ivy.
             (propertize str counsel-buffers--prop prop)))
    (let ((buffers
            (loop for b in (internal-complete-buffer "" nil t)
                  unless (get-buffer-window b) ; exclude visible windows
                    collect (%cand b :buffer)))
          (recent-files (loop for f in recentf-list
                              collect (%cand f :recentf)))
          (bookmarks (loop for b in (bookmark-all-names)
                           collect (%cand b :bookmark)))
          (views (loop for v in ivy-views
                       collect (%cand (car v) :ivy-view)))
          (dired-recent (loop for d in (or dired-recent-directories
                                           (progn (dired-recent-load-list)
                                                  dired-recent-directories))
                              collect (%cand d :dired-recent))))
      (append buffers
              bookmarks
              views
              ;; Remove open files from the list. Note that while
              ;; `get-file-buffer'doesn't detect renamed buffers, it
              ;; is A LOT faster than `find-buffer-visiting'.
              (cl-remove-if #'dired-find-buffer-nocreate dired-recent)
              (cl-remove-if #'get-file-buffer recent-files)))))

;; TODO: ivy-occur for this
;; MAYBE: show active frames as well as buffers
(defun counsel-buffers (&optional initial-input)
  "Switch to buffer, recently opened file, bookmark or ivy-view.
If ivy is exited and result has {} prefix, create a new ivy-view.
Use `counsel-buffer-cycle-action' while in ivy minibuffer to change where
buffer will be opened(current window, other window, other frame)."
  (interactive)
  (let* ((counsel-buffers-current-action (or counsel-buffers-current-action
                                             (if (= 4 (prefix-numeric-value current-prefix-arg))
                                                 #'counsel-buffers-action-other-frame
                                               #'counsel-buffers-action)))
         (prompt-suffix (counsel-buffer-prompt)))
    (ivy-read (format "Switch to buffer%s" prompt-suffix)
              (counsel-buffers-all-candidates)
              :initial-input initial-input
              :history 'counsel-buffers-history
              :action (lambda (cand) (funcall counsel-buffers-current-action cand))
              :caller 'counsel-buffers
              :keymap counsel-buffers-map)))

(defun counsel-buffers-other-window (&optional initial-input)
  "Same as `counsel-buffers', but open buffer in other window by default."
  (interactive)
  (let ((counsel-buffers-current-action
          (if (= 4 (prefix-numeric-value current-prefix-arg))
              #'counsel-buffers-action-other-frame
            #'counsel-buffers-action-other-window)))
    (counsel-buffers initial-input)))

(defun counsel-buffers-other-frame (&optional initial-input)
  "Same as `counsel-buffers', but open buffer in other frame by default."
  (interactive)
  (let ((counsel-buffers-current-action #'counsel-buffers-action-other-frame))
    (counsel-buffers initial-input)))

(defun visit-buffer (buffer &optional where)
  (case where
    (:window (switch-to-buffer-other-window buffer))
    (:frame (switch-to-buffer-other-frame buffer))
    (t (switch-to-buffer buffer))))

(defun visit-file (path &optional where)
  (if-let ((buf (get-file-buffer path)))
      (visit-buffer buf where)
    (when (file-exists-p path)
      (case where
        (:window (find-file-other-window path))
        (:frame (find-file-other-frame path))
        (t (find-file path))))))

(defun visit-bookmark (name &optional where)
  (when name
    (bookmark-jump
     name
     (case where
       (:window #'switch-to-buffer-other-window)
       (:frame #'switch-to-buffer-other-frame)))))

(defun visit-directory (dir &optional where)
  (when (stringp dir)
    (let ((d (list dir)))
      (case where
        (:window (dired-other-window d))
        (:frame (dired-other-frame d))
        (t (dired d))))))

(defun counsel-buffers-action (item &optional where)
  ;; (message "%s %s %s" item where (counsel-buffers--buffer-type item))
  (case (counsel-buffers--buffer-type item)
    (:buffer (visit-buffer item where))
    (:recentf (visit-file item where))
    (:dired-recent (visit-directory item where))
    (:bookmark (visit-bookmark item where))
    (:ivy-view
     (when (eq :frame where)
       (select-frame-set-input-focus (make-frame)))
     (ivy-set-view-recur (second (assoc item ivy-views))))
    (t (cond ((string-prefix-p "{}" item)
              (ivy-new-view item))
             ((file-exists-p item)
              (visit-file item where))
             (t (visit-buffer item where))))))

(defun counsel-buffers-action-other-window (item)
  (counsel-buffers-action item :window))

(defun counsel-buffers-action-other-frame (item)
  (counsel-buffers-action item :frame))

(defvar counsel-buffers-current-action nil)

(defvar counsel-buffers-actions
  '(counsel-buffers-action
    counsel-buffers-action-other-window
    counsel-buffers-action-other-frame))

(defvar counsel-buffer-prompts
  '((counsel-buffers-action . ": ")
    (counsel-buffers-action-other-window . "(other window): ")
    (counsel-buffers-action-other-frame . "(other frame): ")))

(defvar counsel-buffers-actions-ring
  (let ((ring (make-ring (length counsel-buffers-actions))))
    (dolist (action counsel-buffers-actions)
      (ring-insert-at-beginning ring action))
    ring))

(defun counsel-buffer-cycle-action ()
  (interactive)
  (when-let* ((next-action (ring-next counsel-buffers-actions-ring
                                      counsel-buffers-current-action))
              (current-prompt (alist-get counsel-buffers-current-action counsel-buffer-prompts ":? *$"))
              (next-prompt (counsel-buffer-prompt next-action)))
    (setq counsel-buffers-current-action next-action)
    (setq ivy--prompt
          (replace-regexp-in-string current-prompt next-prompt ivy--prompt))
    (ivy--insert-prompt)))

(cl-defun counsel-buffer-prompt (&optional (action counsel-buffers-current-action))
  (alist-get action counsel-buffer-prompts ": "))

(global-set-key (kbd "C-v") 'counsel-buffers)
(define-key ctl-x-4-map (kbd "v") 'counsel-buffers-other-window)
(define-key ctl-x-5-map (kbd "v") 'counsel-buffers-other-frame)
(global-set-key (kbd "C-x b") (lambda () (interactive) (message "Use C-v")))
(define-key counsel-buffers-map (kbd "C-v") 'counsel-buffer-cycle-action)

;;** switch to scratch
(defun counsel-buffers-scratch ()
  (interactive)
  (ivy--done (buffer-name (startup--get-buffer-create-scratch))))

(define-key counsel-buffers-map (kbd "C-s") 'counsel-buffers-scratch)

;;* ivy-special
(cl-defmacro ivy-special (special-form &rest else)
  "Eval SPECIAL-FORM and exit minibuffer if it is empty,
otherwise eval ELSE and stay."
  `(if (string= "" ivy-text)
       (ivy-exit-with-action
        (lambda (it)
          ,special-form))
     ,@else))

(cl-defmacro def-ivy-special (map key special-func default-func)
  (let ((func-name (intern (format "%s-%s" (symbol-name map) key))))
    (cl-labels ((%call-or-eval (thing)
                  (if (symbolp thing)
                      `(call-interactively #',thing)
                    thing)))
      `(progn
         (defun ,func-name ()
           (interactive)
           (ivy-special ,(%call-or-eval special-func) ,(%call-or-eval default-func)))

         (define-key ,map (kbd ,key) ',func-name)))))

(def-ivy-special counsel-buffers-map "C-w" windmove-up backward-kill-word)
(def-ivy-special counsel-buffers-map "C-a" windmove-left move-beginning-of-line)
(def-ivy-special counsel-buffers-map "C-d" windmove-right delete-char)
(def-ivy-special counsel-buffers-map "<C-i>" (funcall counsel-buffers-current-action user-init-file) hippie-expand)
;;* toggle-symbol-start/end
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

;;* counsel-imenu-anywhere
(require 'imenu-anywhere)

(map-put ivy-height-alist 'counsel-imenu 15)

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
;;* ivy-done-other-window/frame/ace-window
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

(defun ivy-done-ace-window ()
  "Select a window with `ace-window', then call ivy-action."
  (interactive)
  (let ((action (ivy--get-action ivy-last)))
    (ivy-exit-with-action
     (lambda (x)
       (let ((aw-dispatch-always t))
         (ace-window t))
       (funcall action x)))))

(define-key ivy-minibuffer-map (kbd "C-c C-c") 'ivy-done-ace-window)
(define-key ivy-minibuffer-map (kbd "C-x 4 RET") 'ivy-done-other-window)
(define-key ivy-minibuffer-map (kbd "C-x 5 RET") 'ivy-done-other-frame)
(define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-done-other-window)

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

;;* [DISABLED] avian
;; (require 'avian)

;; (avian-swiper-mode)

;; (define-key swiper-map (kbd "SPC") avian:swiper-maybe-done)

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

;;* counsel-descbinds
;; TODO: remove <remap>s from `counsel-descbinds'

;;* avy-goto-char-timer-or-swiper
(setq avy-handler-function #'avy-handler-swiper)

(defun avy-handler-swiper (char)
  (case char
    ;; hardcoded char since avy uses `read-char' instead of keymaps
    (?\C-s
     (avy-resume-swiper)
     (throw 'done 'exit))
    (t (avy-handler-default char))))

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

;;* ivy-magical-space
(defun ivy-magical-space ()
  "If there is a single ivy candidate and point is at the end of the minibuffer,
exit with that candidate, otherwise insert SPACE character as usual."
  (interactive)
  (call-interactively
   (if (and (= 1 (length ivy--old-cands))
            (= (point) (line-end-position)))
       #'ivy-done
     #'self-insert-command)))

;; (define-key ivy-minibuffer-map (kbd "SPC") 'ivy-magical-space)
;; (define-key swiper-map (kbd "SPC") 'ivy-magical-space)

;;* ivy-done and ivy-call with prefix args
(defun ivy-call-or-dispatching-call (dispatch)
  "Call `ivy-dispatching-call' with prefix arg, `ivy-call' otherwise."
  (interactive "P")
  (call-interactively (if dispatch #'ivy-dispatching-call #'ivy-call)))

(defun ivy-done-or-dispatching-done (dispatch)
  "Call `ivy-dispatching-done' with prefix arg, `ivy-done' otherwise."
  (interactive "P")
  (call-interactively (if dispatch #'ivy-dispatching-done #'ivy-done)))

(define-key ivy-minibuffer-map (kbd "M-m") 'ivy-call-or-dispatching-call)
(define-key ivy-minibuffer-map (kbd "RET") 'ivy-done-or-dispatching-done)

;;* dired-recent
(require 'dired-recent)

(dired-recent-mode 1)

(defvar dired-recent-save-list-timer
  (run-with-idle-timer 120 t #'dired-recent-save-list))

;; disable C-x C-d overriding, since we will use counsel-buffers instead
(define-key dired-recent-mode-map (kbd "C-x C-d") nil)

;;* counsel-shell-history
(with-eval-after-load 'comint
  (define-key comint-mode-map
    [remap comint-history-isearch-backward-regexp]
    'counsel-shell-history)
  (define-key comint-mode-map (kbd "C-r") 'counsel-shell-history))

;;* swiper-evil-replace
(with-eval-after-load 'evil
  (defun swiper-evil-replace ()
    (interactive)
    (ivy-exit-with-action
     (lambda (_)
       (let ((re (string-join (ivy--split ivy-text) ".*?")))
         (evil-ex (format "%%s/%s/" re))))))
  (define-key swiper-map (kbd "M-q") 'swiper-evil-replace))

;;* swiper-narrow
(defun swiper-narrow ()
  (interactive)
  (ivy-exit-with-action
   (lambda (_)
     (save-restriction
      (call-interactively #'narrow-dwim)
      (swiper ivy-text)))))

(define-key swiper-map (kbd "C-x C-n") 'swiper-narrow)

;;* KEYS
(defhydra hydra-M-g (global-map "M-g")
  "M-g"
  ("n" next-error)
  ("M-n" next-error)
  ("j" next-error)
  ("p" previous-error)
  ("M-p" previous-error)
  ("k" previous-error)
  ("g" avy-goto-line)
  ("M-g" avy-goto-line)
  ("c" goto-char))

(global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x b") 'counsel-buffers)
(global-set-key (kbd "C-x 4 b") 'counsel-buffers-other-window)
(global-set-key (kbd "C-x 5 b") 'counsel-buffers-other-frame)
(global-set-key (kbd "C-M-s") 'swiper-all)
(global-set-key (kbd "C-r") 'counsel-grep-or-swiper)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x f") 'counsel-files)
(global-set-key (kbd "C-x d") 'counsel-dirs)
(global-set-key (kbd "M-y") 'counsel-yank-pop)

(global-set-key (kbd "C-c C-s") 'counsel-imenu-dwim)
(global-set-key (kbd "C-c s") 'counsel-imenu-dwim)
(global-set-key (kbd "C-c C-s") 'counsel-imenu-dwim)
(with-eval-after-load 'sh-script
 (define-key sh-mode-map (kbd "C-c C-s") 'counsel-imenu-dwim))

(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c C-v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
(global-set-key [remap insert-char] 'counsel-unicode-char)

;;** C-h bindings
(setq counsel-describe-function-function #'helpful-function)
(setq counsel-describe-variable-function #'helpful-variable)
(setq counsel-describe-symbol-function #'helpful-symbol)

(define-key help-map "o" 'counsel-describe-symbol)


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
(define-key swiper-map (kbd "C-M-y") 'ivy-yank-symbol)
(define-key ivy-minibuffer-map (kbd "C-,") 'ivy-minibuffer-toggle-symbol-start)
(define-key ivy-minibuffer-map (kbd "C-.") 'ivy-minibuffer-insert-symbol-end)
(define-key counsel-describe-map (kbd "C-,") 'ivy-minibuffer-toggle-symbol-start)
(define-key counsel-describe-map (kbd "C-.") 'ivy-minibuffer-insert-symbol-end)
(define-key ivy-minibuffer-map (kbd "M-o") 'ivy-occur)

;;** ivy actions and ivy-call
(define-key ivy-minibuffer-map (kbd "M-a") 'ivy-dispatching-done)
(define-key ivy-minibuffer-map (kbd "C-M-a") 'ivy-dispatching-call)
(define-key ivy-minibuffer-map (kbd "C-M-S-a") 'ivy-read-action)

(define-key ivy-minibuffer-map (kbd "M-j") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "M-k") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "C-n") 'ivy-next-line-and-call)
(define-key ivy-minibuffer-map (kbd "C-p") 'ivy-previous-line-and-call)
(define-key ivy-minibuffer-map (kbd "C-M-j") 'ivy-next-line-and-call)
(define-key ivy-minibuffer-map (kbd "C-M-k") 'ivy-previous-line-and-call)
(define-key ivy-minibuffer-map (kbd "C-t") 'ivy-avy)
(define-key ivy-minibuffer-map (kbd "S-SPC") 'ivy-avy)
(define-key swiper-map (kbd "S-SPC") 'swiper-avy)
;; (define-key ivy-minibuffer-map (kbd "M-.") 'ivy-xref-action)
(define-key ivy-minibuffer-map (kbd "M-.") 'counsel-find-symbol)
(define-key ivy-minibuffer-map (kbd "M-,") 'counsel--info-lookup-symbol)
(define-key ivy-minibuffer-map (kbd "C-M-m") 'ivy-immediate-done)

(define-key ivy-minibuffer-map (kbd "C-h") 'ivy-backward-delete-char)
(define-key ivy-minibuffer-map (kbd "C-w") 'ivy-backward-kill-word)

;;** ivy-minibuffer-grow/shrink
;; MAYBE increase grow/shrink step to 4?
(with-eval-after-load 'ivy-hydra
  (define-key ivy-minibuffer-map (kbd "C-=") 'ivy-minibuffer-grow)
  (define-key ivy-minibuffer-map (kbd "C--") 'ivy-minibuffer-shrink))



(provide 'configure-ivy)
