(require 'go-mode)
(require 'go-guru)
;; (require 'go-complete)
;; (require 'go-eldoc)


(let* ((gopath (string-trim (shell-command-to-string "go env \"GOPATH\"")))
       (gobin (concat gopath "/bin")))
  (setenv "GOPATH" gopath)
  (setenv "PATH" (concat (getenv "PATH") ":" gobin))
  (add-to-list 'exec-path gobin))

(setq gofmt-command "goimports")

(add-hook 'before-save-hook 'gofmt-before-save)
;; (add-hook 'go-mode-hook 'go-guru-hl-identifier-mode)
;; (add-hook 'go-mode-hook 'go--enable-completion)
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; treat CamelCase as separate words
(add-hook 'go-mode-hook 'subword-mode)

;; MAYBE do something with it
;; (defun go--enable-completion ()
;;   (add-hook 'completion-at-point-functions 'go-complete-at-point))

;; (defvar go--doc-timer nil)
;; (defvar go--doc-delay 0.1)

;; (defun go--idle-doc-show ()
;;   (when go--doc-timer
;;     (cancel-timer go--doc-timer))
;;   (setq go--doc-timer
;;         (run-with-idle-timer go--doc-delay nil
;;                              (lambda () (call-interactively #'godef-describe)))))

;; (defun go--enable-idle-doc ()
;;   (add-hook 'post-command-hook #'go--idle-doc-show nil t))

;; (defun go--disable-idle-doc ()
;;   (remove-hook 'post-command-hook #'go--idle-doc-show t))

;; (add-hook 'go-mode-hook #'go--enable-idle-doc)

;;* smartparens setup. Mostly stolen from https://github.com/raxod502/radian/blob/develop/emacs/radian.el
(require 'smartparens)
(require 'smartparens-config)

(defun smartparens-go-setup ()
  (sp-use-paredit-bindings)
  (setq-local sp-highlight-pair-overlay nil)
  (setq-local sp-highlight-wrap-overlay nil)
  (setq-local sp-highlight-wrap-tag-overlay nil)
  (setq-local sp-cancel-autoskip-on-backward-movement nil)
  (smartparens-mode 1)
  (with-minor-mode-map-overriding (map smartparens-mode)
    (define-key map (kbd "M-r") 'sp-splice-sexp-killing-backward)
    (define-key map (kbd "M-[") 'sp-wrap-square)
    (define-key map (kbd "M-{") 'sp-wrap-curly)))

(add-hook 'go-mode-hook 'smartparens-go-setup)

;; TODO: move to init.el or some utils package
(cl-defmacro with-minor-mode-map-overriding ((new-map minor-mode) &body body)
  "Create a keymap locally overriding MINOR-MODE keymap and bind it to NEW-MAP inside BODY"
  (let ((old-map (gensym)))
    `(when-let ((,new-map (make-sparse-keymap))
                (,old-map (alist-get ',minor-mode minor-mode-map-alist)))
       (set-keymap-parent ,new-map ,old-map)
       (make-local-variable 'minor-mode-overriding-map-alist)
       (push (cons ',minor-mode ,new-map)
             minor-mode-overriding-map-alist)
       ,@body)))

;; Pressing <return> after inserting a pair creates an extra newline. Stolen from radian.el ^
(defun newline-and-indent-twice ()
  (newline-and-indent)
  (newline-and-indent)
  (forward-line -1))

(sp-local-pair #'go-mode "{" nil :post-handlers '((newline-and-indent-twice "<return>")))
(sp-local-pair #'go-mode "(" nil :post-handlers '((newline-and-indent-twice "<return>")))
(sp-local-pair #'go-mode "[" nil :post-handlers '((newline-and-indent-twice "<return>")))


;;* `DEFUNS'
;; (defun godef-jump-other-frame ()
;;   (interactive))

;;** Text Objects
(require 'evil)

(defun go-guru--find-enclosing (type)
  "Return (list BEG END) of the closest enclosing region of type TYPE."
  (when-let ((enclosing (go-guru--enclosing)))
    (cl-loop for ((_ . desc) (_ . start) (_ . end)) across enclosing
             when (string= type desc)
             return (cons start end))))

;; func
(defun go--func-bounds ()
  (when-let ((bounds (go-guru--find-enclosing "function declaration")))
    (cons (1+ (car bounds))
          (1+ (cdr bounds)))))
(put 'go-func 'bounds-of-thing-at-point #'go--func-bounds)

(defun go--func-beginning ()
  (interactive)
  (when-let ((bounds (go--func-bounds)))
    (goto-char (car bounds))))
(put 'go-func 'beginning-op #'go--func-beginning)

(defun go--func-end ()
  (interactive)
  (when-let ((bounds (go--func-bounds)))
    (goto-char (cdr bounds))))
(put 'go-func 'end-op #'go--func-end)

(evil-define-text-object go-inner-func (count &optional beg end type)
  (when-let ((range (evil-select-inner-object 'go-func beg end type)))
    (save-excursion
      (goto-char (car range))
      (when-let ((inner-beg (re-search-forward "{$" nil t)))
        (setf (car range) inner-beg)
        (decf (second range))))
    range))

(evil-define-text-object go-a-func (count &optional beg end type)
  (evil-select-inner-object 'go-func beg end type))


;; Expand-region based on `go-guru-expand-region' from go-mode.el
;; Accepts numeric arg and supports contraction of previously expanded regions.
(defvar go-guru--last-enclosing-idx 0
  "Current expansion's position in go-guru--last-enclosing vector.")

(defun go-guru-expand-region (n)
  "Expand region to the Nth enclosing syntactic unit.
If N is negative, undo previous N expansions."
  (interactive "p")
  (if (or (eq last-command #'go-guru-expand-region)
          (eq last-command #'go-guru-contract-region))
      (setq go-guru--last-enclosing-idx
            (max 0 (min (length go-guru--last-enclosing)
                        (+ go-guru--last-enclosing-idx n))))
    (let ((blocks (go-guru--enclosing-unique))
          (initial (list (cons 'desc "initial")
                         (cons 'start (point))
                         (cons 'end (if (region-active-p)
                                        (mark)
                                      (point))))))
      (setq go-guru--last-enclosing
            (vconcat (list initial) blocks))
      (setq go-guru--last-enclosing-idx 0)))
  (unless (> n (length go-guru--last-enclosing))
    (setq go-guru--last-enclosing-idx
          (max 0 (+ go-guru--last-enclosing-idx n)))
    (when-let* ((block (elt go-guru--last-enclosing go-guru--last-enclosing-idx))
                (beg (byte-to-position (cdr (assoc 'start block))))
                (end (byte-to-position (cdr (assoc 'end block))))
                (type (cdr (assoc 'desc block))))
      (unless (zerop go-guru--last-enclosing-idx)
        (incf beg)
        (incf end))
      (goto-char beg)
      (set-mark end)
      (message "Region: %s" type)
      (setq deactivate-mark (eq beg end)))))

(defun go-guru-contract-region (n)
  "Reverse expansions made by `go-guru-expand-region'."
  (interactive "p")
  (go-guru-expand-region (- n)))

;;** Compilation
(defun go-build (read-file-name)
  (interactive "P")
  (compile (format "go build %s" (if read-file-name
                                     (read-file-name "go build: ")
                                   (buffer-file-name)))))

(defun go-run (read-file-name)
  (interactive "P")
  (compile (format "go run %s" (if read-file-name
                                   (read-file-name "go build: ")
                                 (buffer-file-name)))
           t))

;;* `KEYS'
(define-key go-mode-map (kbd "M-.") 'godef-jump)
(define-key go-mode-map (kbd "C-x 4 .") 'godef-jump-other-window)
(define-key go-mode-map (kbd "C-c C-k") 'go-build)
(define-key go-mode-map (kbd "C-c C-b") 'go-run)
(define-key go-mode-map [remap er/expand-region] 'go-guru-expand-region)
(define-key go-mode-map [remap er/contract-region] 'go-guru-contract-region)


(evil-define-key 'visual go-mode-map
  "v" 'go-guru-expand-region)

(evil-define-key 'operator go-mode-map
  "x" 'go-guru-expand-region
  "s" (lambda () (interactive) (go-guru-expand-region 2))
  "if" 'go-inner-func
  "af" 'go-a-func)

(evil-define-key 'motion go-mode-map
  (kbd "<backspace>") 'go--func-beginning
  (kbd "<return>") 'go--func-end)


(provide 'configure-go)
