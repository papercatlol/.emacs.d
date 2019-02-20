(require 'slime)
(require 'slime-autoloads)
(require 'popup)
(require 'ace-link)

(setq inferior-lisp-program (getenv "LISP_BINARY"))
(setq slime-contribs '(slime-repl
                       slime-autodoc
                       slime-editing-commands
                       slime-fancy-inspector
                       slime-fancy-trace
                       ;; slime-fuzzy
                       ;; slime-mdot-fu
                       ;; slime-macrostep
                       slime-presentations
                       ;; slime-scratch
                       slime-references
                       ;; slime-fontifying-fu
                       slime-trace-dialog
                       slime-cl-indent
                       slime-uncompiled-fringe
                       slime-tramp))
(slime-setup slime-contribs)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook       #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; TODO: fix regex
;; upper -> lower pathname tail
;; (setq slime-filename-translations
;;       (list (list
;;              ".*"
;;              #'downcase
;;              (lambda (pathname)
;;                (let ((case-fold-search nil))
;;                  (message (format "path::\"%s\"" pathname))
;;                  (if (string-match (rx line-start
;;                                        (group (minimal-match (1+ any)))
;;                                        (group (1+ "/" (1+ (or upper "."))))
;;                                        line-end)
;;                                    pathname)
;;                      (concat (match-string 1 pathname) (downcase (match-string 2 pathname)))
;;                    pathname))))))

;; indentation  (c) igord
(defun lisp-add-keywords (face-name keyword-rules)
   (let* ((keyword-list (mapcar #'(lambda (x)
                                    (symbol-name (cdr x)))
                                keyword-rules))
          (keyword-regexp (concat "(\\("
                                  (regexp-opt keyword-list)
                                  "\\)[ \n]")))
     (font-lock-add-keywords 'lisp-mode
                             `((,keyword-regexp 1 ',face-name))))
   (mapc #'(lambda (x)
             (put (cdr x)
                  ;;'scheme-indent-function
                  'common-lisp-indent-function
                  (car x)))
         keyword-rules))
 
(lisp-add-keywords
 'font-lock-keyword-face
 '((1 . mv-let*)
   (1 . letvar)
   (1 . letvar*)
   (nil . deftrf)
   (2 . !~)
   (2 . !.)
   (2 . foreach)
   (2 . foreach)
   (2 . forsome)
   (2 . forthis)
   (2 . forthis!)
   (2 . /.)
   (2 . foreach-child)
   
   (0 . aif)
   (1 . awhen)

   (2 . defclass*)
   ))

(defun slime-documentation-popup ()
  "Display `swank:documentation-symbol' using `popup.el'."
  (interactive)
  (let ((symbol-name (slime-read-symbol-name "Describe symbol: ")))
    (popup-tip (slime-eval `(swank:documentation-symbol ,symbol-name))
               :truncate nil
               :height 60)))

(defun slime--edit-definition-ivy (&optional where)
  "Adapted from `slime-edit-definition-cont'. Use `ivy' to select a candidate if multiple."
  (let* ((symbol-name (slime-read-symbol-name "Edit definition of: "))
         (xrefs (slime-find-definitions symbol-name)))
    (cl-destructuring-bind (same-loc file-alist) (slime-analyze-xrefs xrefs)
      (cond ((null xrefs)
             (error "No known definition for: %s (in %s)" symbol-name (slime-current-package)))
            (same-loc
             (slime-push-definition-stack)
             (slime-pop-to-location (slime-xref.location (car xrefs)) where))
            ;; ((:error "..."))
            ((slime-length= xrefs 1)
             (error "%s" (second (slime-xref.location (car xrefs)))))
            (t
             (let* ((items (mapcar (lambda (xref)
                                     (let* ((spec (downcase
                                                   (replace-regexp-in-string "[\n ]+" " " (slime-xref.dspec xref))))
                                            (location (slime-xref.location xref))
                                            (file (second (assoc :file (cdr location))))
                                            (line (line-number-at-pos (second (assoc :position (cdr location))))))
                                       (and spec file line location (list spec file line location))))
                                   xrefs))
                    (sorted-items (sort (remove nil items)
                                        (lambda (i1 i2)
                                          (if (string= (second i1) (second i2))
                                              (< (third i1) (third i2))
                                            (string< (second i1) (second i2)))))))
               (ivy-read "Edit definition of: "
                         sorted-items
                         :action (lambda (item)
                                   (slime-push-definition-stack)
                                   (slime-pop-to-location (fourth item) where)))))))))

(defun slime-edit-definition-ivy (arg)
  "Like `slime-edit-definition' but use `ivy' to select a candidate."
  (interactive "p")
  (if (minusp arg)
      (call-interactively #'slime-edit-definition)
    (slime--edit-definition-ivy nil)))

(defun slime-edit-definition-other-window-ivy (arg)
  "Like `slime-edit-definition-ivy' but switch to other window."
  (interactive "p")
  (if (minusp arg)
      (call-interactively #'slime-edit-definition-other-window)
    (slime--edit-definition-ivy 'window)))

(defun slime-edit-definition-other-frame-ivy (arg)
  "Like `slime-edit-definition-ivy' but switch to other frame."
  (interactive "p")
  (if (minusp arg)
      (call-interactively #'slime-edit-definition-other-frame)
    (slime--edit-definition-ivy 'frame)))

(defun slime-kill-package-name ()
  (interactive)
  (let ((package (slime-pretty-package-name (slime-current-package))))
    (message package)
    (kill-new package)))

;;** `slime-find-all-symbols'
(defun slime-find-all-symbols (&optional internal)
  "Return symbol-names of symbols from all registered packages."
  (let* ((scope (if internal '(:external :internal) '(:external)))
         (symbols (slime-eval
                   `(cl:let ((packages (cl:remove (cl:find-package :keyword) (cl:list-all-packages)))
                             (symbols))
                            (cl:with-package-iterator (next packages ,@scope)
                                                      (cl:loop (cl:multiple-value-bind (morep symbol) (next)
                                                                                       (cl:push symbol symbols)
                                                                                       (cl:unless morep (cl:return)))))
                            (cl:mapcar (cl:lambda (symbol) (cl:string-downcase (cl:format nil "~s" symbol)))
                                       (cl:remove-duplicates symbols)))
                   "CL-USER")))
    symbols))

;; TODO: xref and documentation lookups in completion buffer
(defun slime-complete-symbol-global (internal)
  "Complete a symbol searching symbols from all visible packages.
If INTERNAL is T, also search internal symbols."
  (interactive "P")
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (start (and bounds (car bounds)))
         (end (and bounds (cdr bounds)))
         (initial-input (and bounds (buffer-substring-no-properties start end)))
         (symbol (completing-read "Find symbol: " (slime-find-all-symbols internal) nil nil initial-input)))
    (when bounds
      (delete-region start end))
    (insert symbol)))

;;** `slime-saved-presentations'
(defvar slime-saved-presentations nil
  "Saved slime-presentations. List of pairs (name . presentation).
Doesn't work properly with multiple lisp connections.")
(defvar slime-saved-presentations-history nil)

(defun slime-saved-presentations-reset ()
  (setq slime-saved-presentations nil))
(add-hook 'slime-inferior-process-start-hook #'slime-saved-presentations-reset)

(defun slime-save-presentation-at-point (point)
  "Save a presentation at point under prompted name. If presentation with the same name
already exists, override it."
  (interactive "d")
  (multiple-value-bind (presentation start end)
      (slime-presentation-around-or-before-point-or-error point)
    (let* ((str (buffer-substring start end))
           (name (ivy-read "Save presentation as: " (mapcar #'car slime-saved-presentations)
                           :initial-input (substring-no-properties str)
                           :require-match nil
                           :caller 'slime-save-presentation-at-point
                           :history 'slime-saved-presentations-history)))
      (slime-save-presentation name str presentation))))

(defun slime-save-presentation (name str presentation)
  (let ((id (slime-presentation-id presentation))
        (member (cl-member name slime-saved-presentations :key #'car :test #'string=)))
    (unless (numberp id)
      (setq id (slime-eval `(swank:lookup-and-save-presented-object-or-lose ',id))))
    (let ((item (cons name (list str id))))
      (if member
          (setf (car member) item)
        (push item slime-saved-presentations)))))

(defun slime-insert-saved-presentation-action (item)
  ;; TODO: update presentation str?
  (destructuring-bind (str id) (cdr item)
    (slime-insert-presentation str id)))

(defun slime-insert-saved-presentation ()
  "Insert presentation saved under prompted name."
  (interactive)
  (ivy-read "Insert presentation: " slime-saved-presentations
            :history 'slime-saved-presentations-history
            :caller 'slime-insert-saved-presentation
            :action #'slime-insert-saved-presentation-action))

(defun slime-saved-presentation-remove-action (presentation)
  (setq slime-saved-presentations
        (remove presentation slime-saved-presentations)))

(ivy-add-actions 'slime-save-presentation-at-point '(("d" slime-saved-presentation-remove-action "remove")))
(ivy-add-actions 'slime-insert-saved-presentation '(("d" slime-saved-presentation-remove-action "remove")))

(defun slime-saved-presentation-dwim (point)
  "Save presentation if there is one around or before point,
otherwise insert a saved presentation."
  (interactive "d")
  (call-interactively
   (if (slime-presentation-around-or-before-point-p)
       #'slime-save-presentation-at-point
     #'slime-insert-saved-presentation)))

(defun slime-repl-set-default-package ()
  (interactive)
  (slime-repl-set-package "cl-user"))

(defun sldb-sync-frame-package ()
  "Set Lisp package to that of the current frame."
  (interactive)
  (let* ((frame (sldb-frame-number-at-point))
         (pkg (slime-eval `(swank:frame-package-name ,frame))))
    (slime-repl-set-package pkg)))

(defun slime-repl-bury-buffer ()
  (interactive)
  (bury-buffer)
  (other-window 1))

;;** `avy-actions'
(defun avy-action-copy-to-repl (pt)
  (when (number-or-marker-p pt)
    (case major-mode
      (slime-inspector-mode
       (goto-char pt)
       (call-interactively #'slime-inspector-copy-down-to-repl))
      ((or slime-mode slime-popup-buffer-mode slime-trace-dialog-mode sldb-mode slime-repl-mode)
       (slime-copy-presentation-at-point-to-repl pt)))))

(defun avy-action-inspect (pt)
  (when (number-or-marker-p pt)
    (case major-mode
      (slime-inspector-mode
       (goto-char pt)
       (call-interactively #'slime-inspector-operate-on-point))
      ((or slime-mode slime-popup-buffer-mode slime-trace-dialog-mode sldb-mode slime-repl-mode)
       (slime-inspect-presentation-at-point pt)))))

(add-to-list 'avy-dispatch-alist (cons ?C #'avy-action-copy-to-repl))

(add-to-list 'avy-dispatch-alist (cons ?I #'avy-action-inspect))

;;* `KEYS'
(dolist (keymap (list slime-mode-map slime-repl-mode-map))
  (define-key keymap (kbd "C-c C-d C-d") 'slime-documentation-popup)
  (define-key keymap [remap slime-edit-definition] 'slime-edit-definition-ivy)
  (define-key keymap [remap slime-edit-definition-other-window] 'slime-edit-definition-other-window-ivy)
  (define-key keymap [remap slime-edit-definition-other-frame] 'slime-edit-definition-other-frame-ivy))

(dolist (keymap (list sldb-mode-map slime-inspector-mode-map slime-trace-dialog-mode-map slime-xref-mode-map))
  (define-key keymap (kbd "k") 'previous-line)
  (define-key keymap (kbd "j") 'next-line)
  (define-key keymap (kbd "C-f") 'ace-link))

(define-key slime-repl-mode-map (kbd "<f5>") 'slime-restart-inferior-lisp)
(define-key slime-repl-mode-map (kbd "(") 'self-insert-command)
(define-key slime-repl-mode-map (kbd "C-c C-z") 'slime-repl-bury-buffer)
(define-key slime-mode-map (kbd "C-x C-e") 'slime-eval-last-expression-in-repl)
(define-key sldb-mode-map (kbd "<tab>") 'sldb-toggle-details)
(define-key slime-inspector-mode-map (kbd "DEL") 'slime-inspector-pop)
(define-key slime-mode-map (kbd "C-c p") 'slime-pprint-eval-last-expression)
(define-key slime-mode-map (kbd "C-c C-i") 'slime-complete-symbol-global)
(define-key slime-repl-mode-map (kbd "C-c C-i") 'slime-complete-symbol-global)

;; package-related utils
(define-key slime-mode-map (kbd "C-c w") 'slime-kill-package-name)
(define-key slime-mode-map (kbd "C-c C-p") 'slime-sync-package-and-default-directory)
(define-key sldb-mode-map (kbd "C-c C-p") 'sldb-sync-frame-package)
(define-key slime-repl-mode-map (kbd "C-c C-p") 'slime-repl-set-default-package)


;;** `presentations'
(define-key slime-presentation-map "r" 'slime-copy-presentation-at-point-to-repl)
(define-key slime-presentation-map "c" 'slime-copy-presentation-at-point-to-repl)
(define-key slime-presentation-map "w" 'slime-copy-presentation-at-point-to-kill-ring)
(define-key slime-presentation-map "d" 'slime-describe-presentation-at-point)
(define-key slime-presentation-map "P" 'slime-pretty-print-presentation-at-point)
(define-key slime-presentation-map "i" 'slime-inspect-presentation-at-point)
(define-key slime-presentation-map "." 'slime-edit-definition-ivy)
;; (define-key slime-presentation-map "k" 'slime-previous-presentation)
(define-key slime-presentation-map "p" 'slime-previous-presentation)
;; (define-key slime-presentation-map "j" 'slime-next-presentation)
(define-key slime-presentation-map "n" 'slime-next-presentation)
(define-key slime-presentation-map "v" 'slime-save-presentation-at-point)
(define-key slime-presentation-map "m" 'slime-mark-presentation)

(define-key slime-presentation-command-map (kbd "C-v") 'slime-saved-presentation-dwim)


(provide 'configure-slime)
