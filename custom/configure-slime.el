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

(defun slime--edit-definition-popup (&optional where)
  "Adapted from `slime-edit-definition-cont'. Use `popup.el' to select a candidate if multiple."
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
             (error "%s" (cadr (slime-xref.location (car xrefs) where))))
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
                                            (string< (second i1) (second i2))))))
                    (menu-items (mapcar (lambda (item)
                                          (popup-make-item (first item)
                                                           :value (fourth item)
                                                           :summary (format "%s:%s" (second item) (third item))))
                                        sorted-items))
                    (selected-location (popup-menu* menu-items)))
               (when selected-location
                 (slime-push-definition-stack)
                 (slime-pop-to-location selected-location where))))))))

(defun slime-edit-definition-popup (arg)
  "Like `slime-edit-definition' but use `popup.el' to select a candidate."
  (interactive "p")
  (if (minusp arg)
      (call-interactively #'slime-edit-definition)
    (slime--edit-definition-popup nil)))

(defun slime-edit-definition-other-window-popup (arg)
  "Like `slime-edit-definition-popup' but switch to other window."
  (interactive "p")
  (if (minusp arg)
      (call-interactively #'slime-edit-definition-other-window)
    (slime--edit-definition-popup 'window)))

(defun slime-edit-definition-other-frame-popup (arg)
  "Like `slime-edit-definition-popup' but switch to other frame."
  (interactive "p")
  (if (minusp arg)
      (call-interactively #'slime-edit-definition-other-frame)
    (slime--edit-definition-popup 'frame)))

(defun slime-kill-package-name ()
  (interactive)
  (let ((package (string-trim-left (slime-current-package) ":")))
    (message package)
    (kill-new package)))

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
  (define-key keymap [remap slime-edit-definition] 'slime-edit-definition-popup)
  (define-key keymap [remap slime-edit-definition-other-window] 'slime-edit-definition-other-window-popup)
  (define-key keymap [remap slime-edit-definition-other-frame] 'slime-edit-definition-other-frame-popup))

(dolist (keymap (list sldb-mode-map slime-inspector-mode-map slime-trace-dialog-mode-map slime-xref-mode-map))
  (define-key keymap (kbd "k") 'previous-line)
  (define-key keymap (kbd "j") 'next-line)
  (define-key keymap (kbd "C-f") 'ace-link))

(define-key slime-mode-map (kbd "C-c w") 'slime-kill-package-name)
(define-key slime-repl-mode-map (kbd "<f5>") 'slime-restart-inferior-lisp)
(define-key slime-repl-mode-map (kbd "(") 'self-insert-command)
(define-key sldb-mode-map (kbd "<tab>") 'sldb-toggle-details)
(define-key slime-inspector-mode-map (kbd "DEL") 'slime-inspector-pop)

;;** `presentations'
(define-key slime-presentation-map "r" 'slime-copy-presentation-at-point-to-repl)
(define-key slime-presentation-map "c" 'slime-copy-presentation-at-point-to-repl)
(define-key slime-presentation-map "w" 'slime-copy-presentation-at-point-to-kill-ring)
(define-key slime-presentation-map "d" 'slime-describe-presentation-at-point)
(define-key slime-presentation-map "P" 'slime-pretty-print-presentation-at-point)
(define-key slime-presentation-map "i" 'slime-inspect-presentation-at-point)
(define-key slime-presentation-map "." 'slime-edit-definition-popup)
(define-key slime-presentation-map "k" 'slime-previous-presentation)
(define-key slime-presentation-map "p" 'slime-previous-presentation)
(define-key slime-presentation-map "j" 'slime-next-presentation)
(define-key slime-presentation-map "n" 'slime-next-presentation)
(define-key slime-presentation-map "v" 'slime-save-presentation-at-point)
(define-key slime-presentation-map "m" 'slime-mark-presentation)

(define-key slime-presentation-command-map (kbd "C-v") 'slime-saved-presentation-dwim)


(provide 'configure-slime)
