(require 'slime)
(require 'slime-autoloads)
(require 'popup)

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
                       slime-uncompiled-fringe))
(slime-setup slime-contribs)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

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


(dolist (keymap (list slime-mode-map slime-repl-mode-map))
  (define-key keymap (kbd "C-c C-d C-d") 'slime-documentation-popup)
  (define-key keymap [remap slime-edit-definition] 'slime-edit-definition-popup)
  (define-key keymap [remap slime-edit-definition-other-window] 'slime-edit-definition-other-window-popup)
  (define-key keymap [remap slime-edit-definition-other-frame] 'slime-edit-definition-other-frame-popup))

(define-key slime-repl-mode-map (kbd "C-c <f5>") 'slime-restart-inferior-lisp)




(provide 'configure-slime)
