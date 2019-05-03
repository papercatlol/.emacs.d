(require 'slime)
(require 'slime-autoloads)
(require 'popup)
(require 'ace-link)

(require 'eros)
(add-hook 'emacs-lisp-mode-hook #'eros-mode)

(setq inferior-lisp-program (getenv "LISP_BINARY"))
(setq slime-contribs '(slime-repl
                       slime-autodoc
                       slime-asdf
                       slime-editing-commands
                       slime-fancy-inspector
                       slime-fancy-trace
                       ;; slime-fuzzy
                       ;; slime-mdot-fu
                       ;; slime-macrostep
                       slime-presentations
                       slime-package-fu
                       ;; slime-scratch
                       slime-references
                       ;; slime-fontifying-fu
                       slime-trace-dialog
                       slime-cl-indent
                       slime-uncompiled-fringe
                       slime-tramp
                       ;; slime-xref-browser
                       ))

(slime-setup slime-contribs)

;; slime-package-fu
(setq slime-defpackage-regexp
             (rx line-start "("
                 (or (and (? (or "cl:" "common-lisp:")) "defpackage")
                     (and (? (or "uiop:" "uiop/package:" "package:")) "define-package"))
                 symbol-end
                 (* space)))

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

(defun slime-read-symbol-name-global (prompt &optional query)
  (cond ((or current-prefix-arg query (not (slime-symbol-at-point)))
         (let ((internal (= 16 (prefix-numeric-value current-prefix-arg))))
           (completing-read prompt
                            (slime-find-all-symbols internal)
                            nil nil
                            (slime-symbol-at-point))))
        (t (slime-symbol-at-point))))

(defalias 'slime-read-symbol-name 'slime-read-symbol-name-global) 

(defun slime--bounds-of-region-or-symbol ()
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (bounds-of-thing-at-point 'symbol)))

;; TODO: xref and documentation lookups in completion buffer
(defun slime-complete-symbol-global (internal)
  "Complete a symbol searching symbols from all visible packages.
If INTERNAL is T, also search internal symbols."
  (interactive "P")
  (let* ((bounds (slime--bounds-of-region-or-symbol))
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

(cl-defun slime-collect-presentations (&optional (buffer (current-buffer)))
  "Return a list of (start-pos . end-pos) pairs for each slime presentation in BUFFER."
  (let ((positions))
    (slime-for-each-presentation-in-region (window-start) (window-end)
                                           (lambda (presentation start end whole-p)
                                             (push (cons start end) positions))
                                           buffer)
    (reverse positions)))

(defun slime-avy-copy-presentation-to-point ()
  (interactive)
  (let ((candidates (mapcan (lambda (window)
                              (with-selected-window window
                                (mapcar (lambda (bounds)
                                          (cons (car bounds) window))
                                        (slime-collect-presentations))))
                            (avy-window-list))))
    (avy-with slime-avy-copy-presentation-to-point
      (avy--process
       candidates
       (avy--style-fn 'de-bruijn)))
    (slime-copy-presentation-at-point-to-kill-ring (point))
    (avy-pop-mark)
    (with-current-buffer (slime-repl-buffer)
      (call-interactively #'yank))))

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

(defun slime-repl-complete-ivy ()
  (interactive)
  (let ((initial-input (slime-repl-current-input)))
    (ivy-read "REPL search: " slime-repl-input-history
              :initial-input initial-input
              :action (lambda (text)
                        (slime-repl-delete-current-input)
                        (insert text)))))

;;* Code refactoring utils
;;** `lisp-toggle-*-form'
(defvar lisp-keywords-with-*-variant nil
  "Keywords that have a *-variant. Assoc list ((major-mode . keywords)).")

(defmacro define-*-keywords (mode &rest keywords)
  (let ((regexps (mapcar (lambda (keyword)
                           `(and "(" symbol-start ,keyword (? "*") symbol-end))
                         keywords)))
    `(setf (alist-get ',mode lisp-keywords-with-*-variant) (rx (or ,@regexps)))))

(define-*-keywords lisp-mode "let" "do" "list" "prog")
(define-*-keywords emacs-lisp-mode "let" "if-let" "when-let" "do" "list" "prog")

(defun lisp-toggle-*-form (arg)
  "Toggle * of ARGth nearest enclosing form that has a *-variant."
  (interactive "p")
  (when-let ((regexp (alist-get major-mode lisp-keywords-with-*-variant)))
    (save-excursion
      (condition-case err
          (cl-loop do (up-list -1 t t)
                   counting (looking-at regexp) into i
                   when (= i arg)
                   do (progn (forward-symbol 1)
                             (if (looking-back "\\*")
                                 (backward-delete-char 1)
                               (insert "*"))
                             (up-list -1 t t)
                             (indent-sexp)
                             (return)))
        (scan-error nil)))))

(defun toggle-char (char pos &optional after)
  "Toggle CHAR before POS. If AFTER is T, toggle after POS."
  (save-excursion
    (goto-char pos)
    (if (eq char (if after (char-after) (char-before)))
        (if after (delete-char 1) (backward-delete-char 1))
      (insert char))))

(defun lisp-toggle-tick ()
  "Toggle ' at the start of current region(if active) or symbol."
  (interactive)
  (when-let ((pos (if (region-active-p)
                      (region-beginning)
                    (car (bounds-of-thing-at-point 'symbol)))))
    (toggle-char ?\' pos)))

(dolist (map (list lisp-mode-map emacs-lisp-mode-map slime-mode-map))
  (define-key map (kbd "C-c C-8") 'lisp-toggle-*-form)
  (define-key map (kbd "C-c C-'") 'lisp-toggle-tick)
  (define-key map (kbd "C-c C-'") 'lisp-toggle-tick)
  (define-key map (kbd "C-c '") 'lisp-toggle-tick)
  (define-key map (kbd "C-c '") 'lisp-toggle-tick))

;; copy to repl
(defun slime--repl-insert-string (string)
  (slime-switch-to-output-buffer)
  (goto-char slime-repl-input-start-mark)
  (insert string))

(defun slime-expression-at-point ()
  (or (and (looking-back (rx (or symbol-end ")")))
           (slime-last-expression))
      (slime-symbol-at-point)))

(defun slime-call-toplevel ()
  "Like `slime-call-defun', but treat unknown forms as function definitions."
  (interactive)
  ;; Reuse frame when popping to repl.
  (let ((display-buffer-alist
         (cons '("\\*slime-repl" nil (reusable-frames . t))
               display-buffer-alist)))
    (condition-case nil
        (progn (call-interactively #'slime-call-defun) t)
      (error
       (when-let ((toplevel (slime-parse-toplevel-form))
                  (qualified-name (and (symbolp toplevel)
                                       (slime-qualify-cl-symbol-name toplevel))))
         (slime--repl-insert-string (format "(%s )" qualified-name))
         (backward-char 1)
         t)))))

(defun slime-copy-to-repl (toplevel)
  "Copy region to repl if active, else copy last sexp.
With prefix arg, copy toplevel form."
  (interactive "P")
  (cond ((region-active-p)
         (slime--repl-insert-string
          (buffer-substring-no-properties (region-beginning) (region-end))))
        (toplevel
         (or (slime-call-toplevel)
             (destructuring-bind (beg end) (slime-region-for-defun-at-point)
               (slime--repl-insert-string
                (buffer-substring-no-properties beg end)))))
        (t (slime--repl-insert-string (slime-expression-at-point)))))

(define-key slime-mode-map [remap slime-call-defun] 'slime-copy-to-repl)
(define-key slime-repl-mode-map (kbd "C-c C-y") 'slime-copy-to-repl)

;; evaluation
(defun slime-eval-last-expression-eros ()
  (interactive)
  (destructuring-bind (output value)
      (slime-eval `(swank:eval-and-grab-output ,(slime-expression-at-point)))
    (eros--make-result-overlay (concat output value)
      :where (point)
      :duration eros-eval-result-duration)))

(define-key slime-mode-map (kbd "C-x C-e") 'slime-eval-last-expression-eros)
(define-key slime-repl-mode-map (kbd "C-x C-e") 'slime-eval-last-expression-eros)

;; asdf
(with-eval-after-load 'slime-asdf
  (defun slime-load-system-dwim (reload)
    "Compile and load an ASDF system. With prefix arg reload it instead."
    (interactive "P")
    (when-let* ((prompt (if reload "Reload system" "Load system"))
                (system (slime-read-system-name prompt nil t)))
      (if reload
          (slime-reload-system system)
        (slime-load-system system))))

  (define-key slime-mode-map (kbd "C-c L") 'slime-load-system-dwim))

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
(define-key sldb-mode-map (kbd "<tab>") 'sldb-toggle-details)
(define-key slime-inspector-mode-map (kbd "DEL") 'slime-inspector-pop)
(define-key slime-mode-map (kbd "C-c p") 'slime-pprint-eval-last-expression)
(define-key slime-mode-map (kbd "C-c C-i") 'slime-complete-symbol-global)
(define-key slime-repl-mode-map (kbd "C-c C-i") 'slime-complete-symbol-global)
(define-key slime-repl-mode-map (kbd "C-c C-v") nil)
(define-key slime-mode-map (kbd "C-c C-t") 'slime-trace-dialog-toggle-trace)
(define-key slime-mode-map (kbd "C-c t") 'slime-trace-dialog)
(define-key slime-mode-map (kbd "C-c M-t") 'slime-toggle-trace-fdefinition)
(define-key slime-repl-mode-map [remap swiper-at-point] 'swiper-isearch)
(define-key slime-repl-mode-map [remap slime-repl-previous-matching-input] 'slime-repl-complete-ivy)

;; package-related utils
(define-key slime-mode-map (kbd "C-c w") 'slime-kill-package-name)
(define-key slime-mode-map (kbd "C-c C-p") 'slime-sync-package-and-default-directory)
(define-key sldb-mode-map (kbd "C-c C-p") 'sldb-sync-frame-package)
(define-key slime-repl-mode-map (kbd "C-c C-p") 'slime-repl-set-default-package)
(define-key slime-mode-map (kbd "C-c C-x C-s") 'slime-export-symbol-at-point)


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

(define-key slime-presentation-command-map (kbd "C-v") 'slime-avy-copy-presentation-to-point)

(with-eval-after-load 'lispy
  (define-key emacs-lisp-mode-map (kbd "C-c C-x C-x") 'hydra-lispy-x/body)
  (define-key slime-mode-map (kbd "C-c C-x C-x") 'hydra-lispy-x/body)
  (define-key slime-mode-map (kbd "M-s") 'lispy-splice))


(provide 'configure-lisp)
