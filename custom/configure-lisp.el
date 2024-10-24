;;; -*- lexical-binding: t -*-
(require 'ace-link)
(require 'slime)
(require 'slime-autoloads)


;;* lisp ampersand completion
(defvar lisp-lambda-list-keyword-alist
  '((t "&optional" "&rest")
    (emacs-lisp-mode "&optional" "&rest"
     ;; cl-defun/defun* only (see (cl) Argument Lists):
     "&key" "&aux" "&body")
    (lisp-mode
     ;; (eww-open-file "~/quicklisp/HyperSpec/Body/03_d.htm")
     "&allow-other-keys" "&environment" "&rest" "&aux" "&key" "&whole"
     "&body" "&optional"))
  "Alist MODE or T -> list of keywords as strings.")

(defun lisp-complete-lambda-list-keyword ()
  "Try to complete a lambda-list keyword (&optional &rest etc)."
  (let* ((amp? nil)
         (beg (save-excursion
               (backward-sexp 1)
               (setq amp? (= ?& (char-after)))
               (point))))
    (when amp?
      (list beg (point)
            (completion-table-dynamic
             (lambda (&rest _)
               (or (alist-get major-mode lisp-lambda-list-keyword-alist)
                   (alist-get t lisp-lambda-list-keyword-alist))))
            :exclusive 'no))))

(defun lisp-enable-lambda-list-keywords-completion ()
  (add-hook 'completion-at-point-functions
            #'lisp-complete-lambda-list-keyword nil 'local))

;; NOTE if using slime, add `lisp-complete-lambda-list-keyword' to
;; `slime-completion-at-point-functions' instead.
(dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook))
  (add-hook hook 'lisp-enable-lambda-list-keywords-completion))

;;* cl
(setq inferior-lisp-program (or (getenv "LISP_BINARY") "sbcl"))

(add-to-list 'auto-mode-alist '("\\.cl\\'" . common-lisp-mode))

;;* slime repl history
(setq slime-repl-history-size 4096)
(setq slime-repl-history-file (expand-file-name "~/.slime/slime-history.eld"))

;;* slime-contribs
(setq slime-contribs '(slime-repl
                       slime-autodoc
                       slime-asdf
                       slime-editing-commands
                       slime-fancy-inspector
                       slime-fancy-trace
                       ;; slime-fuzzy
                       slime-mdot-fu
                       slime-macrostep
                       slime-presentations
                       slime-package-fu
                       slime-scratch
                       slime-references
                       ;; slime-fontifying-fu
                       slime-trace-dialog
                       slime-indentation
                       ;; slime-cl-indent
                       ;; slime-uncompiled-fringe
                       slime-tramp
                       slime-xref-browser
                       slime-c-p-c
                       inferior-slime
                       ;; slime-mrepl
                       ))
;;** allegro hacks
(when (require 'slime-allegro-hacks nil t)
  (push 'slime-allegro-hacks slime-contribs))

(slime-setup slime-contribs)

;;* uiop support for slime-package-fu
(setq slime-defpackage-regexp
      (rx line-start "("
          (or (and (? (or "cl:" "common-lisp:")) "defpackage")
              (and (? (or "uiop:" "uiop/package:" "package:")) "define-package"))
          symbol-end
          (* space)))

;;* slime-search-buffer-package fix
(defun slime-search-buffer-package+ ()
  "Same as `slime-search-buffer-package', but handle the case
when cursor is directly inside the in-package form."
  (save-excursion (beginning-of-line)
                  (slime-search-buffer-package)))
(setq slime-find-buffer-package-function #'slime-search-buffer-package+)

;;* slime fasl dir
(let ((dir "/tmp/slime-fasls/"))
  (unless (file-exists-p dir)
    (make-directory dir))
  (when (file-exists-p dir)
    (setf (plist-get slime-compile-file-options :fasl-directory) dir)))

;;* Paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;; We use lispy in eval-expression minibuffer.
;;(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook       #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(define-key paredit-mode-map (kbd "C-}") nil)
(define-key paredit-mode-map (kbd "C-{") nil)

;;* skip parens when reading symbol-at-point
(defun symbol-at-point--skip-parens (orig-fn &rest args)
  "Handle cases like |(((symbol or symbol)))| where | is the cursor position."
  (save-excursion
    (skip-chars-forward "(")
    (skip-chars-backward ")")
    (apply orig-fn args)))

(advice-add 'slime-symbol-at-point :around #'symbol-at-point--skip-parens)
(advice-add 'elisp-slime-nav--read-symbol-at-point :around #'symbol-at-point--skip-parens)

;;* Elisp
;;** indentation
;; It seems easier to use `common-lisp-indent-function' for stuff such as
;; labels & loop, and manually fix indent of elisp-specific forms(if, when-let, etc)
;; TODO: refactor this
;; This was a bad idea, better to just copy indentation for stuff like
;; loop from `common-lisp-indent-function'.
(setq lisp-indent-function 'common-lisp-indent-function)
(with-eval-after-load 'cl-indent
  (labels ((%copy-indent (new old)
             (put new 'common-lisp-indent-function
                  (get old 'common-lisp-indent-function))))
    (%copy-indent 'cl-flet 'flet)
    (%copy-indent 'flet 'flet)
    (%copy-indent 'cl-labels 'labels)
    (%copy-indent 'cl-defun 'defun)
    (%copy-indent 'cl-defmacro 'defmacro)
    (%copy-indent 'when-let 'when)
    (%copy-indent 'when-let* 'when)
    (%copy-indent 'eval-after-load 'when)
    (%copy-indent 'letf 'let)
    (%copy-indent 'letf* 'let)
    (%copy-indent 'cl-letf 'let)
    (%copy-indent 'cl-letf* 'let)
    (%copy-indent 'cl-typecase 'typecase)
    (%copy-indent 'while 'when)
    (%copy-indent 'evil-define-key 'defun)
    (%copy-indent 'avy-with 'when)
    (%copy-indent 'with-ivy-window 'save-excursion)
    (%copy-indent 'evil-define-command 'defun)
    (%copy-indent 'cl-destructuring-bind 'destructuring-bind)
    (put 'if 'common-lisp-indent-function 2)
    (put 'if-let 'common-lisp-indent-function 2)
    (put 'if-let* 'common-lisp-indent-function 2)
    (%copy-indent '-let 'let)
    (%copy-indent 'rx-let 'let)
    (%copy-indent 'transient-append-suffix 'defun)
    (%copy-indent 'condition-case 'case)
    (put 'pretty-hydra-define 'common-lisp-indent-function 2)))

;;** documentation
(defvar *elisp-documentation-last-symbol* nil
  "Last symbol for which documentation was queried.")

(defun elisp-documentation (prompt &optional symbol)
  "Show documentation for SYMBOL in the minibuffer.
If SYMBOL is not provided, use symbol-at-point.
With prefix arg prompt for symbol first.
When called second time consecutively, call `helpful-symbol' for SYMBOL."
  (interactive "P")
  (if (and (eq this-command last-command)
           *elisp-documentation-last-symbol*
           (fboundp 'helpful-symbol))
      (helpful-symbol *elisp-documentation-last-symbol*)
    (when-let* ((symbol (or symbol
                            (if prompt
                                (intern (completing-read "Show documentation for: "
                                                         obarray nil t nil nil
                                                         (when-let ((s (symbol-at-point)))
                                                           (symbol-name s))))
                              (symbol-at-point))))
                (doc (if (or (functionp symbol)
                             (macrop symbol))
                         (documentation symbol)
                       (documentation-property symbol 'variable-documentation))))
      (setq *elisp-documentation-last-symbol* symbol)
      ;; TODO: truncate docs that are more that a page long
      (display-truncated-message "%s" doc))))

(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'elisp-documentation)
(define-key read-expression-map (kbd "C-c C-d") 'elisp-documentation)
(with-eval-after-load 'ielm
  (define-key inferior-emacs-lisp-mode-map (kbd "C-c C-d") 'elisp-documentation))

;;** evaluation
(require 'eros)

(defun eros-eval-last-sexp-dwim ()
  "Eval region if active, eval symbol-at-point if any,
else call eros-eval-last-sexp."
  (interactive)
  (if (region-active-p)
      (let ((lines (count-lines (region-beginning) (region-end))))
        (call-interactively #'eval-region)
        (message "Evaluated %d line(s)." lines))
    (if-let ((symbol-end (cdr (bounds-of-thing-at-point 'symbol))))
        (eros--eval-overlay
         (save-excursion
          (goto-char symbol-end)
          (call-interactively #'eval-last-sexp))
         symbol-end)
      (call-interactively #'eros-eval-last-sexp))))

(global-set-key [remap eval-last-sexp] #'eros-eval-last-sexp-dwim)
(global-set-key [remap eval-defun] #'eros-eval-defun)

;;** pp-eval
(defun pp-eval-dwim (&optional expression)
  "Evaluate EXPRESSION and pretty-print its value. If region is active, copy it
as initial value when reading expression.
Also add the value to the front of the list in the variable `values'."
  (interactive)
  (let ((expression
         (or expression
             (read--expression
              "Eval: "
              (when (region-active-p)
                (buffer-substring-no-properties
                 (region-beginning) (region-end))))))
        (out-buffer-name "*Pp Eval Output*")
        (temp-buffer-show-function #'pop-to-buffer))
    (message "Evaluating...")
    (push (eval expression lexical-binding) values)
    ;; From `pp-display-expression'. We always want to create a new buffer.
    ;; TODO extract this to somewhere, maybe create a pp-result-mode
    (with-output-to-temp-buffer out-buffer-name
      (pp expression)
      (with-current-buffer standard-output
        (emacs-lisp-mode)
        (setq buffer-read-only nil)
        (set (make-local-variable 'font-lock-verbose) nil)
        (local-set-key (kbd "C-c C-q") 'quit-window)
        (when (fboundp 'evil-insert-state)
          (evil-insert-state))))))

(global-set-key (kbd "C-x M-e") 'pp-eval-dwim)
(define-key emacs-lisp-mode-map (kbd "C-c M-e") 'pp-eval-dwim)

;;** elisp-slime-nav
(require 'elisp-slime-nav)

(define-key elisp-slime-nav-mode-map (kbd "C-c C-d") nil)

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook eval-expression-minibuffer-setup-hook))
  (add-hook hook 'elisp-slime-nav-mode))

;;** ielm toggle
(defun elisp-switch-to-ielm ()
  "Switch to ielm buffer if exists or call `ielm'.
If there was an active region, insert it into repl."
  (interactive)
  (let ((selection (and (region-active-p)
                        (buffer-substring-no-properties (region-beginning) (region-end)))))
    (if-let ((buf (get-buffer "*ielm*")))
        (pop-to-buffer buf)
      (ielm))
    (when selection (insert selection))))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'elisp-switch-to-ielm)
(with-eval-after-load 'ielm
  (define-key inferior-emacs-lisp-mode-map (kbd "C-c C-z") 'quit-window))

;;* TODO: edebug-mode: make compatible with evil-mode, add hydra
(define-key edebug-mode-map (kbd "C-c C-d") nil)

;;** debugger-mode
(with-eval-after-load 'debug
  (define-key debugger-mode-map "j" 'next-line)
  (define-key debugger-mode-map "k" 'previous-line))

;;** hydras in imenu
(add-hook 'emacs-lisp-mode-hook #'hydra-add-imenu)


;;* slime hacks
;;** documentation
(defun slime-documentation-symbol (symbol-name)
  (slime-eval `(swank:documentation-symbol ,symbol-name)))

(defun slime-documentation-minibuffer ()
  "Display `swank:documentation-symbol' in the minibuffer"
  (interactive)
  (when-let* ((symbol-name (slime-read-symbol-name "Describe symbol: "))
              (doc (slime-documentation-symbol symbol-name)))
    (display-truncated-message doc)))

;; Slime documentation is opened in fundamental-mode by default. Force it to open
;; in help-mode instead. MAYBE: define slime-description-mode.
(defun slime-show-description-help-mode (string package)
  "Like `slime-show-description', but set description buffer to Help-mode."
  (let ((bufname (slime-buffer-name :description)))
    (slime-with-popup-buffer (bufname :package package
                                      :connection t
                                      :select slime-description-autofocus
                                      :mode 'help-mode)
      (princ string)
      (goto-char (point-min)))))
(advice-add 'slime-show-description :override #'slime-show-description-help-mode)

(setf (alist-get (rx "*slime-description" (* any)) display-buffer-alist nil nil #'equal)
      '((display-buffer-in-side-window)
        (window-width . fit-window-to-buffer)
        (side . left)
        (slot . 0)))

;;*** hyperspec

;; local hyperspec
(load "~/quicklisp/clhs-use-local.el" t)

;; browse hyperspec in eww
(defun hyperspec-lookup-advice (func &rest args)
  (let ((browse-url-browser-function #'eww-browse-url))
    (apply func args)))

(advice-add 'hyperspec-lookup :around #'hyperspec-lookup-advice)
(advice-add 'hyperspec-lookup-reader-macro :around #'hyperspec-lookup-advice)
(advice-add 'hyperspec-lookup-format :around #'hyperspec-lookup-advice)

;;** edit definition(M-.)
(defun slime--edit-definition-ivy (&optional symbol-name where)
  "Adapted from `slime-edit-definition-cont'. Use `ivy' to select a candidate if multiple."
  (unless symbol-name
    (setq symbol-name (slime-read-symbol-name "Edit definition of: ")))
  (or (run-hook-with-args-until-success 'slime-edit-definition-hooks symbol-name where)
      (let ((xrefs (slime-find-definitions symbol-name)))
        (cl-destructuring-bind (same-loc file-alist) (slime-analyze-xrefs xrefs)
          (cond ((null xrefs)
                 (error "No known definition for: %s (in %s)" symbol-name (slime-current-package)))
                (same-loc
                 (slime-push-definition-stack)
                 (slime-pop-to-location (slime-xref.location (car xrefs)) where))
                ;; ((:error "..."))
                ((slime-length= xrefs 1)
                 (error "%s" (cl-second (slime-xref.location (car xrefs)))))
                (t
                 (let* ((items (mapcar (lambda (xref)
                                         (unless (eq :error (caadr xref))
                                           (let* ((spec (downcase
                                                         (replace-regexp-in-string "[\n ]+" " " (slime-xref.dspec xref))))
                                                  (location (slime-xref.location xref))
                                                  (file (cl-second (assoc :file location)))
                                                  (position (cl-second (assoc :position location)))
                                                  (line (with-current-buffer (find-file-noselect file)
                                                          (line-number-at-pos position))))
                                             (and spec file line location (list spec file line location)))))
                                       xrefs))
                        (sorted-items (sort (remove nil items)
                                            (lambda (i1 i2)
                                              (if (string= (cl-second i1) (cl-second i2))
                                                  (< (cl-third i1) (cl-third i2))
                                                (string< (cl-second i1) (cl-second i2)))))))
                   (ivy-read "Edit definition of: "
                             sorted-items
                             :action (lambda (item)
                                       (slime-push-definition-stack)
                                       (slime-pop-to-location (cl-fourth item) where))))))))))

(defun slime-edit-definition-ivy (arg)
  "`slime-edit-definition' but use `ivy' to select a candidate.
With negative prefix arg call original `slime-edit-definition'."
  (interactive "p")
  (if (minusp arg)
      (call-interactively #'slime-edit-definition)
    (slime--edit-definition-ivy nil nil)))

(defun slime-edit-definition-other-window-ivy (arg)
  "`slime-edit-definition-ivy' but open result in other window."
  (interactive "p")
  (if (minusp arg)
      (call-interactively #'slime-edit-definition-other-window)
    (slime--edit-definition-ivy nil 'window)))

(defun slime-edit-definition-other-frame-ivy (arg)
  "`slime-edit-definition-ivy' but open result in other frame."
  (interactive "p")
  (if (minusp arg)
      (call-interactively #'slime-edit-definition-other-frame)
    (slime--edit-definition-ivy nil 'frame)))

;;** package-related stuff
(defun slime-kill-package-name (&optional symbol-at-point)
  "Kill current package name. With prefix arg or if region is
active, kill fully qualified symbol-at-point/region."
  (interactive "P")
  (let* ((symbol (cond (symbol-at-point (slime-symbol-at-point))
                       ((region-active-p)
                        (buffer-substring-no-properties
                         (region-beginning) (region-end)))))
         (kill (if symbol
                   (slime-qualify-cl-symbol-name symbol)
                 (slime-pretty-package-name (or (slime-current-package)
                                                (error "No current package."))))))
    (message kill)
    (kill-new kill)))

(defun slime-qualify-cl-symbol-name-from-lisp (symbol-or-name)
  "Qualify SYMBOL-OR-NAME by asking lisp.
`slime-qualify-cl-symbol-name' will just take current package."
  (unless symbol-or-name (error "SYMBOL-OR-NAME is nil."))
  (let ((name (if (symbolp symbol-or-name)
                  (symbol-name symbol-or-name)
                symbol-or-name)))
    (destructuring-bind (package status)
        (slime-eval
         `(cl:multiple-value-bind (symbol status)
              (cl:find-symbol ,name)
            (cl:when symbol
              (cl:list
               (cl:package-name
                (cl:find-package (cl:symbol-package symbol)))
               status))))
      (concat (or package "")
              (case status
                ((:inherited :internal) ":")
                (:external "::")
                (otherwise ""))
              name))))

(advice-add 'slime-qualify-cl-symbol-name :override
            #'slime-qualify-cl-symbol-name-from-lisp)

;;*** nicknames
(defun slime-get-package-name-and-nicknames (symbol-or-name)
  (unless symbol-or-name (error "SYMBOL-OR-NAME is nil."))
  (let ((name (if (symbolp symbol-or-name)
                  (symbol-name symbol-or-name)
                (string-trim-right symbol-or-name ":"))))
    (slime-eval
     `(cl:let ((package (cl:find-package ,name)))
        (cl:when package
          (cl:cons (cl:package-name package)
                   (cl:package-nicknames package)))))))

(defun slime-get-qualified-symbol-variants (symbol-or-name)
  "E.g. member => '(common-lisp:member cl:member lisp:member)."
  (unless symbol-or-name (error "SYMBOL-OR-NAME is nil."))
  (let* ((symbol-name (if (symbolp symbol-or-name)
                          (symbol-name symbol-or-name)
                        symbol-or-name))
         (name (slime-cl-symbol-name symbol-name))
         (package (slime-cl-symbol-package symbol-name))
         (current-package (string-trim-left (slime-current-package)
                                            (rx (or ":" "#")))))
    (slime-eval
     `(cl:multiple-value-bind (symbol status)
          (cl:find-symbol ,name ,@(when package (list package)))
        (cl:let ((package (cl:and symbol (cl:symbol-package symbol))))
          (cl:when package
            (cl:sort
             (cl:append
              ;; unqualified symbol if visible from current package
              (cl:and (cl:find-symbol ,name ,@(when current-package
                                                (list current-package)))
                      (cl:list ,name))
              (cl:loop :for p :in (cl:cons (cl:package-name package)
                                           (cl:package-nicknames package))
                 :collect (cl:format nil "~a~a~a"
                                     p
                                     (cl:if (cl:eq :internal status)
                                         "::" ":")
                                     (cl:symbol-name symbol))))
             #'cl:< :key #'cl:length)))))))

(defun slime-cycle-qualified-symbol (beg end)
  "Cycle between package nicknames of a symbol at point."
  (interactive (or (and (region-active-p)
                        (list (region-beginning)) (region-end))
                   (when-let ((bounds (bounds-of-thing-at-point 'symbol)))
                     (list (car bounds) (cdr bounds)))))
  (when-let* ((name (buffer-substring-no-properties beg end))
              (variants (slime-get-qualified-symbol-variants name))
              (next (or (second (cl-member name variants :test #'equal))
                        (car variants))))
    (delete-region beg end)
    (insert next)))
(define-key slime-editing-map (kbd "H-u") 'slime-cycle-qualified-symbol)
(define-key slime-editing-map (kbd "C-c C-x C-u") 'slime-cycle-qualified-symbol)

;;*** slime-edit-package-definition
(defun slime-edit-package-definition (name &optional where)
  ;; TODO handle `where'
  (when-let ((name (car (slime-get-package-name-and-nicknames name))))
    (or (ignore-errors (slime-goto-package-source-definition name))
        (counsel-rg (format "defpackage.+%s" name)))))
(add-hook 'slime-edit-definition-hooks 'slime-edit-package-definition)

;;*** repl set package hack
(defun slime-repl-set-package--push-package (&rest args)
  ;; TODO: check if this works properly for multiple swank connections
  (let ((package (slime-lisp-package)))
    (cl-pushnew package slime-repl-package-stack)
    (cl-pushnew package slime-read-package-name-history)))
(advice-add 'slime-repl-set-package :after #'slime-repl-set-package--push-package)

;;*** slime-all-packages
;; TODO: use swank:shortest-package-nickname for nicknames
(defvar *slime-all-packages* nil)

(defun slime-all-packages (&optional update)
  (if (or (null *slime-all-packages*) update)
      (setq *slime-all-packages*
            (slime-eval
             `(cl:loop
                 for p in (cl:list-all-packages)
                 with k = (cl:find-package :keyword)
                 unless (cl:eq p k)
                   collect (cl:cons (cl:package-name p)
                                    (cl:package-nicknames p)))))
    *slime-all-packages*))

(defun slime-complete-package-name-exit-func (str status)
  (insert ":"))

(defun slime-package-name (&optional prefix)
  (when-let ((packages (slime-all-packages t)))
    (if prefix
        (cl-loop for p in packages
              when (string-prefix-p prefix (car p))
                collect p)
      packages)))

(defun slime-complete-package-name ()
  (let ((end (point))
        (beg (slime-symbol-start-pos)))
    (list beg end (completion-table-dynamic #'slime-package-name)
          :exclusive 'no
          :exit-function #'slime-complete-package-name-exit-func)))

;;** completion
;; (defvar *slime-internal-symbols* nil)
;; (defvar *slime-external-symbols* nil)

;;*** keyword args
(defun slime-operator-keyword-args (&optional prefix)
  "Return list of keyword args for operator before point."
  (when-let* ((op (slime-operator-before-point))
              (args (slime-eval
                     `(swank:operator-arglist ,op
                                              ,(slime-current-package)))))
    (when prefix
      (setq prefix (string-trim-left prefix ":")))
    (cl-loop for arg in (cdr (member '&key (read args)))
          until (= ?& (aref (symbol-name arg) 0))
          for name = (symbol-name arg)
          when (or (null prefix)
                   (string-prefix-p prefix name))
            collect (concat ":" (symbol-name arg)))))

(defun slime-complete-keyword-arg ()
  (when-let ((end (point))
             (beg (slime-symbol-start-pos))
             (prefix (unless (= beg end)
                       (string-trim-left (buffer-substring-no-properties beg end) ":")))
             (args (slime-operator-keyword-args prefix)))
    (list beg end args)))

;;*** completion-at-point
(defvar slime-completion-table-stage-1
  (completion-table-merge (completion-table-dynamic #'slime-operator-keyword-args)
                          (completion-table-dynamic #'slime-simple-completions)
                          ;; (slime-visible-symbols)
                          (completion-table-dynamic
                           (lambda (_) (slime-all-packages t)))
                          ;; #'slime-filename-completion
                          (completion-table-dynamic
                           (lambda (_) *slime-all-symbols*))
                          ))

(defun slime-completion-stage-1 ()
  (list (slime-symbol-start-pos) (point) slime-completion-table-stage-1 :exclusive 'no))
(setq slime-completion-at-point-functions-old slime-completion-at-point-functions)
(setq slime-completion-at-point-functions (list #'lisp-complete-lambda-list-keyword
                                                #'slime-completion-stage-1))

;;** slime-visible-symbols
(defun slime-visible-symbols ()
  (slime-eval
   `(cl:let ((symbols))
      (cl:do-symbols (s "cl-user" symbols)
        (cl:push s symbols)))
   (slime-current-package)))

;;** slime-find-all-symbols
(defvar *slime-all-symbols* nil)

(defun slime-refresh-all-symbols ()
  (interactive)
  (when (slime-connected-p)
    (message "slime-refresh-all-symbols: refreshing...")
    (slime-eval-async
     `(cl:let ((symbols (cl:make-hash-table))
               (names nil))
        (cl:do-all-symbols (symbol symbols)
          (cl:unless (cl:or (cl:keywordp symbol)
                            (cl:gethash symbol symbols))
            (cl:setf (cl:gethash symbol symbols) t)))
        (cl:with-standard-io-syntax
          (cl:maphash (cl:lambda (symbol _)
                        (cl:declare (cl:ignore _))
                        (cl:push (cl:string-downcase (cl:prin1-to-string symbol)) names))
                      symbols))
        names)
     (lambda (symbols)
       (setq *slime-all-symbols* symbols)
       (unless (minibuffer-window-active-p (selected-window))
         (message "slime-refresh-all-symbols: %s symbols." (length *slime-all-symbols*))))
     "CL-USER"))
  *slime-all-symbols*)

(defun slime-maybe-refresh-all-symbols ()
  (unless *slime-all-symbols*
    (slime-refresh-all-symbols)))

(add-hook 'slime-connected-hook #'slime-maybe-refresh-all-symbols)

(defvar slime-refresh-all-symbols-timer
  (run-with-idle-timer 30 t #'slime-refresh-all-symbols))

(define-key slime-mode-map (kbd "C-c <f5>") 'slime-refresh-all-symbols)
(define-key slime-repl-mode-map (kbd "C-c <f5>") 'slime-refresh-all-symbols)

(defun slime-find-all-symbols (&optional internal)
  "Return symbol-names of symbols from all registered packages."
  ;; TODO: Extract cl code as a slime contrib or something.
  ;; TODO: (CL) Add caching. Update cache via a compilation hook.
  (let ((symbols
          (if internal
              *slime-all-symbols*
            ;; (slime-eval
            ;;  `(cl:let ((symbols))
            ;;     (cl:do-all-symbols (symbol symbols)
            ;;       (cl:unless (cl:keywordp symbol)
            ;;         (cl:push (cl:string-downcase (cl:prin1-to-string symbol)) symbols)))
            ;;     (cl:nreverse symbols))
            ;;  "CL-USER")
            (slime-eval
             `(cl:let ((symbols))
                (cl:dolist (p (cl:list-all-packages))
                  (cl:do-external-symbols (symbol p)
                    (cl:unless (cl:keywordp symbol)
                      (cl:push (cl:string-downcase
                                (cl:prin1-to-string symbol))
                               symbols))))
                (cl:nreverse symbols))
             "CL-USER"))
          ))
    symbols))

;;*** slime-completing-read
(defvar slime-completing-read-func #'completing-read)

(defun slime-completing-read (prompt collection &optional predicate require-match
                                     initial-input hist def inherit-input-method)
  (funcall slime-completing-read-func prompt collection predicate require-match
           initial-input hist def inherit-input-method))

(defvar slime-ivy-read-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m ivy-minibuffer-map)
    m))

(defun slime-ivy-read (prompt &optional collection predicate require-match
                                initial-input hist def inherit-input-method)
  (ivy-read prompt collection
            :predicate predicate
            :require-match require-match
            :initial-input initial-input
            :history (or hist 'slime-minibuffer-history)
            :def def
            :keymap slime-ivy-read-map))

(setq slime-completing-read-func #'slime-ivy-read)

(define-key slime-ivy-read-map (kbd "<f5>") 'slime-refresh-all-symbols)

(defun slime-ivy-read--edit-definition ()
  (interactive)
  ;; HACK: Make it seem like completing-read didn't return anything
  (let ((symbol (ivy-state-current ivy-last)))
    (setf (ivy-state-current ivy-last) nil)
    (ivy-exit-with-action (lambda (_) (slime-edit-definition symbol)))))
(define-key slime-ivy-read-map (kbd "M-.") 'slime-ivy-read--edit-definition)

(defun slime-read-symbol-name-global (prompt &optional query internal)
  "Either read a symbol or choose one at point. Choose from
all external symbols if QUERY is non-nil, there is no symbol
at point or a single prefix arg is supplied. With double
prefix arg or if INTERNAL is non-nil include internal symbols."
  (let ((symbol (slime-symbol-at-point)))
    (when (string-prefix-p "#:" symbol)
      (setq symbol (substring symbol 2)))
    (cond ((or current-prefix-arg query (not symbol))
           (let ((internal (or internal
                               (= 16 (prefix-numeric-value current-prefix-arg)))))
             (slime-completing-read
              prompt
              (slime-find-all-symbols internal)
              nil nil
              symbol)))
          (t (slime-symbol-at-point)))))

(advice-add 'slime-read-symbol-name :override #'slime-read-symbol-name-global)

(defun slime--bounds-of-region-or-symbol ()
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (bounds-of-thing-at-point 'symbol)))

(defun slime-complete-symbol-global (external)
  "Complete a symbol searching symbols from all visible packages.
If INTERNAL is T, also search internal symbols."
  ;; TODO: xref and documentation lookups in completion buffer
  (interactive "P")
  (let* ((bounds (slime--bounds-of-region-or-symbol))
         (start (and bounds (car bounds)))
         (end (and bounds (cdr bounds)))
         (initial-input (and bounds (buffer-substring-no-properties start end)))
         (symbol (slime-completing-read "Find symbol: " (slime-find-all-symbols (not external)) nil nil initial-input)))
    (when symbol
      (when bounds
        (delete-region start end))
      (insert symbol))))

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
  "Complete input from repl history."
  (interactive)
  (let ((initial-input (slime-repl-current-input)))
    (ivy-read "REPL search: " slime-repl-input-history
              :initial-input initial-input
              :action (lambda (text)
                        (slime-repl-delete-current-input)
                        (insert text)))))

;;** slime-read-package-name
(defvar slime-read-package-name-history nil)

(defun slime-read-package-name* (prompt &optional initial-value)
  "Read a package name from the minibuffer, prompting with PROMPT."
  (let ((completion-ignore-case t))
    (completing-read prompt (slime-bogus-completion-alist
                             (slime-eval
                              `(swank:list-all-package-names t)))
		     nil t initial-value 'slime-read-package-name-history)))
(advice-add 'slime-read-package-name :override #'slime-read-package-name*)

;;** set `slime-buffer-package' in `edit-indirect'
(with-eval-after-load 'edit-indirect
  (defun edit-indirect--set-slime-package ()
    (when slime-mode
      (when-let* ((ov edit-indirect--overlay)
                  (buf (overlay-buffer ov))
                  (package (with-current-buffer buf
                             (slime-current-package))))
        (setq-local slime-buffer-package package))))
  (add-hook 'edit-indirect-after-creation-hook #'edit-indirect--set-slime-package))

;;** slime-switch-to-sldb-buffer
(defun slime-switch-to-sldb-buffer ()
  (interactive)
  (when-let* ((bufs (sldb-buffers))
              (buf (if (eq major-mode 'sldb-mode)
                       (cadr (member (current-buffer) bufs))
                     (car bufs))))
    (pop-to-buffer buf)))

(define-key slime-parent-map (kbd "C-c C-a") 'slime-switch-to-sldb-buffer)
(define-key slime-prefix-map (kbd "C-a") 'slime-switch-to-sldb-buffer)

;;** if* keyword form indentation
(setq common-lisp-indent-if*-keyword
      (rx (? ":") (or "else" "elseif" "then" "thenret")))

;;** if* syntax highlighting
(font-lock-add-keywords
 'lisp-mode
 (list
  ;; if*
  `(,(rx symbol-start (? ":") "then" symbol-end) 0 'font-lock-builtin-face)
  `(,(rx symbol-start (? ":") "thenret" symbol-end) 0 'font-lock-builtin-face)
  `(,(rx symbol-start (? ":") "else" symbol-end) 0 'font-lock-builtin-face)
  `(,(rx symbol-start (? ":") "elseif" symbol-end) 0 'font-lock-builtin-face)
  ;; highlight keywords with colons as errors because code style
  `(,(rx symbol-start ":then" symbol-end) 0 'style-warning-face prepend)
  `(,(rx symbol-start ":thenret" symbol-end) 0 'style-warning-face prepend)
  `(,(rx symbol-start ":else" symbol-end) 0 'style-warning-face prepend)
  `(,(rx symbol-start ":elseif" symbol-end) 0 'style-warning-face prepend)))

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
(define-*-keywords emacs-lisp-mode "let" "if-let" "when-let" "do" "list" "prog" "letf" "cl-letf")

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

;;** `lisp-toggle-tick'
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
  (define-key map (kbd "C-c C-'") 'lisp-toggle-tick))

;;* slime repl copying/killing
(defun slime--repl-insert-string (string &optional at-point)
  (when string
    (slime-switch-to-output-buffer)
    (unless at-point
      (goto-char slime-repl-input-start-mark))
    (insert string)))

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
  "Copy region to repl if active, else copy current form.
With prefix arg, copy toplevel form."
  (interactive "P")
  (cond ((region-active-p)
         (slime--repl-insert-string
          (buffer-substring-no-properties (region-beginning) (region-end)) t))
        (toplevel
         (or (call-interactively #'slime-call-toplevel)
             (destructuring-bind (beg end) (slime-region-for-defun-at-point)
               (slime--repl-insert-string
                (buffer-substring-no-properties beg end)))))
        (t (slime--repl-insert-string
            (save-excursion
             (when (slime-inside-string-p)
               (skip-chars-forward "^\"")
               (forward-char 1))
             (if-let ((bounds (bounds-of-thing-at-point 'list)))
                 (buffer-substring-no-properties (car bounds) (cdr bounds))
               ""))))))

(define-key slime-mode-map (kbd "C-c C-y") 'slime-copy-to-repl)
(define-key slime-repl-mode-map (kbd "C-c C-y") 'slime-copy-to-repl)

(defun slime-repl-kill-input-dwim (&optional all?)
  "Like `slime-repl-kill-input', but kill all input with prefix arg.
Also always use `kill-region' instead of `delete-region'."
  (interactive "P")
  (cond ((or all? (= (point) (marker-position slime-repl-input-start-mark)))
         (kill-region (slime-repl-history-yank-start) (point-max)))
        ((< (marker-position slime-repl-input-start-mark) (point))
         (kill-region slime-repl-input-start-mark (point)))))

(define-key slime-repl-mode-map [remap slime-repl-kill-input] 'slime-repl-kill-input-dwim)

;;* slime-c-p-c.el completion
;; Use ivy instead of a completion buffer
(cl-pushnew 'slime-c-p-c-completion-at-point slime-completion-at-point-functions)

(defun slime-display-completions-ivy (completions beg end)
  (ivy-completion-in-region beg end completions))

(advice-add 'slime-display-or-scroll-completions :override #'slime-display-completions-ivy)

;;* sldb-fancy-break
;; TODO: track/toggle breaks, highlight functions with breaks/traces etc
;; TODO: slime-read-function-name
(defun sldb-fancy-break (name)
  "Set a breakpoint at the start of the function NAME."
  (interactive (list (slime-read-symbol-name "Break on enter: " t)))
  (slime-eval-async `(swank:sldb-break ,name)
    (lambda (msg) (message "%s" msg))))

(define-key slime-parent-map (kbd "C-c B") 'sldb-fancy-break)

;;* evaluation(eros)
;; Display overlays with evaluation results
(with-eval-after-load 'eros
  (defun slime-eval-last-expression-eros ()
    (interactive)
    (destructuring-bind (output value)
        (slime-eval `(swank:eval-and-grab-output ,(slime-expression-at-point)))
      (eros--make-result-overlay (concat output value)
        :where (point)
        :duration eros-eval-result-duration)))

  (define-key slime-mode-map (kbd "C-x C-e") 'slime-eval-last-expression-eros)
  (define-key slime-repl-mode-map (kbd "C-x C-e") 'slime-eval-last-expression-eros))

;;* asdf
(with-eval-after-load 'slime-asdf
  (defun slime-load-system-dwim (reload)
    "Compile and load an ASDF system. With prefix arg reload it instead."
    (interactive "P")
    (when-let* ((prompt (if reload "Reload system" "Load system"))
                (system (slime-read-system-name prompt nil t)))
      (if reload
          (slime-reload-system system)
        (slime-load-system system))))

  (define-key slime-mode-map (kbd "C-c L") 'slime-load-system-dwim)
  (define-key slime-repl-mode-map (kbd "C-c L") 'slime-load-system-dwim))

;;** slime-edit-definition support for asdf components
(defun slime-edit-asdf-component (name &optional where)
  ;; (or current-prefix-arg (not (equal (slime-symbol-at-point) name)))
  (setq name (string-trim-right name ":"))
  (if current-prefix-arg
      nil
      (when-let* ((pathname
                   (slime-eval
                    `(cl:when (cl:find-package :asdf) ; TODO: a better way to check; featurep ?
                       (cl:let* ((symbol
                                   (swank::find-definitions-find-symbol-or-package ,name))
                                 (package (swank::guess-package ,name))
                                 (component
                                   (cl:or (asdf/find-component:find-component nil symbol)
                                       (cl:when package
                                         (asdf/find-component:find-component
                                          nil (cl:package-name package))))))
                         (cl:and component
                                 (cl:namestring
                                  (cl:or (cl:slot-value component
                                                        'asdf/component::source-file)
                                         (asdf/component:component-pathname component)))))))))
        ;; MAYBE: use `slime-edit-definition-cont'
        (slime-push-definition-stack)
        (case where
          (window (find-file-other-window pathname))
          (frame (find-file-other-frame pathname))
          (t (find-file pathname)))
        t)))

(add-hook 'slime-edit-definition-hooks 'slime-edit-asdf-component)

;;* `slime-import-symbol'
(with-eval-after-load 'slime-package-fu
  (require 'slime-import-symbol)
  (define-key slime-mode-map (kbd "C-c C-x <C-i>") 'slime-import-symbol))

;;* trace
(defun sldb-fetch-traces ()
  (interactive)
  (with-current-buffer (slime-trace-dialog--ensure-buffer)
    (slime-trace-dialog-fetch-traces)))

(defun sldb-continue-and-fetch-traces ()
  (interactive)
  (call-interactively #'sldb-continue)
  (call-interactively #'sldb-fetch-traces))

;;** fix for emacs 28: `make-text-button' now returns a new string.
(defun slime-trace-dialog--button (title lambda &rest props)
  (let ((string (format "%s" title)))
    (apply #'make-text-button string nil
           'action #'(lambda (button)
                       (funcall lambda button))
           'mouse-face 'highlight
           'face 'slime-inspector-action-face
           props)))

;;* fancier fancy-trace
(defun slime-toggle-trace-dwim (&optional using-context-p)
  "Toggle trace. Tries to guess what to trace depending on
major-mode. E.g. `slime-xref-mode'."
  (interactive "P")
  (let* ((spec (cond
                ((eq major-mode 'slime-xref-mode)
                 (when-let ((dspec (slime-xref-dspec-at-point)))
                   (slime-dspec-operator-name dspec)))
                (using-context-p
                 (slime-extract-context))
                (t (slime-symbol-at-point))))
         (spec (slime-trace-query spec)))
    (message "%s" (slime-eval `(swank-trace-dialog:dialog-toggle-trace
                                (swank::from-string ,spec))))
    (run-hooks 'slime-trace-dialog-after-toggle-hook)))

(define-key slime-prefix-map [remap slime-toggle-fancy-trace] 'slime-toggle-trace-dwim)
(define-key slime-prefix-map (kbd "C-c t") 'slime-trace-dialog)
(define-key slime-prefix-map (kbd "C-c T") nil)
(define-key slime-trace-dialog-minor-mode-map (kbd "C-c t") 'slime-trace-dialog)
(define-key slime-trace-dialog-minor-mode-map (kbd "C-c T") nil)

;;** slime-dspec-operator-name
(defun slime-dspec-operator-name (dspec)
  "Try to extract operator name from slime dspec."
  (let ((parsed-spec (car (read-from-string dspec))))
    (pcase parsed-spec
      (`(:operator (method (setf ,(and (pred symbolp) name) . ,_))) (symbol-name name))
      (`(:operator (method ,(and (pred symbolp) name) . ,_)) (symbol-name name))
      (`(:operator (setf ,(and (pred symbolp) name) . ,_)) (symbol-name name))
      (`(:operator ,(and (pred symbolp) name)) (symbol-name name))
      (_ nil))))

;;** slime-xref-toggle-trace-all
(defun slime-xref-toggle-trace-all (&optional untrace)
  "Toggle tracing for ALL operators in a `slime-xref-mode' buffer.
TODO: With prefix arg untrace all."
  (interactive "P")
  (when-let* ((xrefs (slime-all-xrefs))
              (queries
               (cl-loop for (dspec) in xrefs
                     when (slime-dspec-operator-name dspec)
                     collect `(swank-trace-dialog:dialog-toggle-trace
                               (swank::from-string ,it)))))
    (slime-eval `(cl:progn ,@queries))
    (run-hooks 'slime-trace-dialog-after-toggle-hook)))

;; (define-key slime-xref-mode-map (kbd "C-c A") 'slime-xref-toggle-trace-all)
(define-key slime-xref-mode-map (kbd "C-c T") 'slime-xref-toggle-trace-all)


;;** disable slime-autodoc-space in slime-xref buffer
(defun slime-autodoc--unbind-space ()
  (with-minor-mode-map-overriding (map slime-autodoc-mode)
    (define-key map (kbd "SPC") nil)))

(add-hook 'slime-xref-mode-hook 'slime-autodoc--unbind-space)

;;* avy-actions
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

;;* macrostep
(defun ace-link-macrostep ()
  "Jump to and expand a macro or collapse an expaded one."
  (interactive)
  (let ((pt (avy-with ace-link-macrostep
              (avy--process
               (ace-link--macrostep-collect)
               (avy--style-fn avy-style)))))
      (ace-link--macrostep-action pt)))

(defun ace-link--macrostep-action (pt)
  (when (number-or-marker-p pt)
    (goto-char pt)
    (if (get-text-property pt 'macrostep-macro-start)
        (macrostep-expand)
      (macrostep-collapse))))

(defun ace-link--macrostep-collect ()
  (let ((candidates (list))
        (prop-macro-start 'macrostep-macro-start)
        (ov-original-text 'macrostep-original-text))
    ;; macro forms that can be expanded
    (cl-loop with pt = (window-start)
          while (and pt (< pt (window-end)))
          when (get-text-property pt prop-macro-start)
            do (cl-pushnew pt candidates)
          do (setq pt (next-single-property-change pt prop-macro-start)))
    ;; macro forms that can be collapsed
    (cl-loop for ov in macrostep-overlays
          for start = (overlay-start ov)
          when (and (>= start (window-start))
                    (<= start (window-end)))
            do (cl-pushnew start candidates))
    (nreverse candidates)))

(setf (alist-get 'ace-link-macrostep avy-styles-alist) 'pre)

(with-eval-after-load 'macrostep
  (define-key macrostep-keymap (kbd "C-f") 'ace-link-macrostep)
  (define-key macrostep-keymap (kbd "C-c C-q") 'macrostep-collapse-all)

  (when (fboundp 'evil-mode)
    ;; From evil-collection-macrostep.el:
    ;; Keymaps don't seem to be populated on first try.
    ;; Force `evil' to normalize keymaps.
    ;; Why? Something to do with buffer-read-only?
    (add-hook 'macrostep-mode-hook #'evil-normalize-keymaps)
    (defvar macrostep-keymap)
    (evil-make-overriding-map macrostep-keymap 'normal)
    (evil-add-hjkl-bindings macrostep-keymap 'normal
      "u" 'macrostep-collapse
      "m" 'macrostep-expand
      "<return>" 'macrostep-expand
      "<backtab>" 'macrostep-prev-macro)))

;;* outline
(defvar lisp-outline-regexp (rx bol (or "(" ";;" "#|")))

(with-eval-after-load 'counsel
  (add-to-list 'counsel-outline-settings
               `(lisp-mode
                 :outline-regexp ,lisp-outline-regexp
                 :display-style 'title)))

(defun set-lisp-outline-regexp ()
  (setq-local outline-regexp lisp-outline-regexp))
(add-hook 'lisp-mode-hook #'set-lisp-outline-regexp)
(add-hook 'emacs-lisp-mode-hook #'set-lisp-outline-regexp)

;;* ielm
(setq ielm-dynamic-return nil)

;;** switch to ielm
(defun elisp-switch-to-ielm ()
  "Switch to ielm buffer if exists or call `ielm'.
If there was an active region, insert it into repl."
  (interactive)
  (let ((selection (and (region-active-p)
                        (buffer-substring-no-properties (region-beginning) (region-end)))))
    (if-let ((buf (get-buffer "*ielm*")))
        (pop-to-buffer buf)
      (ielm))
    (when selection (insert selection))))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'elisp-switch-to-ielm)
;;** keys
(with-eval-after-load 'ielm
  (define-key inferior-emacs-lisp-mode-map (kbd "C-c C-z") 'quit-window)
  (define-key inferior-emacs-lisp-mode-map (kbd "C-m") 'ielm-send-input)
  (define-key inferior-emacs-lisp-mode-map (kbd "C-j") 'ielm-return))

;;* avy-goto-paren-1
(defun avy-goto-paren-1 ()
  (interactive)
  (let ((avy-style 'at))
    (avy-goto-char-2 ?\( (read-char "char following paren: "))))

(define-key paredit-mode-map (kbd "C-(") nil)
(global-set-key (kbd "C-(") 'avy-goto-paren-1)
(global-set-key (kbd "C-9") 'avy-goto-paren-1)

;;* yasnippet in slime-repl
(defun slime-repl-activate-yasnippet ()
  (yas-activate-extra-mode 'lisp-mode))

(add-hook 'slime-repl-mode-hook #'slime-repl-activate-yasnippet)

;;* slime-repl-narrow-to-prompt
(defun slime-repl-narrow-to-prompt ()
  "Narrow buffer to the prompt (and any following output) at point."
  (interactive)
  (let ((start (save-excursion
                (move-end-of-line 1)
                (slime-repl-previous-prompt)
                (point)))
        (end (save-excursion
              (let ((p (point)))
                (slime-repl-next-prompt)
                (if (eq p (point))
                    (point-max)
                  (previous-line)
                  (line-end-position))))))
    (narrow-to-region start end)))

;;* TODO slime-inspector-copy-down-to-repl-other-window
;; The problem is that the slime-eval-async is async and save-selected-window
;; can't handle that. Need to pass a different callback probably
(defun slime-inspector-copy-down-to-repl-other-window ()
  (interactive)
  (save-selected-window
   (call-interactively #'slime-inspector-copy-down-to-repl)))

(define-key slime-inspector-mode-map (kbd "m") 'slime-inspector-copy-down-to-repl-other-window)

;;* TODO <f8> slime hydra
(dolist (lisp '("sbcl" "ecl"))
  (when (executable-find lisp)
    (pushnew (list (intern lisp)
                   (list lisp)
                   ;;:init slime-init-command
                   )
             slime-lisp-implementations)))

(defun slime* ()
  "Same as `slime' with a negative prefix argument."
  (interactive)
  (let ((current-prefix-arg '-))
    (call-interactively #'slime)))

(define-key global-map (kbd "<f8>") 'slime*)

;;* KEYS
(dolist (keymap (list slime-mode-map slime-repl-mode-map))
  (define-key keymap (kbd "C-c C-d C-d") 'slime-documentation-minibuffer)
  (define-key keymap (kbd "C-c C-d d") 'slime-documentation)
  (define-key keymap (kbd "C-c C-d C-g") nil)
  (define-key keymap (kbd "C-c C-d g") 'common-lisp-hyperspec-glossary-term)
  (define-key keymap [remap slime-edit-definition] 'slime-edit-definition-ivy)
  (define-key keymap [remap slime-edit-definition-other-window] 'slime-edit-definition-other-window-ivy)
  (define-key keymap [remap slime-edit-definition-other-frame] 'slime-edit-definition-other-frame-ivy))

(define-key slime-parent-map (kbd "C-4 .") 'slime-edit-definition-other-window-ivy)
(define-key slime-parent-map (kbd "C-4 C-.") 'slime-edit-definition-other-window-ivy)
(define-key slime-parent-map (kbd "C-5 .") 'slime-edit-definition-other-frame-ivy)
(define-key slime-parent-map (kbd "C-5 C-.") 'slime-edit-definition-other-frame-ivy)

(dolist (keymap (list sldb-mode-map slime-inspector-mode-map slime-trace-dialog-mode-map slime-xref-mode-map))
  (define-key keymap (kbd "k") 'previous-line)
  (define-key keymap (kbd "j") 'next-line)
  (define-key keymap (kbd "C-f") 'ace-link))

(define-key slime-repl-mode-map (kbd "<f5>") 'slime-restart-inferior-lisp)
(define-key slime-repl-mode-map (kbd "(") 'self-insert-command)
(define-key slime-repl-mode-map (kbd "C-c C-z") 'slime-next-connection)
(define-key slime-connection-list-mode-map (kbd "C-c t") 'slime-trace-dialog)
(define-key slime-connection-list-mode-map (kbd "t") 'slime-trace-dialog)
(define-key sldb-mode-map (kbd "<tab>") 'sldb-toggle-details)
(define-key slime-inspector-mode-map (kbd "DEL") 'slime-inspector-pop)
(define-key slime-inspector-mode-map (kbd "C-h") 'slime-inspector-pop)
(define-key slime-inspector-mode-map (kbd "C-w") 'slime-inspector-pop)
(define-key slime-inspector-mode-map (kbd "<mouse-8>") 'slime-inspector-pop)
;;(define-key slime-inspector-mode-map (kbd "w") 'slime-inspector-next-inspectable-object)
(define-key slime-inspector-mode-map (kbd "w") 'forward-word)
;;(define-key slime-inspector-mode-map (kbd "b") 'slime-inspector-previous-inspectable-object)
(define-key slime-inspector-mode-map (kbd "b") 'backward-word)
(define-key slime-mode-map (kbd "C-c p") 'slime-pprint-eval-last-expression)
(define-key slime-mode-map (kbd "C-c <C-i>") 'slime-complete-symbol-global)
(define-key slime-minibuffer-map (kbd "C-c <C-i>") 'slime-complete-symbol-global)
(define-key slime-repl-mode-map (kbd "C-c <C-i>") 'slime-complete-symbol-global)
(define-key slime-repl-mode-map (kbd "C-c C-v") nil)
(define-key slime-mode-map (kbd "C-c C-t") 'slime-trace-dialog-toggle-trace)
(define-key slime-mode-map (kbd "C-c t") 'slime-trace-dialog)
(define-key slime-repl-mode-map (kbd "C-c C-t") 'slime-trace-dialog-toggle-trace)
(define-key slime-repl-mode-map (kbd "C-c t") 'slime-trace-dialog)
(define-key sldb-mode-map (kbd "C-c C-t") 'slime-trace-dialog-toggle-trace)
(define-key sldb-mode-map (kbd "C-c t") 'slime-trace-dialog)
(define-key sldb-mode-map (kbd "T") 'slime-trace-dialog)
(define-key sldb-mode-map "G" 'sldb-fetch-traces)
(define-key sldb-mode-map "C" 'sldb-continue-and-fetch-traces)
(define-key sldb-mode-map "h" 'backward-char)
(define-key sldb-mode-map "l" 'forward-char)
(define-key sldb-mode-map (kbd "C-q") 'bury-buffer)
(define-key sldb-mode-map (kbd "C-M-j") 'sldb-down)
(define-key sldb-mode-map "f" 'sldb-show-source)
(define-key sldb-mode-map (kbd "C-M-k") 'sldb-up)
(define-key sldb-mode-map (kbd "C-j") 'sldb-down)
(define-key sldb-mode-map (kbd "C-k") 'sldb-up)
(define-key sldb-mode-map (kbd "w") 'forward-word)
(define-key sldb-mode-map (kbd "b") 'backward-word)
(define-key slime-mode-map (kbd "C-c M-t") 'slime-toggle-trace-fdefinition)
(define-key slime-repl-mode-map [remap swiper-at-point] 'swiper-isearch)
(define-key slime-repl-mode-map [remap slime-repl-previous-matching-input] 'slime-repl-complete-ivy)
(define-key slime-repl-mode-map (kbd "C-c p") 'slime-repl-previous-prompt)
(define-key slime-repl-mode-map (kbd "C-c n") 'slime-repl-next-prompt)
(define-key slime-mode-map (kbd "C-c a") 'slime-autodoc-manually)
(define-key slime-repl-mode-map (kbd "C-c a") 'slime-autodoc-manually)

;;** slime-next/prev-location
(define-key slime-editing-map (kbd "C-M-.") nil)
(define-key slime-editing-map (kbd "C-c j") 'slime-next-location)
(define-key slime-editing-map (kbd "C-c k") 'slime-previous-location)


;;** package-related utils
(define-key slime-mode-map (kbd "C-c M-w") 'slime-kill-package-name)
(define-key slime-mode-map (kbd "C-c C-p") 'slime-sync-package-and-default-directory)
(define-key sldb-mode-map (kbd "C-c C-p") 'sldb-sync-frame-package)
(define-key slime-repl-mode-map (kbd "C-c p") 'slime-repl-set-default-package)
(define-key slime-mode-map (kbd "C-c C-x C-s") 'slime-export-symbol-at-point)

;;** presentations
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

;;** slime-trace-dialog
(define-key slime-trace-dialog-mode-map "h" 'backward-char)
(define-key slime-trace-dialog-mode-map "l" 'forward-char)
(define-key slime-trace-dialog-mode-map "F" 'slime-trace-dialog-autofollow-mode)
(define-key slime-trace-dialog-mode-map "(" 'slime-trace-dialog-hide-details-mode)
(define-key slime-trace-dialog-mode-map "C-n" nil)
(define-key slime-trace-dialog--detail-mode-map (kbd "C-f") 'link-hint-open-link)

;;** slime-macroexpansion-minor-mode
(define-key slime-macroexpansion-minor-mode-map (kbd "C-c C-q") 'bury-buffer)

;;** slime-scratch
(define-key slime-mode-map (kbd "C-c M-s") 'slime-scratch)
(define-key slime-repl-mode-map (kbd "C-c M-s") 'slime-scratch)

;;** eval-in-repl
(require 'eval-in-repl)

(define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
(define-key slime-mode-map (kbd "<C-return>") 'eir-eval-in-slime)

;;** Elisp
(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'compile-defun)
(define-key emacs-lisp-mode-map (kbd "C-c <return>") 'macrostep-expand)
(define-key emacs-lisp-mode-map (kbd "C-c RET") 'macrostep-expand)

;;** Global `lisp-mode' mapping
(define-key global-map (kbd "C-c C-l") 'lisp-mode)

;;* lispy
(require 'configure-lispy)

(provide 'configure-lisp)
