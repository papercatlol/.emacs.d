(require 'slime-package-fu)
;; slime-package-fu: `slime-goto-package-source-definition', `slime-format-symbol-for-defpackage'

(cl-defun slime-package--add-import (symbol-name package)
  "Tries to insert an import for SYMBOL-NAME from PACKAGE.
For new imports uses
\\(:import-from :package
                #:symbol\\)
 style.
 Prompts for nickname to use if there are any."
  ;; TODO: Support :shadowing-import-from.
  ;; MAYBE: Some control over package nicknames when printing fully
  ;; quialified symbols e.g. alexandria:if-let vs alexandria.1.0.0:if-let
  (save-excursion
   (slime-goto-package-source-definition (slime-current-package))
   (let ((buf (current-buffer))
         (package-symbol (if (symbolp package)
                             package
                           (make-symbol (upcase package))))
         (definition-end (save-excursion (forward-sexp 1) (1- (point))))
         package-import first-export)
     (down-list 1)
     (forward-sexp 1)
     (loop for sexp-start = (point)
           for sexp = (read buf)
           do (when (consp sexp)
                (when (or (eq :import-from (car sexp))
                          (eq :shadowing-import-from (car sexp)))
                  (when (slime-eval `(cl:eq (cl:find-package ',package-symbol)
                                            (cl:find-package ',(second sexp))))
                    ;; Check if symbol-name is already imported.
                    (dolist (imported-symbol (cddr sexp))
                      ;; In Elisp (symbol-name :a) is not equal to (symbol-name 'a),
                      ;; so we have to do some ugly stuff. Is there a better way?
                      (when (string= (downcase symbol-name)
                                     (downcase (string-trim-left (symbol-name imported-symbol) ":")))
                        (message "Symbol %s:%s is already imported"
                                 package
                                 (downcase symbol-name))
                        ;; MAYBE throw user-error here in true emacs fashion
                        (return-from slime-package--add-import nil)))
                    (setq package-import sexp-start)
                    (return)))
                (when (and (null first-export) (eq :export (car sexp)))
                  (setq first-export sexp-start)))
           until (= (point) definition-end))
     (cond
       ;; There is an (:import-from PACKAGE ..) form already. Append new import to it.
       (package-import
        (goto-char package-import)
        (forward-list 1)
        (backward-char 1)
        (when (looking-back (rx symbol-end))
          (newline-and-indent))
        (insert (slime--format-symbol-name symbol-name)))
       ;; Insert a new (:import-from PACKAGE ..) before first-export
       ;; or at the end of the package declaration.
       (t
        (let* ((nicknames (slime-eval `(cl:package-nicknames
                                        (cl:find-package ',package-symbol))))
               (package-name (slime--format-package-name package-symbol))
               (import-name (if nicknames
                                (completing-read (format "Import %s as: " package-symbol)
                                                 (cons package-name
                                                       (mapcar #'slime--format-package-name
                                                               nicknames)))
                              package-name)))
          (when import-name
            (goto-char (or first-export definition-end))
            (newline-and-indent)
            (insert (format "(:import-from %s" (downcase import-name)))
            (newline-and-indent)
            (insert (format "%s)" (slime--format-symbol-name symbol-name)))))))
     (call-interactively #'slime-compile-defun))))

(defun slime--format-package-name (package-name)
  (downcase (format ":%s" package-name)))

(defun slime--format-symbol-name (symbol-name)
  (downcase (format "#:%s" symbol-name)))

;; Avoid recursive import
;; TODO: move slime-read-symbol-name-global to a separate package
(declare-function slime-read-symbol-name-global "configure-lisp.el")

(defun slime-import-symbol (&optional internal)
  "Reads qualified symbol name and tries to add an import for it.
If INTERNAL is non-nil or a prefix arg is supplied, include internal symbols."
  (interactive "P")
  (when-let* ((qualified-name (slime-read-symbol-name-global "Import symbol: " t internal))
              (symbol-name (slime-cl-symbol-name qualified-name))
              (package (slime-cl-symbol-package qualified-name)))
    (slime-package--add-import symbol-name package)
    ;; unqualify symbol-at-point if it has been imported
    (let ((symbol-at-point (slime-symbol-at-point)))
      (when (and (not (string-suffix-p ":" symbol-at-point)) ; foo: is a valid slime-symbol(!?)
                 ;; Call eq from swank to resolve aliases
                 (slime-eval `(cl:eq (cl:intern ,qualified-name) (cl:intern ,symbol-at-point))))
        (slime-unqualify-symbol-at-point)))))

(defun slime-unqualify-symbol-at-point ()
  "Remove package name from symbol at point."
  (interactive)
  (when-let* ((bounds (bounds-of-thing-at-point 'slime-symbol))
              (beg (car bounds))
              (end (cdr bounds))
              (qualified-name (buffer-substring-no-properties beg end))
              (symbol-name (slime-cl-symbol-name qualified-name)))
    (unless (string= qualified-name symbol-name)
      (delete-region beg end)
      (insert symbol-name))))

;; TODO: slime-toggle-qualified-name

(provide 'slime-import-symbol)
