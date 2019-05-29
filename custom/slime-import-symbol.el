(require 'slime-package-fu)
;; slime-package-fu: `slime-goto-package-source-definition', `slime-format-symbol-for-defpackage'

(defun slime-package--add-import (symbol package)
    "Tries to insert an import for SYMBOL from PACKAGE.
TODO: Check if SYMBOL is already imported.
TODO: Support :shadowing-import-from.
For new imports:
 Uses (:import-from :package
                   #:symbol)
 style.
 Prompts for nickname to use if there are any."
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
                       (setq package-import sexp-start)
                       (return)))
                   (when (and (null first-export) (eq :export (car sexp)))
                     (setq first-export sexp-start)))
              until (= (point) definition-end))
        (cond (package-import ; there is an (:import-from PACKAGE ..) form already
               (goto-char package-import)
               (forward-list 1)
               (backward-char 1)
               (when (looking-back (rx symbol-end))
                 (newline-and-indent))
               (insert (slime-format-symbol-for-defpackage symbol)))
              ;; Insert a new (:import-from PACKAGE ..) before first-export
              ;; or at the end of package declaration.
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
                   (insert (format "(:import-from :%s" (downcase import-name)))
                   (newline-and-indent)
                   (insert (format "#:%s)" (downcase symbol)))))))
        (call-interactively #'slime-compile-defun))))

(defun slime--format-package-name (package-name)
  (downcase (format ":%s" package-name)))

(defun slime-import-symbol ()
  "Reads qualified symbol name and tries to add an import for it."
  (interactive)
  (when-let ((symbol (slime-read-symbol-name "Import symbol: " t)))
    (slime-package--add-import (slime-cl-symbol-name symbol)
                               (slime-cl-symbol-package symbol))))

(provide 'slime-import-symbol)
