;; -*- lexical-binding: t -*-

;;* completion-at-point
(defvar *indium-pending-completions* (make-hash-table :test #'equal)
  "Expression -> completion result or :wait.")

;; FIXME this is a pretty dumb way to get completion results synchronously.
(defun indium-completion-at-point ()
  "Query indium client for completion.
Adapted from `indium-repl-get-completions' with ugly hacks."
  (let* ((beg (let ((bol (point-at-bol))
                    (prev-delimiter
                      (1+ (save-excursion
                           (re-search-backward "[([:space:]]" nil t)))))
                (if prev-delimiter
                    (max bol prev-delimiter)
                  bol)))
         (end (point))
         (input (buffer-substring-no-properties beg end))
         (expression (if (string-match-p "\\." input)
                         (replace-regexp-in-string "\\.[^\\.]*$" "" input)
                       "this")))
    (list
     beg end
     (completion-table-dynamic
      (lambda (_)
        (let ((res
                (while-no-input
                 (unless (gethash expression *indium-pending-completions*)
                   (setf (gethash expression *indium-pending-completions*) :wait)
                   (indium-client-get-completion
                    expression
                    indium-debugger-current-frame
                    (lambda (candidates)
                      (setf (gethash expression *indium-pending-completions*)
                            (cl-loop for cand across candidates
                                     collect (if (equal "this" expression)
                                                 cand
                                               (concat expression "." cand)))))))
                 (indium-wait-for-completion expression))))
          (unless (eq t res)
            res))))
     :exclusive 'no)))

(cl-defun indium-wait-for-completion (expression)
  (loop for result = (gethash expression *indium-pending-completions*)
        do (cond ((eq :wait result)
                  (sit-for 0 10))       ; ewww!
                 (t
                  (remhash expression *indium-pending-completions*)
                  (return-from indium-wait-for-completion result)))))

(defun indium-enable-completion ()
  (setq-local completion-at-point-functions
              (cons 'indium-completion-at-point completion-at-point-functions)))
(add-hook 'indium-interaction-mode-hook #'indium-enable-completion)
(add-hook 'indium-repl-mode-hook #'indium-enable-completion)

(define-key indium-repl-mode-map (kbd "TAB") 'completion-at-point)

;;* counsel-indium-repl-history
(define-key indium-inspector-mode-map (kbd "C-w") 'indium-inspector-pop)

(defun counsel-indium-repl-history ()
  (interactive)
  (counsel--browse-history indium-repl-history
                           :caller 'counsel-indium-repl-history))
(define-key indium-repl-mode-map (kbd "M-r") 'counsel-indium-repl-history)

;;* initialization
(defun maybe-indium-interaction-mode ()
  (when (indium-client-process-live-p)
    (indium-interaction-mode 1)))
(add-hook 'js-mode-hook #'maybe-indium-interaction-mode)

(defun indium-init ()
  (company-mode -1))
(add-hook 'indium-repl-mode-hook #'indium-init)
(add-hook 'indium-interaction-mode-hook #'indium-init)

;;* evaluate css
(cl-defun indium-inject-css (css &optional (id "indium-stylesheet") callback)
  (indium-eval (format "{
let sheet = document.getElementById('%s');
if (sheet == null) {
  sheet = document.createElement('style');
  sheet.setAttribute('id', '%s');
  document.body.append(sheet);
}
sheet.innerHTML = `%s`;
}"
                       id id css)
               callback))

(defun indium-eval-css-buffer (&optional buff)
  "Append current stylesheet file to the document body."
  (interactive (list (current-buffer)))
  (unless (indium-client-process-live-p)
    (user-error "Indium not connected."))
  (indium-inject-css (with-current-buffer buff (buffer-string))
                     (or (file-name-base (buffer-file-name buff))
                         (buffer-name buff)
                         "indium-stylesheet")
                     (lambda (&rest args) (message "CSS applied."))))
(define-key css-mode-map (kbd "C-c C-k") 'indium-eval-css-buffer)


(provide 'configure-indium)
