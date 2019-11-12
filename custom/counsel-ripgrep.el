(require 'ivy)


(defun counsel-rg--add-prompt-dir (&rest args)
  (setq ivy--prompt (counsel-prompt-function-dir)))

(advice-add 'counsel-ag-function :before #'counsel-rg--add-prompt-dir)


(defun counsel-rg-change-dir (&optional new-dir)
  "Change search directory to new-dir. If interactive, prompt for a directory."
  (interactive)
  (let ((state ivy-last))
    (symbol-macrolet ((dir (ivy-state-directory state)))
      (when-let ((new-dir (or new-dir (read-directory-name "Change directory: " dir))))
        (setf dir new-dir)
        (ivy--reset-state state)))))

(defun counsel-rg-up-dir (n)
  "Change search directory to Nth parent of ther current directory."
  (interactive "p")
  ;; there must be a better way
  (let ((new-dir (loop for i from n downto 0
                       for dir = default-directory
                       then (file-name-directory (directory-file-name dir))
                       finally (return dir))))
    (counsel-rg-change-dir new-dir)))

(define-key counsel-ag-map (kbd "C-c C-d") 'counsel-rg-change-dir)
(define-key counsel-ag-map (kbd "C-c DEL") 'counsel-rg-up-dir)

(provide 'counsel-rg)
