(require 'ivy)


;;* vars
(setq rg-command "rg --no-heading --line-number --color never -M 160")
(setq counsel-rg-base-command (concat rg-command " --sort path %s"))
(setq counsel-grep-base-command (concat rg-command " --no-filename %s %s"))
(setf (alist-get 'counsel-grep ivy-more-chars-alist) 0)

;;* show current dir in prompt
(defun counsel-rg--add-prompt-dir (&rest args)
  (setq ivy--prompt (counsel-prompt-function-dir)))

(setf (alist-get 'counsel-rg ivy-hooks-alist) #'counsel-rg--add-prompt-dir)

;;* changing current dir on the fly
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


;;* use `rg.el' instead of ivy-occur
;; TODO
(defun counsel-rg-occur ()
  ""
  (interactive)
  (let* ((text ivy-text)
         (command-args (counsel--split-command-args text))
         (regex (counsel--grep-regex (cdr command-args)))
         (switches (concat (car command-args)
                           (counsel--ag-extra-switches regex)
                           (if (ivy--case-fold-p text)
                               " -i "
                             " -s ")))
         (query (counsel--format-ag-command switches
                                            regex))
         (dir (ivy-state-directory ivy-last)))
    (ivy-exit-with-action
     (lambda (_)
       (rg-run regex "everything" dir nil nil
               (funcall rg-command-line-flags-function (split-string switches)))))))

(define-key counsel-ag-map [remap ivy-occur] 'counsel-rg-occur)


(provide 'counsel-ripgrep)
