;; -*- lexical-binding: t -*-
(require 'ivy)
(require 'rg)

;;* vars
(setq rg-command-line-flags '("--max-columns=160")) ; rg.el
(setq rg-executable "rg") ; don't use `executable-find' because it fails on remote
(setq rg-command " rg --no-heading --line-number --color never -M 160")
(setq counsel-rg-base-command (concat rg-command " --sort path %s"))
(setq counsel-grep-base-command (concat rg-command " --no-filename %s %s"))
(setf (alist-get 'counsel-grep ivy-more-chars-alist) 0)

;;* rg mode keybindings
(define-key rg-mode-map (kbd "C-x C-/") 'rg-menu)

;;* show current dir in prompt
(ivy-set-prompt #'counsel-rg #'counsel-prompt-function-dir)
(ivy-set-prompt #'counsel-dirs #'counsel-prompt-function-dir)
(ivy-set-prompt #'counsel-files #'counsel-prompt-function-dir)

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
(define-key counsel-find-file-map (kbd "C-c C-d") 'counsel-rg-change-dir)
(define-key counsel-find-file-map (kbd "C-c DEL") 'counsel-rg-up-dir)

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
