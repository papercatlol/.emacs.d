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


;;* counsel-rg-dir
(defun counsel-rg-dir (&optional initial-input initial-directory extra-rg-args rg-prompt)
  "Same as `counsel-rg' but search starting from current directory instead of the repo root."
  (interactive)
  (counsel-rg (or initial-input (and (symbol-at-point)
                                     (symbol-name (symbol-at-point))))
              (if current-prefix-arg
                  default-directory
                (read-directory-name "rg in directory: "))
              (or extra-rg-args "")
              rg-prompt))

(defun counsel-rg-dwim ()
  "Grep symbol-at-point in current dir. With prefix arg search
from git root."
  (interactive)
  (counsel-rg (thing-at-point 'symbol)
              (if current-prefix-arg
                  (counsel--git-root)
                default-directory)))

(global-set-key (kbd "C-x /") 'counsel-rg-dir)
(global-set-key (kbd "C-x C-/") 'counsel-rg-dwim)

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

(defun counsel-rg-toggle-root-dir ()
  "Set search dir to `default-directory'. If in
`default-directory' already, search from git root."
  (interactive)
  (let ((search-dir (ivy-state-directory ivy-last)))
    (counsel-rg-change-dir
     (if (string= default-directory search-dir)
         (counsel--git-root)
       default-directory))))

(dolist (map (list counsel-ag-map counsel-find-file-map))
  (define-key map (kbd "C-c DEL") 'counsel-rg-up-dir)
  (define-key map (kbd "C-c C-d") 'counsel-rg-change-dir)
  (define-key map (kbd "C-c C-p") 'counsel-rg-toggle-root-dir))

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
