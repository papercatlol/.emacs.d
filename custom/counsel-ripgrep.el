;; -*- lexical-binding: t -*-
(require 'ivy)
(require 'rg)


;;* vars
(setq rg-command-line-flags '("--max-columns=160")) ; rg.el
(setq rg-executable "rg") ; don't use `executable-find' because it fails on remote
(setq rg-command "rg --no-heading --line-number --color never -M 200")
(setq counsel-rg-base-command (concat rg-command " --sort path %s"))
(setq counsel-grep-base-command (concat rg-command " --no-filename %s %s"))
(setf (alist-get 'counsel-grep ivy-more-chars-alist) 0)

;;* *rg* results buffer style
(setq rg-show-columns nil)
(setq rg-align-line-column-separator ":")
(setq rg-align-position-content-separator ": ")
;; These two actually mean "min length"
(setq rg-align-line-number-field-length 1)
(setq rg-align-column-number-field-length 1)

;; Aggressively simplified `rg-perform-position-numbers-alignment'.
(defun rg-perform-position-numbers-alignment--override
    (line-number &optional column-number context-marker)
  "Return aligned LINE-NUMBER, COLUMN-NUMBER and CONTEXT-MARKER."
  (cl-assert (if column-number (not context-marker) context-marker))
  (concat line-number
          (if (and column-number rg-show-columns)
              (concat rg-align-line-column-separator column-number)
            context-marker)
          rg-align-position-content-separator))

(advice-add 'rg-perform-position-numbers-alignment :override
            #'rg-perform-position-numbers-alignment--override)

;;(setf
;; (alist-get (rx "*rg" (* any))
;;            display-buffer-alist nil nil #'equal)
;; '((display-buffer-reuse-window display-buffer-in-direction)
;;   (direction . left)
;;   (window-width . 70)
;;   (reusable-frames . nil)))

(defvar rg-truncate-lines t
  "*rg* buffer setting for `truncate-lines'.")

(defun rg--set-truncate-lines ()
  (setq truncate-lines rg-truncate-lines))

(add-hook 'rg-mode-hook #'rg--set-truncate-lines)


;;* custom type-aliases
(add-to-list 'rg-custom-type-aliases '("lisp" . "*.cl"))
(add-to-list 'rg-custom-type-aliases '("cl" . "*.cl *.lisp *.lsp"))

;;* rg-mode
(with-eval-after-load 'evil
  (evil-set-initial-state 'rg-mode 'normal))

(define-key rg-mode-map (kbd "<tab>") 'compilation-next-error)
(define-key rg-mode-map (kbd "k") 'compilation-previous-error)
(define-key rg-mode-map (kbd "j") 'compilation-next-error)
(define-key rg-mode-map (kbd "h") 'backward-char)
(define-key rg-mode-map (kbd "l") 'forward-char)
(define-key rg-mode-map (kbd "m") 'compilation-display-error)
(define-key rg-mode-map (kbd "M-m") 'compilation-display-error)
(define-key rg-mode-map (kbd "o") 'compilation-display-error)
(define-key rg-mode-map (kbd "C-j") 'compilation-display-next-error)
(define-key rg-mode-map (kbd "C-k") 'compilation-display-previous-error)
(define-key rg-mode-map (kbd "C-x C-/") 'rg-menu)
(define-key rg-mode-map (kbd "M") 'rg-menu)
(define-key rg-mode-map (kbd "S") 'rg-save-search)
(define-key rg-mode-map (kbd "[") 'rg-prev-file)
(define-key rg-mode-map (kbd "]") 'rg-next-file)
(define-key rg-mode-map (kbd "L") 'rg-rerun-change-literal)
(define-key rg-mode-map (kbd "C-c f") 'rg-rerun-change-files)
(define-key rg-mode-map (kbd "C-{") 'rg-back-history)
(define-key rg-mode-map (kbd "C-}") 'rg-forward-history)
(define-key rg-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)

(global-set-key (kbd "M-g /") 'rg)
(global-set-key (kbd "M-g M-/") 'rg-menu)

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
  (let ((new-dir (cl-loop for i from n downto 0
                          for dir = (ivy-state-directory ivy-last)
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
  (define-key map (kbd "C-c C-w") 'counsel-rg-up-dir)
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
