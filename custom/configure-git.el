;; -*- lexical-binding: t -*-

;;* magit
(require 'magit)

(put 'magit-clean 'disabled nil)

(setq magit-log-arguments '("-n64" "--graph" "--decorate" "--patch")
      magit-diff-buffer-file-locked t
      magit-todos-auto-group-items 1000
      magit-section-visibility-indicator (quote (magit-fringe-bitmap+ . magit-fringe-bitmap-)))

;;** widen fringe for magit windows
(defun magit-status-set-wide-fringe (&optional arg)
  (display-line-numbers-mode -1)
  (set-window-fringes nil 11 5))

(add-hook 'magit-status-sections-hook #'magit-status-set-wide-fringe)
(add-hook 'magit-refs-mode-hook #'magit-status-set-wide-fringe)
(add-hook 'magit-revision-sections-hook #'magit-status-set-wide-fringe)

;;** j/k movement
(defun magit-forward-dwim ()
  (interactive)
  (if (region-active-p)
      (magit-next-line)
    (magit-section-forward)))

(defun magit-backward-dwim ()
  (interactive)
  (if (region-active-p)
      (magit-previous-line)
    (magit-section-backward)))

;;** show unstaged diff for current buffer
(defun magit-diff-buffer-file-unstaged ()
  "Like `magit-diff-buffer-file', but show unstaged diff only."
  (interactive)
  (if-let ((file (magit-file-relative-name)))
      (if magit-buffer-refname
          (magit-show-commit magit-buffer-refname
                             (car (magit-show-commit--arguments))
                             (list file))
        (save-buffer)
        (let ((line (line-number-at-pos))
              (col (current-column))
              (args (car (magit-diff-arguments))))
          (magit-diff-setup-buffer nil nil args (list file) magit-diff-buffer-file-locked)))
    (user-error "Buffer isn't visiting a file")))

;;** stage current buffer
(defun magit-stage-buffer-file ()
  (interactive)
  (magit-stage-file (buffer-file-name)))
(pushnew 'magit-stage-buffer-file magit-post-stage-hook-commands)

;;** keybindings
(define-key magit-file-mode-map (kbd "C-x =") 'magit-diff-buffer-file)
(define-key magit-file-mode-map (kbd "C-x G") 'magit-file-dispatch)
(define-key magit-mode-map (kbd "C-c C-l") 'magit-toggle-buffer-lock)
(define-key magit-mode-map (kbd "j") 'magit-forward-dwim)
(define-key magit-mode-map (kbd "k") 'magit-backward-dwim)

(dolist (m (list magit-status-mode-map magit-diff-mode-map))
  (define-key m (kbd "j") 'magit-forward-dwim)
  (define-key m (kbd "k") 'magit-backward-dwim)
  (define-key m (kbd "C-k") 'magit-discard))

;; TODO: merge with hydra-git-gutter
(define-prefix-command 'magit-file-prefix-map)
(define-key magit-file-mode-map (kbd "C-x g") 'magit-file-prefix-map)
(define-key magit-file-prefix-map "g" 'magit-status)
(define-key magit-file-prefix-map "l" 'magit-log-buffer-file)
(define-key magit-file-prefix-map "f" 'magit-find-file)
(define-key magit-file-prefix-map "b" 'magit-blame)
(define-key magit-file-prefix-map "s" 'magit-stage-buffer-file)
(define-key magit-file-prefix-map "d" 'magit-diff)
(define-key magit-file-prefix-map "D" 'vc-ediff)

;;** magit-todos
(require 'magit-todos)

(magit-todos-mode t)

(setq magit-todos-auto-group-items 1000)

(define-key magit-todos-section-map (kbd "j") 'magit-forward-dwim)
(define-key magit-todos-section-map (kbd "k") 'magit-backward-dwim)


;;* git-gutter
(require 'git-gutter-fringe)

(global-git-gutter-mode t)
(setq git-gutter:lighter "")

;;** bitmaps that look prettier with half-fringe
(fringe-helper-define 'git-gutter-fr:added nil
  "........"
  "........"
  "........"
  "..XX...."
  "..XX...."
  "........"
  "........"
  "........")

(fringe-helper-define 'git-gutter-fr:deleted nil
  "........"
  "........"
  "........"
  "..XX...."
  "..XX...."
  "........"
  "........"
  "........")

(fringe-helper-define 'git-gutter-fr:modified nil
  "........"
  "XXXX...."
  "XXXX...."
  "........"
  "........"
  "XXXX...."
  "XXXX...."
  "........")

;;** update on magit stage\unstage
;; This may not update all buffers, in which case TODO: update buffers for staged files
(add-hook 'magit-post-stage-hook #'git-gutter:update-all-windows)
(add-hook 'magit-post-unstage-hook #'git-gutter:update-all-windows)

;;** hydra
(defhydra hydra-git-gutter ()
  ""
  ("j" #'git-gutter:next-hunk "next-hunk")
  ("n" #'git-gutter:next-hunk "next-hunk")
  ("k" #'git-gutter:previous-hunk "previous-hunk")
  ("p" #'git-gutter:previous-hunk "previous-hunk")
  ("s" #'git-gutter:stage-hunk "stage-hunk")
  ("g" #'git-gutter:update-all-windows "update git-gutter")
  ("=" #'magit-diff-buffer-file "diff file"))

;;* dired
(require 'dired-git-info)

(define-key dired-mode-map (kbd ")") 'dired-git-info-mode)
(define-key dired-mode-map (kbd "C-x g") 'magit-file-prefix-map)


(provide 'configure-git)
