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
(define-key magit-mode-map (kbd "C-x g") 'magit-status)

(dolist (m (list magit-status-mode-map magit-diff-mode-map))
  (define-key m (kbd "j") 'magit-forward-dwim)
  (define-key m (kbd "k") 'magit-backward-dwim)
  (define-key m (kbd "C-k") 'magit-discard))

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

;;* git hydra
(defhydra hydra-git (:hint nil)
  "
 ^Stage^                  ^Diff^                   ^Other^
 ^^^^^^---------------------------------------------------------------------------------
 _j_: next hunk           _=_: diff (file)         _g_: magit-status
 _k_: prev hunk           _u_: diff unstaged(file) _l_: git log for current file
 _s_: stage hunk          _U_: diff unstaged(all)  _b_: blame current file
 _S_: stage current file  _d_: magit-diff popup    _B_: magit-blame popup^
 _G_: update git-gutter   _D_: vc-ediff            _f_: magit-find-file "
  ("q" nil)
  ("<escape>" nil)
  ("j" #'git-gutter:next-hunk)
  ("k" #'git-gutter:previous-hunk)
  ("s" #'git-gutter:stage-hunk)
  ("G" #'git-gutter:update-all-windows)

  ("=" #'magit-diff-buffer-file :exit t)
  ("u" #'magit-diff-buffer-file-unstaged :exit t)
  ("U" #'magit-diff-unstaged :exit t)
  ("g" #'magit-status :exit t)
  ("l" #'magit-log-buffer-file :exit t)
  ("f" #'magit-find-file-other-window :exit t)
  ("b" #'magit-blame-addition :exit t)
  ("B" #'magit-blame :exit t)
  ("S" #'magit-stage-buffer-file :exit t)
  ("d" #'magit-diff :exit t)
  ("D" #'vc-ediff :exit t))

(define-key magit-file-mode-map (kbd "C-x g") 'hydra-git/body)

;;* dired
(require 'dired-git-info)

(define-key dired-mode-map (kbd ")") 'dired-git-info-mode)
(define-key dired-mode-map (kbd "C-x g") 'magit-file-prefix-map)


(provide 'configure-git)
