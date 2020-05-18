;; -*- lexical-binding: t -*-

;;* magit
(require 'magit)

;; TODO: configure magit-blame with evil-mode

(put 'magit-clean 'disabled nil)

(setq magit-log-arguments '("-n64" "--graph" "--decorate" "--patch")
      magit-diff-buffer-file-locked t
      magit-diff-refine-hunk nil
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

;;** show unstaged diff for current buffer, diff helper functions
(defun magit-diff-buffer-file-unstaged ()
  "Like `magit-diff-buffer-file', but show unstaged diff only."
  (interactive)
  (if-let ((file (magit-file-relative-name)))
      (if magit-buffer-refname
          (call-interactively #'magit-diff-buffer-file)
        (save-buffer)
        (let ((line (line-number-at-pos))
              (col (current-column))
              (args (car (magit-diff-arguments))))
          (with-current-buffer
              (magit-diff-setup-buffer nil nil args (list file) magit-diff-buffer-file-locked)
            (magit-diff--goto-position file line col))))
    (user-error "Buffer isn't visiting a file")))

(defun magit-diff-buffer-file-other-window ()
  "Like `magit-diff-buffer-file', but don't focus new window."
  (interactive)
  (let ((win (selected-window)))
    (call-interactively #'magit-diff-buffer-file)
    (select-window win)))

(defun magit-diff-buffer-file-unstaged-other-window ()
  "Like `magit-diff-buffer-file-unstaged', but don't focus new window."
  (interactive)
  (let ((win (selected-window)))
    (call-interactively #'magit-diff-buffer-file-unstaged)
    (select-window win)))

(defun magit-diff-unstaged-other-window ()
  "Like `magit-diff-unstaged', but don't focus new window."
  (interactive)
  (let ((win (selected-window)))
    (call-interactively #'magit-diff-unstaged)
    (select-window win)))

;;** stage current buffer
(defun magit-stage-buffer-file ()
  (interactive)
  (magit-stage-file (buffer-file-name)))
(pushnew 'magit-stage-buffer-file magit-post-stage-hook-commands)

;;** diff-buffer-dwim
(defun diff-buffer-dwim (force-magit)
  (interactive "P")
  "If buffer is modified and no prefix arg is supplied, call `diff-buffer-with-file'.
Else call `magit-diff-buffer-file'."
  (if (and (not force-magit) (buffer-modified-p))
      (diff-buffer-with-file (current-buffer))
    (call-interactively #'magit-diff-buffer-file)))

(defun diff-buffer-modified ()
  (interactive)
  (if (buffer-modified-p)
      (diff-buffer-with-file (current-buffer))
    (message "Buffer is not modified.")))

;;** keybindings
(define-key magit-file-mode-map (kbd "C-x =") 'diff-buffer-dwim)
(define-key text-mode-map (kbd "C-x =") 'diff-buffer-modified)
(define-key prog-mode-map (kbd "C-x =") 'diff-buffer-modified)
(define-key magit-file-mode-map [left-fringe mouse-1] 'diff-buffer-dwim)

;; MAYBE: add hydra
(define-key magit-file-mode-map (kbd "C-x G") 'magit-file-dispatch)
(define-key magit-file-mode-map (kbd "C-c M-g") nil)

(define-key magit-mode-map (kbd "C-c C-l") 'magit-toggle-buffer-lock)
(define-key magit-mode-map (kbd "j") 'magit-forward-dwim)
(define-key magit-mode-map (kbd "k") 'magit-backward-dwim)
(define-key magit-mode-map (kbd "C-x g") 'magit-status)

(dolist (m (list magit-status-mode-map magit-diff-mode-map))
  (define-key m (kbd "j") 'magit-forward-dwim)
  (define-key m (kbd "k") 'magit-backward-dwim)
  (define-key m (kbd "C-k") 'magit-discard)
  (define-key m (kbd "=") 'magit-diff-more-context)
  (define-key m (kbd "M-m") 'magit-diff-visit-file-other-window))

;;** magit-todos
(require 'magit-todos)

(magit-todos-mode t)

(setq magit-todos-auto-group-items 1000)

(define-key magit-todos-section-map (kbd "j") 'magit-forward-dwim)
(define-key magit-todos-section-map (kbd "k") 'magit-backward-dwim)


;;* git-gutter
(require 'git-gutter-fringe)

(global-git-gutter-mode t)
(setq git-gutter:lighter ""
      git-gutter:ask-p nil)

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
  "........"
  "........"
  "..XX...."
  "..XX...."
  "........"
  "........"
  "........")

;;** update on magit stage\unstage
;; This may not update all buffers, in which case TODO: update buffers for staged files
(add-hook 'magit-post-stage-hook #'git-gutter:update-all-windows)
(add-hook 'magit-post-unstage-hook #'git-gutter:update-all-windows)

;;** first/last-hunk
(defun git-gutter:first-hunk ()
  (interactive)
  (goto-char (point-min))
  (git-gutter:next-hunk 1))

(defun git-gutter:last-hunk ()
  (interactive)
  (goto-char (point-max))
  (git-gutter:previous-hunk 1))

;;** update magit diff buffer when jumping between hunks
(defun magit-refresh-diff-window-position (&rest _)
  (when-let ((file (magit-file-relative-name))
             (line (line-number-at-pos))
             (col (current-column)))
    (dolist (win (window-list))
      (with-current-buffer (window-buffer win)
        (when (eq major-mode 'magit-diff-mode)
          (magit-diff--goto-position file line col))))))

(advice-add 'git-gutter:next-hunk :after #'magit-refresh-diff-window-position)
(advice-add 'git-gutter:previous-hunk :after #'magit-refresh-diff-window-position)

;;** git-gutter:set-start-revision-magit
;; Get list of refnames from magit, use completing-read
(defun git-gutter:set-start-revision-magit (start-rev)
  "Set start revision. If `start-rev' is nil or empty string then reset
start revision."
  (interactive
   (list (completing-read "Start Revision: " (magit-list-refnames)
                          nil nil nil 'magit-revision-history (magit-get-current-branch))))
  (when (and start-rev (not (string= start-rev "")))
    (unless (git-gutter:revision-valid-p start-rev)
      (error "Revision '%s' is not valid." start-rev)))
  (setq git-gutter:start-revision start-rev)
  (message "Revision set to %s" start-rev)
  (git-gutter))

;;* git hydra
;; TODO: a function to stage current hunk or region
(defhydra hydra-git (:hint nil)
  "
 ^Stage^                  ^Diff^                   ^Other^
 ^^^^^^---------------------------------------------------------------------------------
 _j_: next hunk           _=_: diff(file)          _g_: magit-status
 _k_: prev hunk           _u_: diff unstaged(file) _l_: git log for current file
 _s_: stage hunk          _U_: diff unstaged(all)  _L_: magit-log popup
 _S_: stage current file  _d_: magit-diff popup    _c_: magit-commit popup
 _G_: refresh git-gutter  _D_: vc-ediff            _r_: vc-revert
 _<_: first hunk                                 ^^_p_: magit-push popup
 _>_: last hunk                                  ^^_b_: blame current file
 _R_: set start revision                         ^^_B_: magit-blame popup
                                               ^^^^_f_: magit find file
"
  ("q" nil)
  ("<escape>" nil)
  ("j" #'git-gutter:next-hunk)
  ("k" #'git-gutter:previous-hunk)
  ("s" #'git-gutter:stage-hunk)
  ("S" #'magit-stage-buffer-file)
  ("G" #'git-gutter:update-all-windows)
  ("<" #'git-gutter:first-hunk)
  (">" #'git-gutter:last-hunk)
  ("R" #'git-gutter:set-start-revision-magit)

  ("=" #'magit-diff-buffer-file-other-window)
  ("u" #'magit-diff-buffer-file-unstaged-other-window)
  ("U" #'magit-diff-unstaged-other-window)
  ("d" #'magit-diff :exit t)
  ("D" #'vc-ediff :exit t)

  ("g" #'magit-status :exit t)
  ("l" #'magit-log-buffer-file :exit t)
  ("L" #'magit-log :exit t)
  ("c" #'magit-commit :exit t)
  ("p" #'magit-push :exit t)
  ("r" #'vc-revert :exit t)
  ("f" #'magit-find-file-other-window :exit t)
  ("b" #'magit-blame-addition :exit t)
  ("B" #'magit-blame :exit t))

(define-key magit-file-mode-map (kbd "C-x g") 'hydra-git/body)
(define-key magit-file-mode-map (kbd "C-x C-g") 'ignore)

;;* dired
(require 'dired-git-info)

(define-key dired-mode-map (kbd ")") 'dired-git-info-mode)
(define-key dired-mode-map (kbd "C-x g") 'magit-status)

;;* TODO: ibuffer

(provide 'configure-git)
