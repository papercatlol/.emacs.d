;; -*- lexical-binding: t -*-

;;* magit
(require 'magit)


(put 'magit-clean 'disabled nil)

(setq magit-log-arguments '("-n64" "--decorate" "--patch")
      magit-define-global-key-bindings nil
      magit-diff-buffer-file-locked t
      magit-diff-refine-hunk nil
      magit-todos-auto-group-items 1000
      ;; if T, magit-status will jump to current file position in status buffer
      magit-status-goto-file-position nil
      magit-section-visibility-indicator (quote (magit-fringe-bitmap+ . magit-fringe-bitmap-))
      magit-section-initial-visibility-alist '((untracked . hide)
                                               (stashes . hide)))

;;* transient
(setq transient-default-level 5)        ; up to 7, default is 4

;;* widen fringe for magit windows
(defun magit-status-set-wide-fringe (&optional arg)
  (display-line-numbers-mode -1)
  (set-window-fringes nil 11 5))

(add-hook 'magit-status-sections-hook #'magit-status-set-wide-fringe)
(add-hook 'magit-refs-mode-hook #'magit-status-set-wide-fringe)
(add-hook 'magit-revision-sections-hook #'magit-status-set-wide-fringe)

;;* j/k movement
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

;;* diff
;;** show unstaged diff for current buffer
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

;;** diff helper functions
(defmacro def-no-select (fn &optional new-fn)
  "Define a new function named NEW-FN that behaves exactly like
FN, but restores selected window afterwards. If NEW-FN is
omitted, the functions name will be FN-no-select."
  (let ((new-fn
         (cond ((null new-fn)
                (intern (concat (symbol-name fn) "-no-select")))
               ((symbolp new-fn)
                new-fn)))
        (documentation
         (format "Like `%s', but restore selected window afterwards." fn)))
    (unless (and fn (fboundp fn) new-fn)
      (error "bad args: %s %s" fn new-fn))
    `(defun ,new-fn (&rest args)
       ,documentation
       (interactive)
       (save-selected-window
         (if (interactive-p)
             (call-interactively #',fn)
           (apply #',fn args))))))

(def-no-select magit-diff-buffer-file)
(def-no-select magit-diff-buffer-file-unstaged)
(def-no-select magit-diff-unstaged)
(def-no-select magit-diff-visit-worktree-file-other-window)

;;** diff-buffer-dwim
(defun diff-buffer-dwim (force-magit)
  (interactive "P")
  "If buffer is modified and no prefix arg is supplied, call `diff-buffer-with-file'.
Else call `magit-diff-buffer-file'."
  (if (and (not force-magit) (buffer-modified-p))
      (if (buffer-file-name)
          (diff-buffer-with-file (current-buffer))
        (error "Buffer isn't visiting a file"))
    (call-interactively #'magit-diff-buffer-file)))

(defun diff-buffer-modified ()
  (interactive)
  (if (buffer-modified-p)
      (diff-buffer-with-file (current-buffer))
    (message "Buffer is not modified.")))

(global-set-key (kbd "C-x =") 'diff-buffer-dwim)
(global-set-key [left-fringe mouse-1] 'diff-buffer-dwim)

;;** magit-diff-narrow-to-file
(defun magit-diff-narrow-to-file ()
  "Restrict (narrow) current diff to `magit-file-at-point'. If
already narrowed, widen."
  (interactive)
  (setq magit-buffer-diff-files
        (unless magit-buffer-diff-files
          (list (or (buffer-file-name) (magit-file-at-point)))))
  (magit-refresh)
  (message (if-let (file (car magit-buffer-diff-files))
               (format "Narrow to %s" file)
             "Widen")))

;;* stage current buffer
(defun magit-stage-buffer-file ()
  (interactive)
  (magit-stage-file (buffer-file-name)))
(cl-pushnew 'magit-stage-buffer-file magit-post-stage-hook-commands)

;;* ediff
(setq ediff-split-window-function 'split-window-horizontally)
(setq-default ediff-ignore-similar-regions t)

(defun ediff-scroll-down ()
  (interactive)
  (ediff-scroll-vertically (- (if current-prefix-arg
                               (prefix-numeric-value current-prefix-arg)
                             4))))

(defun ediff-scroll-up ()
  (interactive)
  (ediff-scroll-vertically (if current-prefix-arg
                               (prefix-numeric-value current-prefix-arg)
                             4)))

(defun configure-ediff-keybindings ()
  (define-key ediff-mode-map "j" 'ediff-next-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference)
  (define-key ediff-mode-map (kbd "M-j") 'ediff-scroll-down)
  (define-key ediff-mode-map (kbd "M-k") 'ediff-scroll-up)
  (define-key ediff-mode-map (kbd "C-v") nil)
  (define-key ediff-mode-map "s" 'ediff-toggle-skip-similar)
  )

(add-hook 'ediff-keymap-setup-hook 'configure-ediff-keybindings)

;;** ediff-region-and-kill-ring
(defun ediff-region-and-kill-ring (&optional beg end)
  "If region is active: ediff current region and last string in
kill ring. Otherwise ediff last and second-to last strings. With
prefix arg choose string(s) from kill ring interactively."
  (interactive
   (when (region-active-p)
       (list (region-beginning) (region-end))))
  (unless (require 'ediff nil t)
    (error "Can't load ediff.el."))
  (let ((string-A nil)
        (string-B nil))
    (cond ((and beg end)
           (setq string-A (buffer-substring-no-properties beg end))
           (setq string-B (if current-prefix-arg
                              (completing-read "String B: " kill-ring)
                            (nth 0 kill-ring))))
          (current-prefix-arg
           (setq string-A (completing-read "String A: " kill-ring))
           (setq string-B (completing-read "String B: " kill-ring)))
          (t (setq string-A (nth 0 kill-ring))
             (setq string-B (nth 1 kill-ring))))
    (-let (((buffer-A end-A) (ediff--prepare-buffer-from-string "*ediff A*" string-A))
           ((buffer-B end-B) (ediff--prepare-buffer-from-string "*ediff B*" string-B)))
      (add-hook 'ediff-after-quit-hook-internal
                (ediff--make-after-quit-fn buffer-A buffer-B))
      (ediff-regions-internal
       buffer-A 0 end-A
       buffer-B 0 end-B
       nil 'ediff-regions-linewise nil nil))))

(defun ediff--make-after-quit-fn (buffer-A buffer-B)
  "Make a function that will restore current window configuration
and kill tmp buffers on call and reset the
`ediff-after-quit-hook-internal' hook."
  (let ((window-configuration (current-window-configuration)))
    (lambda ()
      (setq ediff-after-quit-hook-internal nil)
      (set-window-configuration window-configuration)
      (kill-buffer buffer-A)
      (kill-buffer buffer-B))))

(defun ediff--prepare-buffer-from-string (buffer-name string)
  (when-let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert string))
    (list buf (point-max))))

;;** magit-ediff-show-*-buffer-file
(defun magit-ediff-show-unstaged-buffer-file ()
  "Show unstaged changes for current file using Ediff."
  (interactive)
  (magit-ediff-show-unstaged (buffer-file-name)))

(defun magit-ediff-show-staged-buffer-file ()
  "Show staged changes for current file using Ediff."
  (interactive)
  (magit-ediff-show-staged (buffer-file-name)))

(defun magit-ediff-show-working-tree-buffer-file ()
  "Show working-tree changes for current file using Ediff."
  (interactive)
  (magit-ediff-show-working-tree (buffer-file-name)))

;; TODO: ediff hydra
(global-set-key (kbd "H-e") 'ediff-region-and-kill-ring)


;; MAYBE: add hydra
(global-set-key (kbd "C-x G") 'magit-file-dispatch)

(define-key magit-mode-map (kbd "C-c C-l") 'magit-toggle-buffer-lock)
(define-key magit-mode-map (kbd "j") 'magit-forward-dwim)
(define-key magit-mode-map (kbd "k") 'magit-backward-dwim)
(define-key magit-log-mode-map (kbd "j") 'magit-next-line)
(define-key magit-log-mode-map (kbd "k") 'magit-previous-line)
(define-key magit-log-mode-map (kbd "m") 'magit-diff-show-or-scroll-down)

(define-key magit-mode-map (kbd "C-x g") 'magit-status)

(dolist (m (list magit-status-mode-map magit-diff-mode-map))
  (define-key m (kbd "j") 'magit-next-line)
  (define-key m (kbd "k") 'magit-previous-line)
  (define-key m (kbd "[") 'magit-section-backward-sibling)
  (define-key m (kbd "]") 'magit-section-forward-sibling)
  (define-key m (kbd "C-M-k") 'magit-section-backward-sibling)
  (define-key m (kbd "C-M-j") 'magit-section-forward-sibling)
  (define-key m (kbd "C-k") 'magit-discard)
  (define-key m (kbd "=") 'magit-diff-more-context)
  (define-key m (kbd "M-m") 'magit-diff-visit-worktree-file-other-window)
  (define-key m (kbd "M-RET") 'magit-diff-visit-worktree-file-other-window-no-select)
  (define-key m (kbd "C-x C-n") 'magit-diff-narrow-to-file)
  (define-key m "H" 'magit-section-up))

;;** header-line for files in other revisions
(defun magit--set-header-line (&rest args)
  (when magit-buffer-revision
    (setq header-line-format
          (concat "rev: " (magit-format-rev-summary magit-buffer-revision)))))
(advice-add 'magit-find-file--internal :after #'magit--set-header-line)
(advice-add 'magit-diff-visit-file--internal :after #'magit--set-header-line)

;;** magit-blame-dwim
(defun magit-blame-dwim ()
  (interactive)
  (if magit-blame-mode
      (magit-blame-mode -1)
    (call-interactively #'magit-blame-addition)))

;;** magit-list-repositories
(setq magit-repository-directories
      '(("~/.emacs.d/" . 2)
        ("~/clones/" . 2)
        ("~/aur/" . 1)))

;;** magit-delta
(require 'magit-delta)

(setq magit-delta-delta-args
      `("--max-line-distance" "0.6"
        "--24-bit-color" ,(if xterm-color--support-truecolor "always" "never")
        "--color-only"
        "--theme" "none"))

(add-hook 'magit-mode-hook #'magit-delta-mode)

;;** [DISABLED] magit-todos
;; (require 'magit-todos)

;; (magit-todos-mode t)

;; (setq magit-todos-auto-group-items 1000)

;; (define-key magit-todos-section-map (kbd "j") 'magit-forward-dwim)
;; (define-key magit-todos-section-map (kbd "k") 'magit-backward-dwim)


;;* git-gutter
(require 'git-gutter-fringe)

;; only enable git-gutter in local git-tracked files
(defun maybe-enable-git-gutter ()
  (unless (file-remote-p (buffer-file-name))
    (git-gutter-mode 1)))

(add-hook 'find-file-hook #'maybe-enable-git-gutter)

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
;; TODO: look at smerge hydra here:
;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#hydra-git
(defhydra hydra-git (:hint nil :color pink)
  "
 ^Stage^                  ^Diff^                    ^File^               ^Other^
 ^^^^^^-------------------------------------------------------------------------------------------------------
 _j_: next hunk           _==_: ediff unstaged      _f_: find file(blob) _g_: magit-status
 _k_: prev hunk           _u_: diff unstaged(file)  _p_: prev blob       _l_: git log for current file
 _s_: stage hunk          _U_: diff unstaged(all)   _n_: next blob       _L_: magit-log popup
 _S_: stage current file  _d_: magit-diff popup     _C-j_: blob file     _c_: magit-commit popup
 _C-k_: revert hunk       _D_: vc-ediff             _Fd_: delete file    _r_: vc-revert
 _G_: refresh git-gutter  _e_: magit-ediff popup    _Fu_: untrack file   _P_: magit-push popup
 _<_: first hunk          _SPC_: diff(file)         _Fr_: rename file    _b_: blame dwim
 _>_: last hunk           _=s_: ediff staged        _Fc_: checkout file  _B_: magit-blame popup
 _R_: set start revision  _=w_: ediff worktree                         ^^_$_: magit process buffer
"
  ("q" nil)
  ("<escape>" nil)
  ("j" #'git-gutter:next-hunk)
  ("k" #'git-gutter:previous-hunk)
  ("s" #'git-gutter:stage-hunk)
  ("S" #'magit-stage-buffer-file)
  ("C-k" #'git-gutter:revert-hunk)
  ("G" #'git-gutter:update-all-windows)
  ("<" #'git-gutter:first-hunk)
  (">" #'git-gutter:last-hunk)
  ("R" #'git-gutter:set-start-revision-magit)

  ("==" #'magit-ediff-show-unstaged-buffer-file :exit t)
  ("=s" #'magit-ediff-show-staged-buffer-file :exit t)
  ("=w" #'magit-ediff-show-working-tree-buffer-file :exit t)
  ("u" #'magit-diff-buffer-file-unstaged-no-select)
  ("U" #'magit-diff-unstaged-no-select)
  ("d" #'magit-diff :exit t)
  ("D" #'vc-ediff :exit t)
  ("e" #'magit-ediff :exit t)
  ("SPC" #'magit-diff-buffer-file-no-select)

  ("f" #'magit-find-file-other-window :exit t)
  ("p" #'magit-blob-previous)
  ("n" #'magit-blob-next)
  ("C-j" #'magit-blob-visit-file)
  ("C-q" #'magit-blob-visit-file :exit t)
  ("Fr" #'magit-file-rename :exit t)
  ("Fd" #'magit-file-delete :exit t)
  ("Fu" #'magit-file-untrack :exit t)
  ("Fc" #'magit-file-checkout :exit t)


  ("g" #'magit-status :exit t)
  ("l" #'magit-log-buffer-file :exit t)
  ("L" #'magit-log :exit t)
  ("c" #'magit-commit :exit t)
  ("P" #'magit-push :exit t)
  ("r" #'vc-revert :exit t)
  ("b" #'magit-blame-dwim :exit t)
  ("B" #'magit-blame :exit t)
  ("$" #'magit-process-buffer :exit t))

(defun hydra-git-or-magit-status ()
  "If in a git-controlled file, call `hydra-git/body', otherwise
proceed to `magit-status'. With prefix arg always call `magit-status'."
  (interactive)
  (if (and (null current-prefix-arg)
           (or magit-buffer-file-name (buffer-file-name))
           (magit-inside-worktree-p t))
      (hydra-git/body)
    (call-interactively #'magit-status)))

(global-set-key (kbd "C-x g") 'hydra-git-or-magit-status)
(define-key text-mode-map (kbd "C-x C-g") 'ignore)

;;* dired
(require 'dired-git-info)

(define-key dired-mode-map (kbd ")") 'dired-git-info-mode)
(define-key dired-mode-map (kbd "C-x g") 'magit-status)

;;* slime
(with-eval-after-load 'slime-repl
  (defun slime-magit-status ()
    (interactive)
    (let ((default-directory (slime-eval `(swank:default-directory))))
      (magit-status)))

  (define-key slime-repl-mode-map (kbd "C-x g") 'slime-magit-status))

;;* magit-status in various maps
(define-key shell-mode-map (kbd "C-x g") 'magit-status)
(define-key compilation-mode-map (kbd "C-x g") 'magit-status)

;;* git-commit fill-column
(add-hook 'git-commit-mode-hook #'display-fill-column-indicator-mode)
(setq git-commit-fill-column 70)

;;* evil
(with-eval-after-load 'evil
  (add-hook 'git-commit-mode-hook #'evil-insert-state)
  (add-hook 'magit-blame-mode-hook #'evil-insert-state))

;;* blame
;; MAYBE differentiate older/newer changes with lighter/darker colors
;; MAYBE rainbow-commits
;; TODO show (hunk number/hunks in commit) for hunks together with commit info

;; more visible minor-mode lighter
(setf (alist-get 'magit-blame-mode minor-mode-alist)
      (list (concat " "
                    (propertize "Blame"
                                'face 'error))))

;; `margin' style: make default, show line separators, reduce width.
(setq magit-blame-styles
      '((margin
         (margin-format . (" %s%f" " %C %a" " %H"))
         (margin-width . 40)
         (margin-face . magit-blame-margin)
         (margin-body-face . (magit-blame-dimmed))
         (show-lines . t))
        (headings
         (heading-format . "%-20a %C %s\n"))
        (highlight
         (highlight-face . magit-blame-highlight))
        (lines
         (show-lines . t)
         (show-message . t))))

;;
(defun magit-blame-refresh ()
  (interactive)
  (magit-blame--refresh))

;; keybindings
(define-key magit-blame-mode-map (kbd "j") 'magit-blame-next-chunk)
(define-key magit-blame-mode-map (kbd "k") 'magit-blame-previous-chunk)
(define-key magit-blame-mode-map (kbd "C-M-j") 'magit-blame-next-chunk-same-commit)
(define-key magit-blame-mode-map (kbd "C-M-k") 'magit-blame-previous-chunk-same-commit)
(define-key magit-blame-mode-map (kbd "R") 'magit-blame-reverse)
(define-key magit-blame-mode-map (kbd "m") 'magit-show-commit)
(define-key magit-blame-mode-map (kbd "g") 'magit-blame-refresh)

(define-key magit-blame-read-only-mode-map (kbd "f") 'magit-diff-show-or-scroll-down)
(define-key magit-blame-read-only-mode-map (kbd "b") 'magit-diff-show-or-scroll-up)
(define-key magit-blame-read-only-mode-map (kbd "a") 'magit-blame-addition)
(define-key magit-blame-read-only-mode-map (kbd "n") 'magit-blame-next-chunk-same-commit)
(define-key magit-blame-read-only-mode-map (kbd "p") 'magit-blame-previous-chunk-same-commit)
(define-key magit-blame-read-only-mode-map (kbd "DEL") 'magit-blame-removal)
(define-key magit-blame-read-only-mode-map (kbd "C-h") 'magit-blame-removal)
(define-key magit-blame-read-only-mode-map (kbd "C-d") 'magit-blame-removal)

(with-eval-after-load 'evil
  (define-key magit-blame-read-only-mode-map (kbd "SPC") leader-map))

;;* TODO: ibuffer

;;* TODO: git-link
;; https://github.com/sshaw/git-link#building-links-and-adding-services
;; Add to hydra-git, support gerrit and/or gitweb.
;; (require 'git-link)

;; (add-to-list git-link-remote-alist ...)

;;* TODO: git-timemachine


;;* diff-mode
(define-key diff-mode-map (kbd "M-k") nil)
(define-key diff-mode-map (kbd "C-M-k") 'diff-hunk-prev)
(define-key diff-mode-map (kbd "C-M-j") 'diff-hunk-next)

;;* flyspell
(add-hook 'git-commit-mode-hook 'flyspell-mode)

(defvar flyspell-magit-ignored-faces
  '(magit-diff-removed-highlight
    magit-section-highlight
    magit-hash
    magit-filename
    magit-diff-file-heading)
  "List of faces to ignore when spellchecking.")

(defun flyspell-magit-verify ()
  "Used for `flyspell-generic-check-word-predicate' in programming modes."
  (unless (eql (point) (point-min))
    ;; (point) is next char after the word. Must check one char before.
    (let ((f (get-text-property (1- (point)) 'face)))
      (not (memq f flyspell-magit-ignored-faces)))))

(defun flyspell-magit-mode ()
  "Turn on `flyspell-mode' for additions only."
  (interactive)
  (setq flyspell-generic-check-word-predicate
        #'flyspell-magit-verify)
  (flyspell-mode 1))

(defun magit-flyspell-hydra ()
  (interactive)
  (flyspell-magit-mode)
  (flyspell-buffer)
  (hydra-flyspell/body))

(define-key magit-mode-map (kbd "C-c C-f") 'magit-flyspell-hydra)
(define-key magit-diff-mode-map (kbd "C-c C-f") 'magit-flyspell-hydra)
(define-key magit-log-mode-map (kbd "C-c C-f") 'magit-flyspell-hydra)

;;* magit-go-forward/backward
(define-key magit-mode-map (kbd "C-c f") 'magit-go-forward)
(define-key magit-mode-map (kbd "C-{") 'magit-go-forward)
(define-key magit-mode-map (kbd "C-c b") 'magit-go-backward)
(define-key magit-mode-map (kbd "C-}") 'magit-go-backward)

;;* git-rebase-mode
(with-eval-after-load 'git-rebase
  (define-key git-rebase-mode-map "j" 'forward-line)
  (define-key git-rebase-mode-map "k" 'git-rebase-backward-line)
  (define-key git-rebase-mode-map "u" 'git-rebase-undo))

;;* magit-process-mode
(define-key magit-process-mode-map (kbd "C-k") 'magit-process-kill)

;;* magit-jump staged/unstaged
(defun magit-jump-between-staged-unstaged ()
  (interactive)
  (let ((pt (point)))
    (magit-jump-to-staged)
    (when (eq pt (point))
      (magit-jump-to-unstaged))))

(define-key magit-status-mode-map (kbd "H-s") 'magit-jump-between-staged-unstaged)

(provide 'configure-git)
