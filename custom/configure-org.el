;; -*- lexical-binding: t -*-

;;* general
(require 'org)

;;* quickly insert src blocks and stuff (e.g. <s)
;; We use hippie-expand to trigger `tempo.el' tag expansion
(require 'org-tempo)

(setq org-default-notes-file (concat org-directory "/notes.org")
      org-startup-indented t
      org-hide-leading-stars t
      org-special-ctrl-a/e t
      org-special-ctrl-k t
      org-special-ctrl-o t
      org-cycle-separator-lines 0
      org-return-follows-link nil
      org-startup-folded 'content
      org-use-fast-tag-selection 'expert
      ;; src blocks
      org-edit-src-content-indentation 0
      org-edit-src-persistent-message nil
      org-src-window-setup 'current-window
      org-archive-reversed-order t
      )


(advice-add 'org-archive-default-command :after #'org-save-all-org-buffers)
(add-hook 'org-clock-out-hook #'save-buffer-no-message)

(defun save-buffer-no-message ()
  (let ((inhibit-message t))
    (save-buffer)))

(define-key org-mode-map (kbd "C-c C-x <C-i>") 'org-clock-in)

;;* display time as decimal hours in clock tables
(setq org-duration-format
      '(("h" . t) (special . 2)))

;;* clock persistence
(setq org-clock-persist t)
(org-clock-persistence-insinuate)

;;* modeline clock
(setq org-clock-mode-line-total 'today)

(defun org-clock-out-update-mode-line ()
  (setq org-mode-line-string nil)
  (force-mode-line-update))
(add-hook 'org-clock-out-hook #'org-clock-out-update-mode-line)

;;* add timestamps when closing tasks
(setq org-log-done 'time)

;;* refiling
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

(defun org-buffer-list ()
  "Get all open org-mode buffers."
  (loop for buf in (buffer-list)
        when (compat--provided-mode-derived-p
              (buffer-local-value 'major-mode buf) 'org-mode)
          collect buf))

(setq org-refile-targets
      '((nil :maxlevel . 3)             ; current buffer headlies
        (org-agenda-files :maxlevel . 2)
        (org-buffer-list :maxlevel . 3)))


;;* org dumb task tracker
(defvar odtt:task-file "~/org/tasks.org")

(defvar odtt:current-files nil)

;; (setq org-clock-out-switch-to-state "TODO")
;; (setq org-clock-in-switch-to-state "CURRENT")

;;** org todo keyword faces
(setq org-todo-keyword-faces
      '(("GERRIT" . "#ee7b00")
        ("CURRENT" . "purple")
        ("WAITING" . "#ee7b00")))

;;** counsel-goto-task
(cl-defun counsel-goto-task (&optional (files (list odtt:task-file)))
  (interactive)
  (when-let ((odtt:current-files files)
             (todos (odtt:collect-todos files)))
    (ivy-read "Task: " todos
              :action #'odtt:ivy-action-clock-in
              :keymap counsel-goto-task-map
              :preselect (car (find-if (lambda (item)
                                         (and (listp item) (cl-third item)))
                                       todos))
              :caller 'counsel-goto-task
              ;; :dynamic-collection nil
              )))

(defun counsel-goto-task--insert (task)
  (insert
   (let ((text (car task)))
     (if (string-match
          (rx "*" (* space)
              (or "TODO" "DONE" "GERRIT" "WAITING"
                  "CURRENT" "SPR")
              (* space)
              (group (+? any))
              (* space)
              (? ":" (+ (any alnum ":_")) ":")
              eol)
          text)
         (match-string-no-properties 1 text)
       text))))

(ivy-add-actions 'counsel-goto-task
                 '(("M-a" counsel-goto-task--insert "Insert task description")))

;;** utils
(cl-defun odtt:collect-todos (&optional (files (list odtt:task-file)))
  (labels ((%collect (file)
             (when-let ((buf (or (find-buffer-visiting file)
                                 (let ((vc-follow-symlinks t))
                                   (find-file-noselect file t)))))
               (let ((current-task-pos
                       (when (org-clocking-p)
                         (with-current-buffer (marker-buffer org-clock-marker)
                           (save-excursion
                            (goto-char org-clock-marker)
                            (re-search-backward org-todo-line-regexp nil t))))))
                 (with-current-buffer buf
                   (save-excursion
                    (save-restriction
                     (widen)
                     (goto-char (point-min))
                     (cl-loop for match = (re-search-forward org-todo-line-regexp nil t)
                              while match
                              for beg = (match-beginning 0)
                              for end = (line-end-position)
                              for clocking? = (and current-task-pos (= beg current-task-pos) '(t))
                              collect (list* (buffer-substring beg end)
                                             (move-marker (make-marker) beg)
                                             clocking?)))))))))
    (mapcan #'%collect files)))

(defun odtt:ivy-refresh ()
  "Fetch updated todos and refresh ivy minibuffer candidates accodringly."
  (interactive)
  (when-let ((new-todos (odtt:collect-todos odtt:current-files)))
    (setf (ivy-state-collection ivy-last) new-todos)
    (setq ivy--old-re "") ; a hack to stop results from caching
    (ivy-update-candidates (mapcar #'car new-todos)))
  (ivy--exhibit))

(cl-defmacro with-selected-marker (marker &body body)
  (let ((buf (gensym "buf"))
        (m (gensym "marker")))
    `(let ((,m ,marker))
       (when (markerp ,m)
         (when-let ((,buf (marker-buffer ,m)))
           (when (buffer-live-p ,buf)
             (with-current-buffer ,buf
               (goto-char ,m)
               ,@body)))))))

(cl-defmacro with-ivy-action (action &body body)
  "Adapted from `ivy-dispatching-call'. Temporarily bind ivy-action
to ACTION and execute BODY forms."
  (let ((old-actions (gensym "old-actions"))
        (new-action (gensym "new-action")))
    `(let ((,old-actions (copy-sequence (ivy-state-action ivy-last)))
           (,new-action ,action))
       (setq ivy-current-prefix-arg current-prefix-arg)
       (unwind-protect
            (progn
              (ivy-set-action ,action)
              ,@body)
         (ivy-set-action ,old-actions)))))

;;** actions
(defun odtt:ivy-action-goto (item)
  (org-goto-marker-or-bmk (cl-second item)))

(defun odtt:ivy-goto-task ()
  (interactive)
  (ivy-exit-with-action #'odtt:ivy-action-goto))

(defun odtt:ivy-action-clock-in (item)
  (if-let* ((marker (and (listp item) (cl-second item)))
            (buf (marker-buffer marker)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (save-excursion
           (goto-char marker)
           (org-clock-in)
           (let ((inhibit-message t))
             (save-buffer)))))
    (odtt:new-task item)
    (odtt:ivy-refresh)))

(defun odtt:new-task (title &optional file)
  (interactive)
  (when-let* ((file (or file
                        (and (cdr odtt:current-files)
                             (completing-read "File: " odtt:current-files))
                        (car odtt:current-files)
                        odtt:task-file))
              (buf (or (find-buffer-visiting file)
                       (find-file file))))
    (with-current-buffer buf
      (save-excursion
       (goto-char (point-min))
       (org-next-visible-heading 1)
       (org-insert-todo-heading t)
       (insert title)))))

(defun odtt:ivy-action-cycle-todo (item)
  (with-selected-marker (cl-second item)
    (let ((inhibit-message t))
      (call-interactively #'org-todo))))

(defun odtt:ivy-cycle-todo ()
  (interactive)
  (with-ivy-action #'odtt:ivy-action-cycle-todo
    (call-interactively #'ivy-call)
    (odtt:ivy-refresh)))

;;** ivy-configure
(ivy-configure
 'counsel-goto-task
 :display-transformer-fn #'counsel-goto-task-transformer)

(defun counsel-goto-task-transformer (item)
  item)

;;** keymap
(defvar counsel-goto-task-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m ivy-minibuffer-map)
    m))

;;** keybindings
(define-key counsel-goto-task-map (kbd "C-c C-t") 'odtt:ivy-cycle-todo)
(define-key counsel-goto-task-map (kbd "<f5>") 'odtt:ivy-refresh)
(define-key counsel-goto-task-map (kbd "C-c C-x C-o") 'org-clock-out)
(define-key counsel-goto-task-map (kbd "C-c C-x C-i") 'odtt:ivy-action-clock-in)
(define-key counsel-goto-task-map (kbd "M-.") 'odtt:ivy-goto-task)
(define-key counsel-goto-task-map (kbd "<f7>") 'odtt:ivy-goto-task)

(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
(global-set-key (kbd "<f7>") 'org-dumb-time-tracker)

;;** org-dumb-time-tracker interactive command alias
(defun org-dumb-time-tracker ()
  (interactive)
  (call-interactively #'counsel-goto-task))

;;* capture templates & denote
;;(require 'configure-org-roam)

;;(global-set-key (kbd "<f6>") 'hydra-org-roam/body)

;; (setq org-capture-templates
;;       `(("w" "Work" entry (file ,odtt:task-file)
;;              "* TODO %?\n  %u\n  %a" :prepend t)
;;         ("t" "Task" entry (file+headline "" "Tasks")
;;              "* TODO %?\n  %u\n  %a")))
;; (global-set-key (kbd "<f6>") 'counsel-org-capture)

(require 'configure-denote)


;;* rangereport
;; From: https://sachachua.com/blog/2007/12/clocking-time-with-emacs-org/
(defun org-dblock-write:rangereport (params)
  "Display day-by-day time reports."
  (let* ((ts (plist-get params :tstart))
         (te (plist-get params :tend))
         (start (time-to-seconds
                 (apply 'encode-time (org-parse-time-string ts))))
         (end (time-to-seconds
               (apply 'encode-time (org-parse-time-string te))))
         day-numbers)
    (setq params (plist-put params :tstart nil))
    (setq params (plist-put params :end nil))
    (while (<= start end)
      (save-excursion
       (insert "\n\n"
               (format-time-string (car org-time-stamp-formats)
                                   (seconds-to-time start))
               "----------------\n")
       (org-dblock-write:clocktable
        (plist-put
         (plist-put
          params
          :tstart
          (format-time-string (car org-time-stamp-formats)
                              (seconds-to-time start)))
         :tend
         (format-time-string (car org-time-stamp-formats)
                             (seconds-to-time end))))
       (setq start (+ 86400 start))))))

;;* org-table: copy field at point
(defun org-table-kill-field (&optional N)
  (interactive)
  (when-let ((value (org-table-get-field N)))
    (kill-new (string-trim value))
    (message value)))

;; TODO: conditional binding w/ (:menu ...) to only do this inside org tables
(define-key org-mode-map (kbd "C-c w") 'org-table-kill-field)



;;*
(define-key org-mode-map (kbd "C-c p") 'outline-previous-visible-heading)
(define-key org-mode-map (kbd "C-c n") 'outline-next-visible-heading)

;;* org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)
   (shell . t)
   (python . t)))

(defun org-special-ctrl-c-ctrl-y ()
  "A hack to copy current lisp src block to slime repl."
  (interactive)
  (if-let* ((in-block? (org-in-src-block-p))
            (info (org-babel-get-src-block-info t))
            (lang (car info))
            (expanded-block (org-babel-expand-src-block)))
      (cond ((and (slime-connected-p) (string= lang "lisp"))
             (slime--repl-insert-string expanded-block))
            ((string= lang "shell")
             (copy-to-equake expanded-block)))
    (call-interactively #'org-evaluate-time-range)))

(define-key org-mode-map (kbd "C-c C-y") 'org-special-ctrl-c-ctrl-y)

;;* org-present
(require 'org-present nil t)

(with-eval-after-load 'org-present
  (define-key org-present-mode-keymap (kbd "C-c C-n") 'org-present-next)
  (define-key org-present-mode-keymap (kbd "C-c C-p") 'org-present-prev)

  (defvar org-present-mode-line-format nil)

  (defun org-present--setup ()
    (org-present-big)
    (org-display-inline-images)
    ;; (org-present-hide-cursor)
    (setq org-present-mode-line-format mode-line-format)
    (setq-local mode-line-format nil))

  (defun org-present--teardown ()
    (org-present-small)
    ;; (org-remove-inline-images)
    ;; (org-present-show-cursor)
    (setq-local mode-line-format org-present-mode-line-format))

  (add-hook 'org-present-mode-hook #'org-present--setup)
  (add-hook 'org-present-mode-quit-hook #'org-present--teardown)

  (define-key org-mode-map (kbd "C-c P") 'org-present)
  )

;;* org-rich-yank
(org-rich-yank-enable)

(defvar org-rich-yank-mode-translation-alist nil
  "Alist major-mode -> mode to use in the org src code block.")

(setf (alist-get 'slime-repl-mode org-rich-yank-mode-translation-alist)
      'lisp-mode)

;; Add major mode translation functionality back.
(defun org-rich-yank+ ()
  "Yank, surrounded by #+BEGIN_SRC block with major mode of originating buffer."
  (interactive)
  (if org-rich-yank--buffer
      (let* ((source-mode (org-rich-yank--major-mode))
             (paste
               (concat
                (format "#+BEGIN_SRC %s\n"
                        (replace-regexp-in-string "-mode$" "" (symbol-name source-mode)))
                (org-rich-yank--trim-nl (current-kill 0))
                (format "\n#+END_SRC\n")
                (org-rich-yank--link))))
        (insert
         (if org-rich-yank-add-target-indent
             (org-rich-yank-indent paste)
           paste)))
    (message "`org-rich-yank' doesn't know the source buffer – please `kill-ring-save' and try again.")))

(defun org-rich-yank--major-mode ()
  (when-let ((mode (buffer-local-value 'major-mode org-rich-yank--buffer)))
    (or (alist-get mode org-rich-yank-mode-translation-alist)
        mode)))

(define-key org-mode-map (kbd "C-M-y") 'org-rich-yank+)

;;* org-download
(require 'org-download)

(add-hook 'dired-mode-hook 'org-download-enable)

;; TODO: combine with org-rich-yank and bind to C-M-y
(define-key org-mode-map (kbd "C-c y") 'org-download-clipboard)

;;* timestamps
;; [C-u] to include time
;; [C-u C-u] to simply insert current timestamp (maybe reverse this behavior?)
;; `org-time-stamp'          = <2022-07-19 Tue>
;; `org-time-stamp-inactive' = [2022-07-19 Tue]
(define-key ctl-x-map (kbd "T") 'org-time-stamp-inactive)

;;* orgtbl-mode
(define-key orgtbl-mode-map (kbd "C-c SPC") 'nil)

;;* right align tags (Nicolas Rougier)
;; https://www.reddit.com/r/emacs/comments/185e4k1/orgmode_tag_right_alignment/?share_id=epIzc-9qF-duOW7o6k7lX
(add-to-list 'font-lock-extra-managed-props 'display)
(font-lock-add-keywords
 'org-mode
 `(("^.*?\\( \\)\\(:[[:alnum:]_@#%:]+:\\)$"
    (1 `(face nil
              display (space :align-to (- right ,(length (match-string 2)) 3)))
       prepend)))
 t)

;;* TODO setup org-speed-keys; defaults make no sense
;;(setq org-use-speed-commands t)

;; random keybindings
(define-key org-mode-map (kbd "C-c C-8") 'org-ctrl-c-star)
(define-key org-mode-map (kbd "C-c SPC") nil)
(define-key org-mode-map (kbd "C-,") nil)
(define-key org-mode-map (kbd "C-c h") 'org-up-element)
(define-key org-mode-map (kbd "C-c l") 'org-down-element)
(define-key org-mode-map (kbd "C-c L") 'org-toggle-link-display)
(define-key org-mode-map (kbd "C-c I") 'org-toggle-inline-images)
(define-key org-mode-map (kbd "C-c C-s") 'counsel-imenu-dwim)
(define-key org-mode-map (kbd "C-c s") 'org-schedule)
(define-key org-mode-map (kbd "C-c C-4") 'org-archive-subtree)

;;* TODO slime-link (should do slime-xref on a symbol)

(provide 'configure-org)
