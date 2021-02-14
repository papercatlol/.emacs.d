;; -*- lexical-binding: t -*-

;;* general
(require 'org)

(setq org-default-notes-file (concat org-directory "/notes.org")
      org-startup-indented t
      org-hide-leading-stars t
      org-special-ctrl-a/e t
      org-special-ctrl-k t
      org-special-ctrl-o t)

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

;;* org dumb task tracker
(defvar odtt:task-file "~/work/tasks.org")

(defvar odtt:current-files nil)

;; (setq org-clock-out-switch-to-state "TODO")
;; (setq org-clock-in-switch-to-state "CURRENT")

(cl-defun counsel-goto-task (&optional (files (list odtt:task-file)))
  (interactive)
  (when-let ((odtt:current-files files)
             (todos (odtt:collect-todos files)))
    (ivy-read "Task: " todos
              :action #'odtt:ivy-action-clock-in
              :keymap counsel-goto-task-map
              :preselect (car (find-if (lambda (item)
                                         (and (listp item) (third item)))
                                       todos))
              :caller 'counsel-goto-task
              ;; :dynamic-collection nil
              )))

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
                     (loop for match = (re-search-forward org-todo-line-regexp nil t)
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
  (org-goto-marker-or-bmk (second item)))

(defun odtt:ivy-goto-task ()
  (interactive)
  (ivy-exit-with-action #'odtt:ivy-action-goto))

(defun odtt:ivy-action-clock-in (item)
  (if-let* ((marker (and (listp item) (second item)))
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
  (with-selected-marker (second item)
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

(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
(global-set-key (kbd "<f7>") 'org-dumb-time-tracker)

;;** org-dumb-time-tracker interactive command alias
(defun org-dumb-time-tracker ()
  (interactive)
  (call-interactively  #'counsel-goto-task))

;;* capture templates & org-roam
(require 'configure-org-roam)

(global-set-key (kbd "<f6>") 'hydra-org-roam/body)

;; (setq org-capture-templates
;;       `(("w" "Work" entry (file ,odtt:task-file)
;;              "* TODO %?\n  %u\n  %a" :prepend t)
;;         ("t" "Task" entry (file+headline "" "Tasks")
;;              "* TODO %?\n  %u\n  %a")))
;; (global-set-key (kbd "<f6>") 'counsel-org-capture)


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
  (if-let* ((connected? (slime-connected-p))
            (in-block? (org-in-src-block-p))
            (info (org-babel-get-src-block-info t))
            (lang (car info))
            (lisp? (string= lang "lisp"))
            (expanded-block (org-babel-expand-src-block)))
      (slime--repl-insert-string expanded-block)
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

(setf (alist-get 'slime-repl-mode org-rich-yank-mode-translation-alist)
      'lisp-mode)

(define-key org-mode-map (kbd "C-M-y") 'org-rich-yank)

;;* org-download
(require 'org-download)

(add-hook 'dired-mode-hook 'org-download-enable)

;; TODO: combine with org-rich-yank and bind to C-M-y
(define-key org-mode-map (kbd "C-c y") 'org-download-clipboard)

(provide 'configure-org)
