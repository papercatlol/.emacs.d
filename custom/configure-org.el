;; -*- lexical-binding: t -*-

;;* general
(require 'org)

(setq org-default-notes-file (concat org-directory "/notes.org")
      org-startup-indented t
      org-hide-leading-stars t)

(advice-add 'org-archive-default-command :after #'org-save-all-org-buffers)
(add-hook 'org-clock-out-hook #'save-buffer-no-message)

(defun save-buffer-no-message ()
  (let ((inhibit-message t))
    (save-buffer)))

(global-set-key (kbd "<f6>") 'counsel-org-capture)
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
              ;; :dynamic-collection nil
              )))

;;** utils
(cl-defun odtt:collect-todos (&optional (files (list odtt:task-file)))
  (labels ((%collect (file)
             (when-let ((buf (or (find-buffer-visiting file)
                                 (find-file file))))
               (with-current-buffer buf
                 (save-excursion
                  (goto-char (point-min))
                  (loop for match = (re-search-forward org-todo-line-regexp nil t)
                        while match
                        for beg = (match-beginning 0)
                        for end = (line-end-position)
                        collect (cons (buffer-substring beg end)
                                      (move-marker (make-marker) beg))))))))
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
  (org-goto-marker-or-bmk (cdr item)))

(defun odtt:ivy-goto-task ()
  (interactive)
  (ivy-exit-with-action #'odtt:ivy-action-goto))

(defun odtt:ivy-action-clock-in (item)
  (if-let* ((marker (cdr-safe item))
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
  (with-selected-marker (cdr item)
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

(defun org-dumb-time-tracker ()
  (interactive)
  (call-interactively  #'counsel-goto-task))

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


(provide 'configure-org)
