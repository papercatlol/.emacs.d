;; -*- lexical-binding: t -*-

;;* Installation:
;; https://hobo.house/2017/07/17/using-offlineimap-with-the-gmail-imap-api/
;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

;; 1 char is plenty for marks. Need to set this before loading `mu4e' because
;; other variables are calculated from it.
(defconst mu4e~mark-fringe-len 1)

(require 'mu4e)

;;* evil bindings. Some don't work though...
;; TODO get rid of `evil-collection'
(with-eval-after-load 'evil
  (require 'evil-collection)
  (evil-collection-mu4e-setup)
  (define-key mu4e-view-mode-map "h" 'backward-char)
  (define-key mu4e-view-mode-map "j" 'next-line)
  (define-key mu4e-view-mode-map "k" 'previous-line)
  (define-key mu4e-view-mode-map "n" nil)
  (define-key mu4e-view-mode-map "N" nil)
  (define-key mu4e-view-mode-map "l" 'forward-char)
  (define-key mu4e-view-mode-map "V" 'evil-visual-line)
  (define-key mu4e-view-mode-map "v" 'evil-visual-char-or-expand-region)
  (define-key mu4e-view-mode-map (kbd "C-c v") 'mu4e-view-verify-msg-popup)
  (define-key mu4e-view-mode-map (kbd "C-=") 'mu4e-headers-split-view-grow)
  (define-key mu4e-headers-mode-map "h" 'backward-char)
  (define-key mu4e-headers-mode-map "s" 'mu4e-headers-search)
  (define-key mu4e-headers-mode-map "S" 'mu4e-headers-search-edit)
  (define-key mu4e-headers-mode-map "H" 'mu4e-headers-query-prev)
  (define-key mu4e-headers-mode-map "L" 'mu4e-headers-query-next)
  (define-key mu4e-headers-mode-map "n" nil)
  (define-key mu4e-headers-mode-map "N" nil)
  (define-key mu4e-headers-mode-map "X" 'mu4e-kill-update-mail)
  (define-key mu4e-headers-mode-map (kbd "C-=") 'mu4e-headers-split-view-grow)
  (define-key mu4e-headers-mode-map (kbd "/") 'mu4e-headers-search-narrow)

  (evil-define-key '(normal) mu4e-view-mode-map
    "[" 'mu4e-view-headers-prev-unread
    "]" 'mu4e-view-headers-next-unread)

  (evil-define-key '(normal) mu4e-headers-mode-map
    "[" 'mu4e-headers-prev-unread
    "]" 'mu4e-headers-next-unread
    "q" 'mu4e-headers-query-prev
    "s" 'mu4e-headers-search
    "S" 'mu4e-headers-search-edit
    "/" 'mu4e-headers-search-narrow))

;;* general
;; A lot of these are taken from
;; https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/
(setq mu4e-view-prefer-html t
      mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include nil
      mu4e-compose-format-flowed t
      mu4e-headers-show-threads nil)
(setq mu4e-confirm-quit nil)

;;* columns in *mu4e-headers* view
(setq mu4e-headers-fields
      '((:human-date . 8)
        (:flags . 2)
        (:from . 30)
        (:subject)
        ;;(:mailing-list . 10)
        ))

;;* fancy chars
(setq mu4e-use-fancy-chars t)

;;** threads
;; I prefer the original thin characters for threads.
(setq mu4e-headers-thread-child-prefix '("├>" . "├>"))
(setq mu4e-headers-thread-last-child-prefix '("└>" . "└>"))
(setq mu4e-headers-thread-connection-prefix '("│" . "│"))
(setq mu4e-headers-thread-orphan-prefix '("┬>" . "┬>"))
(setq mu4e-headers-thread-single-orphan-prefix '("─>" . "─>"))
(setq mu4e-headers-thread-blank-prefix '(" " . " "))
;; unchanged
(setq mu4e-headers-thread-duplicate-prefix '("=" . "≡ "))

;;** flags
;; Since we're using less characters for the 'flags' column, it's default
;; name (Flgs) gets shortened to ellipsis.
(setf (cl-getf (alist-get :flags mu4e-header-info) :shortname)
      "F")

(setq mu4e-headers-visible-flags
      '(draft flagged  ;; new
        passed replied ;; seen
        trashed attach encrypted
        signed ;; unread
        ))
;;(setq mu4e-headers-attach-mark '("a" . "✉")) ; this symbol is too tall :c
(setq mu4e-headers-attach-mark '("+" . "+"))
(setq mu4e-headers-replied-mark '("R" . "◄"))

;;** mu4e-marks
(setf (cl-getf (alist-get 'read mu4e-marks) :char)
      '("!" . "✔"))
(setf (cl-getf (alist-get 'unread mu4e-marks) :char)
      '("?" . "✘"))
(setf (cl-getf (alist-get 'flag mu4e-marks) :char)
      '("+" . "✚"))
(setf (cl-getf (alist-get 'unflag mu4e-marks) :char)
      '("-" . "╺"))

;;* 24-hour time & DD.MM.YY date
(setq mu4e-headers-time-format "%T")
(setq mu4e-headers-date-format "%d.%m.%y")
(setq mu4e-headers-long-date-format "%A %d %b %Y %T")
(setq mu4e-view-date-format "%A %d %b %Y %T")
(setq mu4e-date-format-long "%A %d %b %Y %T")

;; to view selected message in the browser, no signin, just html mail
(add-to-list 'mu4e-headers-actions
             '("browser" . mu4e-action-view-in-browser) t)

(add-to-list 'mu4e-view-actions
             '("browser" . mu4e-action-view-in-browser) t)

;; TODO Need to install some email-to-pdf binary for this to work.
(add-to-list 'mu4e-headers-actions
             '("pdf" . mu4e-action-view-as-pdf) t)

(add-to-list 'mu4e-view-actions
             '("pdf" . mu4e-action-view-as-pdf) t)
;;* enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;;* smtp
(require 'smtpmail)

;;set up queue for offline email
;;use mu mkdir  /home/il/Maildir/acc/queue to set up first
(setq smtpmail-queue-mail nil)  ;; start in normal mode

;;* saving attachments
;; TODO: something clever with choosing where to save attachments
(setq mu4e-save-multiple-attachments-without-asking t)
(setq mu4e-attachment-dir "/home/il/Downloads/mu4e")

(defun mu4e-choose-attachment-dir ()
  "Read and create a directory path relative to
`mu4e-attachment-dir'."
  (when-let* ((default-directory mu4e-attachment-dir)
              (dir (read-directory-name "mu4e attachment dir: ")))
    (unless (file-exists-p dir) (make-directory dir t))
    (file-name-as-directory dir)))

(defun mu4e~view-request-attachments-dir-wrapper (path)
  (let ((mu4e-attachment-dir path))
    (mu4e-choose-attachment-dir)))

(advice-add 'mu4e~view-request-attachments-dir :override
            #'mu4e~view-request-attachments-dir-wrapper)

;;* compose in a new frame
(setq mu4e-compose-in-new-frame t)

;;* org-mu4e
(require 'org-mu4e)

;; convert org mode to HTML automatically
(setq org-mu4e-convert-to-html t)

;; from vxlabs config
;; show full addresses in view message (instead of just names)
;; toggle per name with M-RET
(setq mu4e-view-show-addresses 't)

;;* getting mail
(setq mu4e-get-mail-command "offlineimap")
(setq mu4e-update-interval 120)
(setq mu4e-hide-index-messages t)

;;* mu4e-context
(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy 'always-ask)
(unless (cl-member "papercatlol" mu4e-contexts :test #'string= :key #'mu4e-context-name)
  (add-to-list
   'mu4e-contexts
   (make-mu4e-context
    :name "papercatlol" ;;for acc2-gmail
    :enter-func (lambda () (mu4e-message "Entering context papercatlol"))
    :leave-func (lambda () (mu4e-message "Leaving context papercatlol"))
    :match-func (lambda (msg)
		  (when msg
		    (mu4e-message-contact-field-matches
		     msg '(:from :to :cc :bcc) "papercatlol@gmail.com")))
    :vars '((user-mail-address . "papercatlol@gmail.com")
	    (user-full-name . "papercatlol")
	    (mu4e-sent-folder . "/Papercatlol/Sent Mail")
	    (mu4e-drafts-folder . "/Papercatlol/Drafts")
	    (mu4e-trash-folder . "/Papercatlol/Trash")
            (mu4e-inbox-dir . "/Papercatlol/INBOX")
	    ;; (mu4e-compose-signature . "-Papercatlol")
	    (mu4e-compose-format-flowed . t)
	    (smtpmail-queue-dir . "/home/il/mail/Papercatlol/queue")
	    (message-send-mail-function . smtpmail-send-it)
	    (smtpmail-smtp-user . "papercatlol@gmail.com")
	    (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
	    (smtpmail-auth-credentials . (expand-file-name "/home/il/.authinfo.gpg"))
	    (smtpmail-default-smtp-server . "smtp.gmail.com")
	    (smtpmail-smtp-server . "smtp.gmail.com")
	    (smtpmail-smtp-service . 587)
	    (smtpmail-debug-info . t)
	    (smtpmail-debug-verbose . t)
	    (mu4e-maildir-shortcuts . (("/Papercatlol/INBOX"     . ?i)
	        		       ("/Papercatlol/Sent Mail" . ?s)
	        		       ("/Papercatlol/Trash"     . ?t)
	        		       ("/Papercatlol/All Mail"  . ?a)
	        		       ("/Papercatlol/Starred"   . ?r)
	        		       ("/Papercatlol/drafts"    . ?d)
	        		       ))
            ))))

;;* HACK to allow keys other than ?o to designate "other" in `mu4e-maildir-shortcuts'
(defvar mu4e-maildir-shortcuts-other-key ?J
  "A char that designates 'other' option in `mu4e-maildir-shortcuts'.")

(defun mu4e-ask-maildir+ (prompt)
  "Ask the user for a shortcut (using PROMPT) as per
(mu4e-maildir-shortcuts), then return the corresponding folder
name. If the special shortcut 'o' (for _o_ther) is used, or if
`(mu4e-maildir-shortcuts)' evaluates to nil, let user choose from
all maildirs under `mu4e-maildir'."
  (let ((prompt (mu4e-format "%s" prompt)))
    (if (not (mu4e-maildir-shortcuts))
        (substring-no-properties
         (funcall mu4e-completing-read-function prompt (mu4e-get-maildirs)))
      (let* ((mlist (append (mu4e-maildir-shortcuts)
                            `((:maildir "Other"  :key ,mu4e-maildir-shortcuts-other-key))))
             (fnames
              (mapconcat
               (lambda (item)
                 (concat
                  "["
                  (propertize (make-string 1 (plist-get item :key))
                              'face 'mu4e-highlight-face)
                  "]"
                  (plist-get item :maildir)))
               mlist ", "))
             (kar (read-char (concat prompt fnames))))
        (if (member kar `(?/ ,mu4e-maildir-shortcuts-other-key)) ;; user chose 'other'?
            (substring-no-properties
             (funcall mu4e-completing-read-function prompt
                      (mu4e-get-maildirs) nil nil "/"))
          (or (plist-get
               (cl-find-if (lambda (item) (= kar (plist-get item :key)))
                           (mu4e-maildir-shortcuts)) :maildir)
              (mu4e-warn "Unknown shortcut '%c'" kar)))))))

(advice-add 'mu4e-ask-maildir :override #'mu4e-ask-maildir+)

;;* show thread at point
;; TODO look at https://github.com/rougier/mu4e-thread-folding
(defun mu4e-show-thread-at-point ()
  (interactive)
  (when-let ((msg (mu4e-message-at-point t)))
    (mu4e-action-show-thread msg)))

(define-key mu4e-headers-mode-map (kbd "<tab>") 'mu4e-show-thread-at-point)

;;* mu4e-maildir-shortcuts
(setq mu4e-maildir-shortcuts
      (append mu4e-maildir-shortcuts
              '(("/Papercatlol/All Mail" . ?p))))

(setq mu4e-completing-read-function 'ivy-completing-read)

;;* attach files from dired
;; From https://zmalltalker.com/linux/mu.html
(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;;* org-capture-template
(with-eval-after-load 'org-capture
  (setf (alist-get "m" org-capture-templates nil nil #'string=)
	`("Work TODO from mu4e"
          entry (file ,odtt:task-file)
          "* TODO %:subject\n  %u\n %a%?" :prepend t))

  (setf (alist-get "m" org-capture-templates-contexts nil nil #'string=)
	'(((in-mode . "mu4e-headers-mode")
           (in-mode . "mu4e-view-mode")))))

;;* mu4e-alert
(mu4e-alert-set-default-style 'libnotify)
(setq mu4e-alert-email-notification-types '(count subjects))

(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)

;;* mu4e-headers-first/last-unread
(defun mu4e-headers-last-unread ()
  (interactive)
  (goto-char (point-max))
  (mu4e-headers-prev-unread))

(define-key mu4e-headers-mode-map (kbd "}") 'mu4e-headers-last-unread)

;; Most of the time this will be the first message.
(defun mu4e-headers-first-unread ()
  (interactive)
  (goto-char (point-min))
  (or (when-let* ((msg (mu4e-message-at-point t))
                  (flags (mu4e-msg-field msg :flags)))
        (and (member 'unread flags)     ; msg at point is unread -> don't move
             (not (member 'trashed flags)))) ; anywhere
      (mu4e-headers-next-unread)))

(define-key mu4e-headers-mode-map (kbd "{") 'mu4e-headers-first-unread)

;;* search-narrow: set initial filter to `bug-reference' keyword at point
(defun mu4e-headers-search-narrow+bug-reference (filter)
  "Like `mu4e-headers-search-narrow', but set initial filter to
`bug-reference' match in current message's subject."
  (interactive
   (list (read-string (mu4e-format "Narrow down to: ")
                      (when-let* ((regexp (and bug-reference-mode
                                               bug-reference-bug-regexp))
                                  (msg (mu4e-message-at-point t))
                                  (subject (mu4e-message-field msg :subject))
                                  (keyword (and (string-match regexp subject)
                                                (match-string-no-properties 0 subject))))
                        (format "subject:%s" keyword))
                      'mu4e~headers-search-hist nil t)))
  (unless mu4e~headers-last-query
    (mu4e-warn "There's nothing to filter"))
  (mu4e-headers-search
   (format "(%s) AND (%s)" mu4e~headers-last-query filter)))

(advice-add 'mu4e-headers-search-narrow :override #'mu4e-headers-search-narrow+bug-reference)

;;* mu4e-headers-mark-for-read-backward

;; Like mu4e-headers-mark-for-read, but move to prev message after marking.
;; Since mu4e lists most recent messages first, I often find myself going
;; through them bottom to top.
(defun mu4e-headers-mark-for-read-backward ()
  "Set mark MARK on the message at point or on all messages in the
region if there is a region, then move to the previous message."
  (interactive)
  (mu4e-mark-set 'read)
  (when mu4e-headers-advance-after-mark (mu4e-headers-prev)))

(define-key mu4e-headers-mode-map (kbd "@") 'mu4e-headers-mark-for-read-backward)

;;* mu4e-goto-inbox
(defun mu4e-goto-inbox ()
  (interactive)
  (when (and mu4e-inbox-dir
             (file-exists-p (concat (mu4e-root-maildir) mu4e-inbox-dir)))
    (mu4e~headers-jump-to-maildir mu4e-inbox-dir)))


(define-key mu4e-main-mode-map "I" 'mu4e-goto-inbox)
(define-key mu4e-headers-mode-map "I" 'mu4e-goto-inbox)
(define-key mu4e-view-mode-map "I" 'mu4e-goto-inbox)

;;* flyspell
(add-hook 'mu4e-compose-mode-hook #'flyspell-mode)

;;* wrapper around `mu4e-found-func'
(defvar-local mu4e~headers-last-count 0
  "The number of results returned by most recent query.")

(defun mu4e~headers-found-silent-handler (count)
  "Cache COUNT and suppress messages."
  (when (buffer-live-p (mu4e-get-headers-buffer))
    (with-current-buffer (mu4e-get-headers-buffer)
      (setq mu4e~headers-last-count count)
      (let ((inhibit-message t))
        (mu4e~headers-found-handler count)))))

(defvar mu4e-found-func #'mu4e~headers-found-silent-handler)

;;* custom header-line for `mu4e-headers'
(defface mu4e-header-line-face
    '((t (:inherit header-line)))
  "Face for mu4e header-line."
  :group 'mu4e-faces)

(defface mu4e-header-line-updating-face
    '((t (:inherit mu4e-header-line-face :foreground "green")))
  "Face for mu4e header-line updating indicator."
  :group 'mu4e-faces)

(defun mu4e~better-header-line ()
  (cl-labels ((%find (query)
                (cl-find query (plist-get mu4e~server-props :queries)
                         :key (lambda (q) (plist-get q :query))
                         :test #'string=)))
    (let* ((today (%find "date:today..now"))
           ;; HACK divide by 2 because mu4e doubles the results for some reason
           (today-count (/ (plist-get today :count) 2))
           (today-unread (/ (plist-get today :unread) 2))
           (unread-count (mu4e~headers-count-unread))
           (update-running-p
             (and (buffer-live-p mu4e~update-buffer)
                  (process-live-p (get-buffer-process mu4e~update-buffer)))))
      (concat
       (propertize (format "Today: %s/%s   Hits: %s   Unread: %s   Query: %s"
                           (- today-count today-unread) today-count
                           mu4e~headers-last-count
                           unread-count
                           mu4e~headers-last-query)
                   'face 'mu4e-header-line-face)
       (when update-running-p
         (propertize " Updating..." 'face 'mu4e-header-line-updating-face))))))

(defun mu4e~headers-count-unread ()
  "Return the number of unread messages in the current header view."
  (let ((count 0))
    (mu4e-headers-for-each
     (lambda (msg)
       (let ((flags (mu4e-msg-field msg :flags)))
         (when (and (memq 'unread flags) (not (memq 'trashed flags)))
           (incf count)))))
    count))

(defun mu4e~better-header-line-format ()
  `(:eval (mu4e~better-header-line)))

(advice-add 'mu4e~header-line-format :override #'mu4e~better-header-line-format)
;; Refresh server props?
(add-hook 'mu4e-index-updated-hook #'mu4e~start)

;;* mark-for-read dwim
(defun mu4e-headers-mark-for-read-dwim (mark-all)
  "Mark header at point with read. With prefix arg mark all unread instead."
  (interactive "P")
  (if mark-all
      ;; Code copied from `mu4e-headers-mark-all-unread-read' because I don't
      ;; want to load `mu4e-contrib'.
      (mu4e-headers-mark-for-each-if
       (cons 'read nil)
       (lambda (msg _param)
         (memq 'unread (mu4e-msg-field msg :flags))))
    (call-interactively #'mu4e-headers-mark-for-read)))

(define-key mu4e-headers-mode-map "!" 'mu4e-headers-mark-for-read-dwim)

(with-eval-after-load 'evil
  (evil-define-key '(normal) mu4e-headers-mode-map
    "!" 'mu4e-headers-mark-for-read-dwim))


(provide 'configure-email)
