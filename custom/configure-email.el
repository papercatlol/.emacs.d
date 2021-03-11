;; -*- lexical-binding: t -*-

;;* Installation:
;; https://hobo.house/2017/07/17/using-offlineimap-with-the-gmail-imap-api/
;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(require 'mu4e)

;;* evil bindings. Some don't work though...
(with-eval-after-load 'evil
  (require 'evil-collection)
  (evil-collection-mu4e-setup)
  (define-key mu4e-view-mode-map "j" 'next-line)
  (define-key mu4e-view-mode-map "k" 'previous-line)
  (define-key mu4e-view-mode-map "V" 'evil-visual-line)
  (define-key mu4e-view-mode-map "v" 'evil-visual-char-or-expand-region)
  (define-key mu4e-view-mode-map (kbd "C-c v") 'mu4e-view-verify-msg-popup)
  (define-key mu4e-headers-mode-map "H" 'mu4e-headers-query-prev)
  (define-key mu4e-headers-mode-map "L" 'mu4e-headers-query-next)
  (define-key mu4e-headers-mode-map (kbd "C-=") 'mu4e-headers-split-view-grow)
  (define-key mu4e-view-mode-map (kbd "C-=") 'mu4e-headers-split-view-grow)

  (evil-define-key '(normal) mu4e-view-mode-map
    "[" 'mu4e-view-headers-prev-unread
    "]" 'mu4e-view-headers-next-unread)

  (evil-define-key '(normal) mu4e-headers-mode-map
    "[" 'mu4e-headers-prev-unread
    "]" 'mu4e-headers-next-unread))

;;* general
;; A lot of these are taken from
;; https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/
(setq mu4e-view-prefer-html t
      mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include nil
      mu4e-compose-format-flowed t)
(setq mu4e-confirm-quit nil)

;; to view selected message in the browser, no signin, just html mail
(add-to-list 'mu4e-headers-actions
             '("Browser" . mu4e-action-view-in-browser) t)

(add-to-list 'mu4e-view-actions
             '("Browser" . mu4e-action-view-in-browser) t)

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

;;from the info manual
(setq mu4e-attachment-dir "/home/il/Downloads/")

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
(setq mu4e-update-interval 180)
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
	    (mu4e-sent-folder . "/home/il/mail/Papercatlol/Sent Mail")
	    (mu4e-drafts-folder . "/home/il/mail/Papercatlol/Drafts")
	    (mu4e-trash-folder . "/home/il/mail/Papercatlol/Trash")
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

;;* larger font in mu4e buffers
(defun text-scale-increase-1 ()
  (text-scale-increase 1))

(add-hook 'mu4e-view-mode-hook #'text-scale-increase-1)
(add-hook 'mu4e-headers-mode-hook #'text-scale-increase-1)
(add-hook 'mu4e-compose-mode-hook #'text-scale-increase-1)

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


(provide 'configure-email)
