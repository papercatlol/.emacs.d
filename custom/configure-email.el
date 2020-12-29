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
  )

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
(setq mu4e-alert-email-notification-types '(count))

(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)

(provide 'configure-email)
