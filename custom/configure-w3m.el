;; -*- lexical-binding: t -*-

(require 'w3m-load)
(require 'w3m)


;; Always ask for an url.
(setq w3m-quick-start nil)
;; (setq w3m-process-inhibit-quit nil)

;;* never pop up (or switch to) frames
(advice-add 'w3m-popup-buffer :override #'pop-to-buffer)

;;(setf (alist-get (rx "*w3m" (* any)) display-buffer-alist nil nil #'equal)
;;      '(display-buffer-same-window
;;        (reusable-frames . nil)
;;        (inhibit-switch-frame . t)))

;;* ivy-actions
(ivy-add-actions
 'counsel-find-file
 '(("W" w3m-find-file "open with w3m")))

;;* link-hint
;; Hack link-hint-w3m-link to allow us to bind the url opening function.
(defvar link-hint-w3m-open-function #'w3m-view-this-url)

(defun link-hint-w3m-open ()
  (funcall link-hint-w3m-open-function))

(setf (get 'link-hint-w3m-link :open)
      #'link-hint-w3m-open)

(defun link-hint-w3m-goto-url-new-session ()
  "Use avy to open a visible url in a new w3m buffer."
  (interactive)
  (let ((link-hint-w3m-open-function #'w3m-view-this-url-new-session))
    (call-interactively #'link-hint-open-link)))

(define-key w3m-mode-map (kbd "f") 'link-hint-open-link)
(define-key w3m-mode-map (kbd "F") 'link-hint-w3m-goto-url-new-session)
(define-key w3m-mode-map (kbd "C-f") 'link-hint-w3m-goto-url-new-session)
(define-key w3m-mode-map (kbd "C-c C-SPC") nil)

;;* tabs (aka buffers or sessions - wtf is this terminology bruh)
(defun w3m-close-tab ()
  "Forward to `w3m-delete-buffer', but don't prompt for
confirmation if this is there is only one tab open."
  (interactive)
  (w3m-delete-buffer t))

(define-key w3m-mode-map (kbd "C-{") 'w3m-previous-buffer)
(define-key w3m-mode-map (kbd "C-}") 'w3m-next-buffer)
(define-key w3m-mode-map (kbd "<C-S-iso-lefttab>") 'w3m-previous-buffer)
(define-key w3m-mode-map (kbd "<C-tab>") 'w3m-next-buffer)
(define-key w3m-mode-map (kbd "C-w") 'w3m-close-tab)
(define-key w3m-mode-map (kbd "C-t") 'w3m-goto-url-new-session)

;;* history
(define-key w3m-mode-map (kbd "H") 'w3m-view-previous-page)
(define-key w3m-mode-map (kbd "q") 'w3m-view-previous-page)
(define-key w3m-mode-map (kbd "L") 'w3m-view-next-page)
(define-key w3m-mode-map (kbd "C-c h") 'w3m-history)

;;* general
(define-key w3m-mode-map (kbd "M-k") nil)
(define-key w3m-mode-map (kbd "C-q") 'bury-buffer)
(define-key w3m-mode-map (kbd "Q") 'w3m-close-window)
(define-key w3m-mode-map (kbd "C-c C-q") 'w3m-quit)
(define-key w3m-mode-map (kbd "g") 'w3m-reload-this-page)
(define-key w3m-mode-map (kbd "G") 'w3m-goto-url)
(define-key w3m-mode-map (kbd "b") 'backward-word)
(define-key w3m-mode-map (kbd "w") 'forward-word)
(define-key w3m-mode-map (kbd "C-x M-w") 'w3m-print-this-url)
(define-key w3m-mode-map (kbd "M-[") nil)
(define-key w3m-mode-map (kbd "M-]") nil)

;;* evil
(with-eval-after-load 'evil
  (evil-collection-w3m-setup)
  (evil-set-initial-state 'w3m-mode 'emacs))


(provide 'configure-w3m)
