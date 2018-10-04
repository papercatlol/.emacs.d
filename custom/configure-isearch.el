(require 'anzu)
(require 'expand-region)
(require 'loccur)
(global-anzu-mode 1)

(defun occur-more-context (arg)
  (interactive "p")
  (let ((nlines (+ arg (second occur-revert-arguments))))
    (setf (second occur-revert-arguments) (if (minusp nlines) 0 nlines))
    (revert-buffer)))

(defun occur-less-context (arg)
  (interactive "p")
  (occur-more-context (- arg)))

(defun occur-no-context ()
  (interactive)
  (setf (second occur-revert-arguments) 0)
  (revert-buffer))

(defun occur-show-next ()
  (interactive)
  (occur-next)
  (occur-mode-display-occurrence))

(defun occur-show-prev ()
  (interactive)
  (occur-prev)
  (occur-mode-display-occurrence))

(global-set-key (kbd "M-%") 'anzu-query-replace-at-cursor)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
(global-set-key (kbd "M-o") 'occur)
(global-set-key (kbd "C-x M-o") 'loccur)
(define-key occur-mode-map (kbd "n") 'occur-show-next)
(define-key occur-mode-map (kbd "p") 'occur-show-prev)
(define-key occur-mode-map (kbd "+") 'occur-more-context)
(define-key occur-mode-map (kbd "-") 'occur-less-context)
(define-key occur-mode-map (kbd "=") 'occur-no-context)



(provide 'configure-isearch)
