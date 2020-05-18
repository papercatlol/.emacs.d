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

(defun occur-save-buffers ()
  (interactive)
  (save-excursion
   (let ((bufs nil))
     (loop
       for pos = (next-single-property-change
                  (or pos (point-min))
                  'occur-target
                  nil
                  (point-max))
       until (= pos (point-max))
       when (get-pos-property pos 'occur-target)
         do (when-let* ((buf (marker-buffer it))
                        (modified? (buffer-modified-p buf)))
              (pushnew buf bufs)))
     (if (null bufs)
         (message "Nothing to save.")
       (dolist (buf bufs)
         (with-current-buffer buf
           (save-buffer)))
       (message "Saved %d buffer(s)." (length bufs))))))

(defun occur-cease-edit-and-save ()
  (interactive)
  (occur-cease-edit)
  (occur-save-buffers))

(global-set-key (kbd "M-%") 'anzu-query-replace-at-cursor)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
;; (global-set-key (kbd "M-o") 'occur)
(global-set-key (kbd "C-x C-o") 'occur)
(global-set-key (kbd "C-x M-o") 'loccur)
(define-key occur-mode-map (kbd "n") 'occur-show-next)
(define-key occur-mode-map (kbd "p") 'occur-show-prev)
(define-key occur-mode-map (kbd "+") 'occur-more-context)
(define-key occur-mode-map (kbd "-") 'occur-less-context)
(define-key occur-mode-map (kbd "=") 'occur-no-context)
(define-key occur-mode-map (kbd "j") 'next-line)
(define-key occur-mode-map (kbd "k") 'previous-line)
(define-key occur-mode-map (kbd "C-j") 'occur-show-next)
(define-key occur-mode-map (kbd "C-k") 'occur-show-prev)
(define-key occur-mode-map (kbd "C-x C-q") 'occur-edit-mode)
(define-key occur-mode-map (kbd "C-x C-s") 'occur-save-buffers)
(define-key occur-edit-mode-map (kbd "C-x C-q") 'occur-cease-edit)
(define-key occur-edit-mode-map (kbd "C-x C-s") 'occur-cease-edit-and-save)




(provide 'configure-isearch)
