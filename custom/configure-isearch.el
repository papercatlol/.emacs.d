(require 'anzu)
(require 'expand-region)
(global-anzu-mode 1)

;; a hacky way of combining expand-region and isearch
(advice-add 'isearch-forward-regexp :before 'push-mark)
(advice-add 'isearch-backward-regexp :before 'push-mark)
(advice-add 'isearch-forward :before 'push-mark)
(advice-add 'isearch-backward :before 'push-mark)

(defun isearch-expand (arg)
  (interactive "p")
  (activate-mark)
  (er/expand-region arg)
  (deactivate-mark)
  (setq isearch-string (buffer-substring (mark) (point)))
  (setq isearch-message isearch-string)
  (isearch-update))

(defun isearch-shrink (arg)
  (interactive "p")
  (isearch-expand (- arg)))


(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-S-r") 'anzu-query-replace-regexp)
(global-set-key (kbd "C-S-s") 'anzu-isearch-query-replace)
(global-set-key (kbd "M-%") 'anzu-query-replace-at-cursor)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
(global-set-key (kbd "M-o") 'occur)
(define-key isearch-mode-map (kbd "M-o") 'isearch-occur)
(define-key isearch-mode-map (kbd "C-.") 'isearch-expand)
(define-key isearch-mode-map (kbd "C-,") 'isearch-shrink)

(provide 'configure-isearch)
