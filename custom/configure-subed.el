;; -*- lexical-binding: t -*-

(setq subed-auto-play-media nil)

(define-key subed-mode-map (kbd "M-j") nil)
(define-key subed-mode-map (kbd "M-k") nil)
(define-key subed-mode-map (kbd "C-c C-k") 'subed-kill-subtitle)
(define-key subed-mode-map (kbd "M-m") 'subed-mpv-jump-to-current-subtitle)
(define-key subed-mode-map (kbd "C-c m") 'subed-merge-dwim)
(define-key subed-mode-map (kbd "C-c M") 'subed-merge-with-previous)


(provide 'configure-subed)
