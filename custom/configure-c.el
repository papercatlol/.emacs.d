;; -*- lexical-binding: t -*-

(define-key c-mode-map (kbd "C-c C-d") 'eldoc-display-full-doc)

(add-hook 'c-mode-hook #'electric-pair-local-mode)

(provide 'configure-c)
