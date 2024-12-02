;;; -*- lexical-binding: t -*-
(require 'go-mode)

(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'electric-pair-local-mode)

;;* keys
(define-key go-mode-map (kbd "M-.") 'xref-find-definitions)
(define-key go-mode-map (kbd "C-c C-c") 'eglot-code-actions)
(define-key go-mode-map (kbd "C-c t") 'eglot-find-typeDefinition)
(define-key go-mode-map (kbd "C-c C-r") 'eglot-rename)
(define-key go-mode-map (kbd "C-c C-d") 'eldoc-display-full-doc)

;; TODO link-hint support for markdown links in lsp documentation.


(provide 'configure-go-lsp)
