;; -*- lexical-binding: t -*-

;;* eglot (lsp)
(add-hook 'java-mode-hook 'eglot-ensure)

(with-eval-after-load 'eglot
  (define-key java-mode-map (kbd "C-c C-c") 'eglot-code-actions)
  (define-key java-mode-map (kbd "C-c M-q") 'eglot-rename)
  (define-key java-mode-map (kbd "C-c =") 'eglot-format)
  (define-key java-mode-map (kbd "C-c C-d") 'eldoc)
  )

;;* electric-pairs
(add-hook 'java-mode 'electric-pair-local-mode)

;;* swap C-c C-s and C-c s
(define-key java-mode-map (kbd "C-c C-s") 'counsel-imenu-dwim)
(define-key java-mode-map (kbd "C-c s") 'c-show-syntactic-information)

;;* TAB
(define-key java-mode-map (kbd "TAB") 'indent-for-tab-command)


;;* dap-mode
(define-key java-mode-map (kbd "C-c C-b") 'dap-hydra/body)

(provide 'configure-java)

