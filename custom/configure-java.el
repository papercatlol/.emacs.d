;; -*- lexical-binding: t -*-

(require 'configure-lsp)
(require 'lsp-java)

(setq lsp-java-server-install-dir "~/work/java-lsp/")

(add-to-list 'lsp-enabled-clients 'jdtls)


(add-hook 'java-mode-hook 'configure-lsp:init)

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

