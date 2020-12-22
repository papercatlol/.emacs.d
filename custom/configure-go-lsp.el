;;; -*- lexical-binding: t -*-

(require 'configure-lsp)
(require 'configure-go)
(require 'lsp-show-doc)
;; (require 'dap-mode)



(defun go-lsp-init ()
  (when-let ((lsp-bin (go-lsp--get-gopls-binary)))
    (message "Found gopls bin: %s" lsp-bin)
    (setq lsp-gopls-server-path lsp-bin
          lsp-gopls-experimental-complete-unimported t)
    (configure-lsp:init)))

(defun go-lsp--get-gopls-binary ()
  (let* ((gopath (shell-command-to-string "go env GOPATH"))
         (gopath (file-name-as-directory (string-trim-right gopath "\n")))
         (gopls (concat gopath "bin/gopls")))
    (when (file-exists-p gopls)
      gopls)))

(define-key go-mode-map (kbd "C-c C-d") 'lsp-show-doc)

(add-hook 'go-mode-hook 'go-lsp-init)


(provide 'configure-go-lsp)
