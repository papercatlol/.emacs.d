(require 'go-mode)
(require 'lsp)
(require 'lsp-ui)
;; (require 'dap-mode)


(defun go-lsp-init ()
  (setq lsp-ui-sideline-enable t
        lsp-ui-doc-enable nil)
  (lsp)
  (yas-minor-mode))

(add-hook 'go-mode-hook 'go-lsp-init)


(provide 'configure-go-lsp)
