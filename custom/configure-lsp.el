;;; -*- lexical-binding: t -*-
(require 'lsp)

(defun configure-lsp:init ()
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil
        lsp-diagnostics-provider :none
        lsp-completion-provider :none
        lsp-eldoc-render-all nil ; TODO: enable this but disable markup in minibuffer
        lsp-keymap-prefix "C-c C-a"
        )
  (when (fboundp 'which-key)
    (lsp-enable-which-key-integration))
  (yas-minor-mode)
  (lsp))


(provide 'configure-lsp)
