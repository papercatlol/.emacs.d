;;; -*- lexical-binding: t -*-

(require 'lsp)
(require 'lsp-ui)
(require 'popup)

(defun lsp-show-doc ()
  (interactive)
  "Query documentation from LSP and display it in the minibuffer"
  (when-let* ((resp (lsp-request "textDocument/hover" (lsp--text-document-position-params)))
              (doc (lsp-inline-doc--render-contents (gethash "contents" resp))))
    (let ((max-mini-window-height 1.0))
      (message (substring-no-properties doc)))))

(defun lsp-inline-doc--render-contents (contents)
  (lsp--render-element contents)
  ;; (gethash "value" contents)
  )

(provide 'lsp-show-doc)
