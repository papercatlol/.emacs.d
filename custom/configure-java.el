;; -*- lexical-binding: t -*-

(require 'configure-lsp)
(require 'lsp-java)

(setq lsp-java-server-install-dir "~/work/java-lsp/")

(add-to-list 'lsp-enabled-clients 'jdtls)


(add-hook 'java-mode-hook 'configure-lsp:init)


(provide 'configure-java)

