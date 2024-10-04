;; -*- lexical-binding: t -*-

;;* lsp
(defvar lsp-java-home "~/work/java-lsp/plugins/org.eclipse.equinox.launcher_1.6.100.v20201223-0822.jar")

(let ((class-path (getenv "CLASSPATH")))
  (when (and (file-exists-p lsp-java-home)
             (not (search lsp-java-home class-path)))
    (setenv "CLASSPATH" (if class-path
                            (format "%s:%s" class-path lsp-java-home)
                          lsp-java-home))))

(require 'configure-lsp)

;;(add-hook 'java-mode-hook 'configure-lsp:init)
(remove-hook 'java-mode-hook 'configure-lsp:init)

;;(add-hook 'java-mode-hook 'eglot-ensure)

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

;;* find-definition M-.
(with-eval-after-load 'lsp
  (define-key java-mode-map (kbd "M-.") 'lsp-find-definition))

;;* dap-mode
(with-eval-after-load 'dap-mode
  (define-key java-mode-map (kbd "C-c C-b") 'dap-hydra)
  (define-key java-mode-map (kbd "C-c C-z") 'dap-ui-repl)
  (define-key dap-ui-repl-mode-map (kbd "C-c C-z") 'other-window-backwards))

(provide 'configure-java)

