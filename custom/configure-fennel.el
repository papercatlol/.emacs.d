;; -*- lexical-binding: t -*-

(add-hook 'fennel-mode-hook #'lispy-mode)
(add-hook 'fennel-repl-mode-hook #'lispy-mode)
(add-hook 'fennel-proto-repl-mode-hook #'lispy-mode)

(defun lispy-fennel-repl-init ()
  (with-minor-mode-map-overriding (map lispy-mode)
    (define-key map (kbd "M-.") nil)
    (define-key map (kbd "C-j") nil)))
(add-hook 'fennel-repl-mode-hook #'lispy-fennel-repl-init)
(add-hook 'fennel-proto-repl-mode-hook #'lispy-fennel-repl-init)

(setf (alist-get 'fennel-mode lispy-eval-alist)
      '(lispy lisp-eval-string))
(setf (alist-get 'fennel-repl-mode lispy-eval-alist)
      '(lispy lisp-eval-string))
(setf (alist-get 'fennel-proto-repl-mode lispy-eval-alist)
      '(lispy lisp-eval-string))

;;* eglot
(add-hook 'fennel-mode-hook #'eglot-ensure)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(fennel-mode . ("fennel-ls"))))

(provide 'configure-fennel)
