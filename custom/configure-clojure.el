;;; -*- lexical-binding: t -*-

(require 'clojure-mode)
(require 'inf-clojure)
(require 'cider)


;;* cider
(setq cider-repl-history-file "~/.emacs.d/cider-history")
(setq cider-repl-use-pretty-printing t)
(setq cider-repl-use-clojure-font-lock t)
(setq cider-repl-result-prefix ";; => ")
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 3000)
;; (setq cider-show-error-buffer nil) ; TODO figure out

;;** repl
(add-hook 'cider-repl-mode-hook 'lispy-mode)
(add-to-list 'lispy-no-indent-modes 'cider-repl-mode)

;;** [M-r] cider-repl-complete-ivy
(defun cider-repl-complete-ivy ()
  "Complete input from repl history."
  (interactive)
  (let ((initial-input (cider-repl--current-input)))
    (ivy-read "REPL search: " cider-repl-input-history
              :initial-input initial-input
              :action (lambda (text)
                        (cider-repl-delete-current-input)
                        (insert text)))))

(define-key cider-repl-mode-map (kbd "M-r") 'cider-repl-complete-ivy)

;;TODO:* clj-refactor

;;* eldoc
(add-hook 'clojure-mode-hook #'eldoc-mode)

;;* evil
(when (and (fboundp 'evil-mode) (fboundp 'cider-mode))
  (evil-set-initial-state 'cider-repl-mode 'insert))

;;* clojurescript
(add-hook 'clojurescript-mode-hook 'cider-mode)
(add-hook 'clojurescript-mode-hook 'lispy-mode)

(provide 'configure-clojure)
