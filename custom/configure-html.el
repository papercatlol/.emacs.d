;; -*- lexical-binding: t -*-


(add-hook 'html-mode-hook #'electric-pair-local-mode)

;;* Loading skewer.el:
;; Put <script src="http://localhost:8080/skewer"></script> into your html file.
;; M-x httpd-serve-directory
;; M-x skewer-html-mode
(add-hook 'html-mode-hook 'skewer-html-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)

;;* tagedit
(with-eval-after-load 'sgml-mode
  (require 'tagedit)
  (tagedit-add-paredit-like-keybindings)
  (tagedit-add-experimental-features)
  (add-hook 'html-mode-hook (lambda () (tagedit-mode 1))))

;;* keys
(define-key html-mode-map (kbd "<return>") 'sgml-close-tag)

(provide 'configure-html)
