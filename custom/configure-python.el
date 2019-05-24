;;; -*- lexical-binding: t -*-

(defun elpy-init ()
  (elpy-enable))
(add-hook 'python-mode-hook 'elpy-init)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(setq python-shell-interpreter "ipython3.6"
      elpy-rpc-python-command "python3.6"
      python-shell-interpreter-args "-i --simple-prompt"
      python-indent-guess-indent-offset-verbose nil)

(setq elpy-modules '(elpy-module-sane-defaults
                     elpy-module-eldoc
                     elpy-module-highlight-indentation
                     elpy-module-pyvenv
                     elpy-module-yasnippet))


;;* `defuns'
(defun elpy-shell-send-region-or-top-statement()
  (interactive)
  (call-interactively
   (if (region-active-p)
       #'elpy-shell-send-region-or-buffer
     #'elpy-shell-send-top-statement)))

(require 'popup)

(defun elpy-doc--popup (documentation)
  "Show DOCUMENTATION in a popup."
  (popup-tip documentation
             :truncate nil
             :height 60))

(advice-add #'elpy-doc--show :override #'elpy-doc--popup)


;;* `keys'
(with-eval-after-load elpy-mode
  (define-key elpy-mode-map (kbd "C-c C-k") 'elpy-shell-send-region-or-buffer)
  (define-key elpy-mode-map (kbd "C-c C-c") 'elpy-shell-send-region-or-top-statement)
  )


(provide 'configure-python)
