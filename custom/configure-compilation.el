;; -*- lexical-binding: t -*-


(with-eval-after-load 'compile
  (setq next-error-message-highlight t)

  (defun compilation-display-next-error (n)
    (interactive "p")
    (compilation-next-error n)
    (compilation-display-error))

  (defun compilation-display-previous-error (n)
    (interactive "p")
    (compilation-display-next-error (- n)))

  (with-eval-after-load 'ace-link
    (setq ace-link--compilation-action-fn #'compilation-display-error))

  (define-key compilation-mode-map (kbd "<tab>") 'compilation-next-error)
  (define-key compilation-mode-map (kbd "o") 'compilation-display-error)
  (define-key compilation-mode-map (kbd "m") 'compilation-display-error)
  (define-key compilation-mode-map (kbd "M-m") 'compilation-display-error)
  (define-key compilation-mode-map (kbd "C-j") 'compilation-display-next-error)
  (define-key compilation-mode-map (kbd "C-k") 'compilation-display-previous-error)
  (define-key compilation-mode-map (kbd "k") 'compilation-previous-error)
  (define-key compilation-mode-map (kbd "j") 'compilation-next-error)
  (define-key compilation-mode-map (kbd "h") 'backward-char)
  (define-key compilation-mode-map (kbd "l") 'forward-char))


(with-eval-after-load 'comint
  (add-hook 'comint-output-filter-functions #'comint-osc-process-output)

  (defun comint-kill-output (&optional delete)
    "Inverse command to `comint-delete-output'."
    (interactive "P")
    (comint-delete-output (not delete)))

  (define-key comint-mode-map [remap comint-delete-output] 'comint-kill-output)
  )


(provide 'configure-compilation)
