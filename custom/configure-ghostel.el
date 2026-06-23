;; -*- lexical-binding: t -*-
(add-to-list 'load-path (expand-file-name "ghostel/lisp" package-user-dir))
(require 'ghostel)

(defun ghostel--mode-init ()
  (setq-local shell-change-dir-function 'ghostel-change-dir))

(add-hook 'ghostel-mode-hook #'ghostel--mode-init)

(advice-add 'ghostel-line-mode :after #'evil-insert-state)

(setq ghostel-initial-input-mode 'line)

;;* general keybindings
(define-key ghostel-semi-char-mode-map (kbd "C-s") 'swiper-dwim)

;;* history completion
(defun ghostel-history-completion ()
  (interactive)
  (let ((initial-input
          (when-let ((beg ghostel--line-input-start))
            (buffer-substring-no-properties
             beg (or ghostel--line-input-end (point-max))))))
    (let ((history (append ghostel--line-mode-history (bash-history))))
      (insert
       (completing-read
        "History: " history nil t initial-input
        bash-history-completion-history)))))

(define-key ghostel-mode-map (kbd "M-r") 'ghostel-history-completion)

(add-to-list 'savehist-additional-variables 'ghostel--line-mode-history)

;;* ghostel-change-dir
(defun ghostel-change-dir (dir)
  (interactive (list (read-directory-name "Change directory: "
                                          default-directory default-directory t)))
  (let ((input-text (ghostel--line-mode-input-text)))
    (when input-text (ghostel--line-mode-delete-input))
    (ghostel-send-string (format "cd \"%s\"" (expand-file-name dir)))
    (ghostel-send-key "return")
    (when input-text (ghostel--line-mode-replace-input input-text))))

(define-key ghostel-mode-map (kbd "C-c C-d") 'ghostel-change-dir)
(define-key ghostel-mode-map (kbd "C-c d") 'shell-sync-dir-to-other-window)


(provide 'configure-ghostel)
