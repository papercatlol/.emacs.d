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

;;* C-k and C-w hacks
(defvar ghostel--kill-ring-update-timeout 0.1)
(defvar-local ghostel--kill-ring-update-timer nil)

(defun ghostel-C-k ()
  (interactive)
  (labels ((%rest-of-input (pos)
             (buffer-substring-no-properties pos (point-max)))
           (%kill-handler (pos input-old)
             (lambda ()
               (let ((input-new (%rest-of-input pos)))
                 (if (string-empty-p input-new)
                     (kill-new input-old)
                   (if-let ((idx (search input-new input-old :from-end t)))
                       (kill-new (substring input-old 0 idx))
                     (message "ghostel-C-k: failed to add text to kill-ring")))))))
    (let* ((pos (point))
           (input-old (%rest-of-input pos)))
      (ghostel--send-encoded "k" "ctrl")
      (run-with-timer ghostel--kill-ring-update-timeout nil
                      (%kill-handler pos input-old)))))

(define-key ghostel-semi-char-mode-map (kbd "C-k") 'ghostel-C-k)

(defun ghostel-C-w ()
  (interactive)
  (labels ((%rest-of-input ()
             (when-let* ((end ghostel--cursor-char-pos)
                         (beg (ghostel-input-start-point))
                         ((<= beg end)))
               (buffer-substring-no-properties beg end)))
           (%kill-handler (input-old appendp)
             (lambda ()
               (if-let* ((input-new (%rest-of-input))
                         (to-kill
                          (if (string-empty-p input-new)
                              input-old
                            ;; Assume that C-w just removes some suffix from
                            ;; INPUT-OLD.
                            (when (string-prefix-p input-new input-old)
                              (substring input-old (length input-new))))))
                   (if appendp
                       (kill-append to-kill 'before-p)
                     (kill-new to-kill))
                 (message "ghostel-C-w: failed to add text to kill-ring")))))

    (let ((input-old (%rest-of-input)))
      (ghostel--send-encoded "w" "ctrl")
      (when (and (stringp input-old) (not (string-empty-p input-old)))
        ;; HACK Buffer state isn't updated synchronously, so we schedule a
        ;; function to update kill-ring. This will fail miserably on tramp
        ;; connections with basically any amount of latency. I wonder if there
        ;; is a good way to sync this. Maybe a hook in the redraw function?
        (when (timerp ghostel--kill-ring-update-timer)
          (cancel-timer ghostel--kill-ring-update-timer))
        (setq ghostel--kill-ring-update-timer
              (run-with-timer ghostel--kill-ring-update-timeout nil
                              (%kill-handler input-old
                                             (eq 'ghostel-C-w last-command))))))))

(define-key ghostel-semi-char-mode-map (kbd "C-w") 'ghostel-C-w)

(evil-define-key 'insert ghostel-semi-char-mode-map
  (kbd "C-w") 'ghostel-C-w)


(provide 'configure-ghostel)
