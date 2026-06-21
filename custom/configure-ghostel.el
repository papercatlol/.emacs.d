;; -*- lexical-binding: t -*-
(require 'ghostel)

(defun ghostel--mode-init ()
  ;;(use-local-map ghostel-line-mode-map)
  ;;(setq ghostel--input-mode 'line)
  ;;(setq ghostel--mode-line-tag ":Line")
  (evil-insert-state)
  (setq-local shell-change-dir-function 'ghostel-change-dir))

(add-hook 'ghostel-mode-hook #'ghostel--mode-init)

(cl-defun ghostel--start-process-after (&rest _)
  ;; HACK because we need to wait for the ghostty process..
  (sleep-for 0.1)
  (loop
   (condition-case err
       (progn (ghostel-line-mode 'force)
              (return-from 'ghostel--start-process-after))
     (user-error (sleep-for 0.1)))))

(advice-add 'ghostel-line-mode :after #'evil-insert-state)
(advice-add 'ghostel--start-process :after #'ghostel--start-process-after)

;;* don't move point when anchoring the window in line-mode
;; Still doesn't work if both the point and input can't fit on the screen at the
;; same time.
(defun ghostel--anchor-window-restore-point (fn &optional window)
  (let* ((window (or window (selected-window)))
         (buf (window-buffer window))
         (input-mode (buffer-local-value 'ghostel--input-mode buf))
         (saved-point (when (eq 'line input-mode) (window-point window))))
    (prog1 (funcall fn window)
      (when (and saved-point (/= saved-point (window-point window)))
        ;;(message "restored point %s" saved-point)
        (set-window-point window saved-point)
        (with-current-buffer buf
          (goto-char saved-point))))))

(advice-add 'ghostel--anchor-window :around
            #'ghostel--anchor-window-restore-point)


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
