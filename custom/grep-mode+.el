(require 'compile)
(require 'dired)
(require 'magit)


(defun compilation--collect-files ()
  "Collect matched files from compilation result buffer."
  (unless (compilation-buffer-p (current-buffer))
    (error "Not in a compilation buffer"))
  (save-excursion
    (goto-char (point-min))
    (loop for msg = (condition-case nil
                        (compilation-next-file 1)
                      (user-error nil))
          while msg
          when (compilation--message->loc msg)
          when (compilation--loc->file-struct it)
          for (file dir) = (car it)
          when (file-exists-p file)
          collect (string-remove-prefix "./"
                   (if dir (expand-file-name file dir) file)))))

(defun compilation-dired-files ()
  "Open a dired buffer with files from compilation result buffer."
  (interactive)
  (dired (cons (generate-new-buffer-name default-directory)
               (compilation--collect-files))))


(define-key compilation-minor-mode-map (kbd "C-c C-d") 'compilation-dired-files)
(define-key dired-mode-map (kbd "C-x g l") 'magit-log)
(define-key dired-mode-map (kbd "C-x g j") 'magit-dired-log)
