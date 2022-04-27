;;;###autoload
(defun compilation-to-dired ()
  "Open files from compilation buffer in dired."
  (interactive)
  (unless (compilation-buffer-p (current-buffer))
    (error "Not in a compilation buffer"))
  (compilation--ensure-parse (point-max))
  (when-let ((files (compilation--collect-files))
             (buffer-name (funcall compilation-to-dired--name-generator)))
    (dired (cons buffer-name files))))

(defun compilation--collect-files ()
  (let ((files))
    (maphash (lambda (k v)
               (let* ((file (car k))
                      (dir (cdr k))
                      (filename (if dir
                                    (concat (file-name-as-directory dir) file)
                                  file)))
                 (when (file-exists-p filename)
                  (cl-pushnew filename files :test #'string=))))
             compilation-locs)
    (reverse files)))

(defvar compilation-to-dired--name-generator
  (lambda ()
    (generate-new-buffer-name (concat "dired: " (buffer-name (current-buffer))))))


(provide 'compilation-to-dired)
