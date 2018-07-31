(require 'ivy)
(require 'ivy-hydra)
(require 'counsel)

(ivy-mode t)
(setq ivy-use-virtual-buffers t
      ivy-count-format "[%d/%d] ")

(defun ivy-add-prompt-count* (next-fn prompt)
  "fix alignment of current match number"
  (if (string-match "%.*d" ivy-count-format)
      (concat ivy-count-format prompt)
    (funcall next-fn prompt)))

(advice-add 'ivy-add-prompt-count :around #'ivy-add-prompt-count*)

;; counsel-files
(defcustom counsel-files-base-cmd "rg --files"
  "command to list files in the project"
  :type 'string
  :group 'ivy)

(defun counsel-files-occur ()
  "Occur function for `counsel-files' using `counsel-cmd-to-dired'."
  (cd (ivy-state-directory ivy-last))
  (counsel-cmd-to-dired
   (counsel--expand-ls
    (format "%s %s | xargs -d \"\\n\" ls"
            counsel-files-base-cmd
            (if (string= "" ivy--old-re)
                ""
              (format "| grep -i -E '%s'" (counsel-unquote-regex-parens ivy--old-re)))))
   ;; set dired properties(e.g. for dired-hide-details-mode)
   (lambda (proc string)
     (let ((inhibit-read-only t)
           (point (point)))
       (insert string "\n")
       (dired-insert-set-properties point (point))))))

(ivy-set-occur 'counsel-files 'counsel-files-occur)

(defun counsel-files (&optional initial-input initial-directory)
  "If in a vc repository, list all tracked files.
Else list all files within the `default-directory' or any of its subdirectories.
With prefix arg prompt for INITIAL-DIRECTORY."
  (interactive
   (list nil
         (when current-prefix-arg
           (read-directory-name "From directory: "))))
  (counsel-require-program (car (split-string counsel-files-base-command " ")))
  (let* ((default-directory (expand-file-name (or initial-directory
                                                  (vc-root-dir)
                                                  (locate-dominating-file default-directory ".git")
                                                  default-directory))))
    (ivy-read "Find file: "
              (split-string
               (shell-command-to-string counsel-files-base-command)
               "\n" t)
              :matcher #'counsel--find-file-matcher
              :initial-input initial-input
              :action (lambda (x)
                        (with-ivy-window
                          (find-file (expand-file-name x ivy--directory))))
              :preselect (counsel--preselect-file)
              :require-match 'confirm-after-completion
              :history 'file-name-history
              :keymap counsel-find-file-map
              :caller 'counsel-files)))

(defun swiper-at-point ()
  (interactive)
  (swiper (cond ((region-active-p)
                 (buffer-substring (point) (mark)))
                ((symbol-at-point)
                 (symbol-name (symbol-at-point))))))

(defun ivy-yank-symbol-at-point ()
  (interactive)
  (let ((symbol))
    (with-ivy-window
      (setq symbol (symbol-at-point)))
    (when symbol
      (insert (symbol-name symbol)))))


(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper-at-point)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x f") 'counsel-files)
(global-set-key (kbd "C-x d") 'counsel-dired-jump)
(global-set-key (kbd "C-x C-/") 'counsel-rg)

(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h l") 'counsel-find-library)

(define-key swiper-map (kbd "C-:") 'swiper-mc)
(define-key swiper-map (kbd "C-t") 'swiper-avy)
(define-key ivy-minibuffer-map (kbd "C-,") 'ivy-yank-symbol-at-point)
(define-key ivy-minibuffer-map (kbd "C-.") 'ivy-yank-word)
(define-key ivy-minibuffer-map (kbd "M-o") 'ivy-occur)



(provide 'configure-ivy)
