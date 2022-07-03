;; Based on denote sample configuration (see denote/README.org)

;;* general
(require 'denote)

(setq denote-directory (expand-file-name "~/org/denote/"))
(setq denote-known-keywords '("emacs" "linux"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
(setq denote-file-type nil)
(setq denote-allow-multi-word-keywords t)
(setq denote-front-matter-date-format nil)


;; TODO remove requires after denote official release
(require 'denote-retrieve)
(require 'denote-link)

(setq denote-link-use-org-id t)

;; By default, we fontify backlinks in their bespoke buffer.
(setq denote-link-fontify-backlinks t)


;;* denote-dired
(require 'denote-dired)

(setq denote-dired-rename-expert nil)
(setq denote-dired-directories (list denote-directory))

(add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

(defun denote-dired-update-dired-buffers* (&rest _)
  (dired-fun-in-all-buffers (denote-directory) nil #'revert-buffer))

(defun denote-dired-vc-register (file &rest _)
  (with-current-buffer (find-file-noselect file)
    (vc-register)))

(setq denote-dired-post-rename-functions
      (list #'denote-dired-update-dired-buffers*
            #'denote-dired-rewrite-front-matter
            #'denote-dired-vc-register))

;; MAYBE denote-dired-do-rename-dwim: when renaming a note file, call
;; `denote-dired-rename-file', else call `dired-do-rename'.
(defun denote-dired-rename-file-dwim (file title keywords)
  "Like `denote-dired-rename-file', but use old title/keywords as
initial input."
  (interactive
   (when-let ((file (denote-dired--rename-file-is-regular
                     (denote-dired--rename-dired-file-or-prompt))))
    (list
     file
     (setq denote-last-title
           (read-string "File title: " (denote-retrieve--value-title file)
                        'denote--title-history))
     ;; Copy-paste from `denote--keywords-crm' & `denote--keywords-prompt'.
     (let ((kw (completing-read-multiple
                "File keyword: " (denote-keywords)
                nil nil
                (replace-regexp-in-string "_" ","
                                          (denote--extract denote--file-regexp file 6))
                'denote--keyword-history)))
       (setq denote-last-keywords
             (cond
               ((null kw) "")
               ((= (length kw) 1) (car kw))
               (t (if denote-sort-keywords
                      (sort kw #'string-lessp)
                    kw))))))))
  (denote-dired-rename-file file title keywords))

;;* denote-dwim
(defun denote-dwim--notes-alist ()
  "Return alist (PRETTY-NOTE-NAME . PATH). We use #keyword notation
for denote keywords for easy keyword-based filtering."
  (let ((notes nil)
        (padding (make-string 5 ?\t)))
    (dolist (filename (denote--directory-files t))
      (when (string-match denote--file-regexp filename)
        (let (;; Sort by date?
              ;;(date (match-string 1 filename))
              ;;(time (match-string 2 filename))
              (title (match-string 4 filename))
              (keywords
                (string-join (mapcar (lambda (x) (concat "#" x))
                                     (split-string (match-string 6 filename) "_"))
                             " ")))
          (push (cons (concat title padding keywords) filename) notes)
          )))
    (cl-sort notes #'string-lessp :key #'car)))

(defun denote-dwim--completing-read (&optional prompt)
  "Prompt for a note from a pretty-printed list of notes with
#keywords. Return (USER-INPUT . EXISTING-NOTE)."
  (let* ((notes-alist (denote-dwim--notes-alist))
         (str (completing-read (or prompt "Denote: ") notes-alist
                               nil nil nil 'denote-dwim-history))
         (existing-note (alist-get str notes-alist nil nil #'equalp)))
    (cons str existing-note)))

(defvar denote-dwim-history nil)

(defun denote-dwim ()
  "Prompt for a note to visit or create a new one. When creating a
new note, #keyword notation can be used to populate the keywords
list. If no keywords were provided this way, prompt for keywords
normally. With prefix argument, prompt for file type first."
  (interactive)
  (let ((denote-file-type (if current-prefix-arg
                              (denote--file-type-symbol
                               (denote--file-type-prompt))
                            denote-file-type)))
    (destructuring-bind (str . existing-note) (denote-dwim--completing-read)
     (if existing-note
         (find-file existing-note)
       (destructuring-bind (title &rest keywords)
           (mapcar #'string-trim (split-string str "#"))
         ;; For some reason denote treats single and multiple keywords
         ;; differently.
         (unless (cdr keywords) (setq keywords (car keywords)))
         (denote title (or keywords (denote--keywords-prompt))))))))

;;* automatically add new notes to vc
(defun denote--vc-register-after (&rest _)
  (save-buffer)
  (vc-register))

(advice-add 'denote :after #'denote--vc-register-after)

;;* hydra-denote
(defhydra hydra-denote (:exit t :columns 1)
  "Denote"
  ("<f6>" #'denote-dwim "DWIM")
  ("l" #'denote-link "Add link")
  ("f" #'denote-link-find-file "Prompt for a linked note to visit")
  ("b" #'denote-link-backlinks "Show backlinks for current note")
  ("a" #'denote-link-add-links "Add links matching regexp")
  ("d" #'denote-directory-dired "Visit denote-directory in Dired")
  ("r" #'denote-dired-rename-file-dwim "Rename"))

(global-set-key (kbd "<f6>") 'hydra-denote/body)

(defun denote-directory-dired ()
  "Visit `denote-directory' in Dired."
  (interactive)
  (dired (denote-directory)))


;;* migrating from org-roam
(defun org-roam-convert-to-denote (files dir)
  "Convert org-roam notes to denote format and insert them into
`denote-directory'. Works in Dired."
  (interactive
   (list (dired-get-marked-files t current-prefix-arg nil nil t)
         (read-directory-name "Denote directory: " (denote-directory))))
  (unless (file-exists-p dir) (error "Directory does not exist: %s" dir))

  (dolist (file files)
    (let* ((filename (file-name-base file))
           (org-roam-filename-regex
             (rx
              ;; date: YYYY-MM-DD
              (group (= 4 digit) "-" (= 2 digit) "-" (= 2 digit))
              "_"
              ;; time: HH:MM
              (group (= 2 digit) ":" (= 2 digit))
              "-"
              ;; title
              (group (* any))))
           (match? (string-match org-roam-filename-regex filename)))
      (unless match?
        (warn "Filename doesn't match org-roam-filename-regex: %s" filename))

      (let* ((date (match-string 1 filename))
             (time (match-string 2 filename))
             (title (or ;; Try to get title with spaces.
                        (denote-retrieve--value-title file)
                        (replace-regexp-in-string "_" " " (match-string 3 filename))))
             (decoded-time (date-to-time (concat date "T" time)))
             (id (format-time-string denote--id-format decoded-time))
             ;; Code from `org-roam-tag-add'.
             (keywords (with-temp-buffer (insert-file-contents file nil 0 1024)
                         (split-string (or (cadr (assoc "FILETAGS"
                                                        (org-collect-keywords '("filetags"))))
                                           "")
                                       ":" 'omit-nulls)))
             (keywords (denote--sluggify-keywords
                        (if (cdr keywords) keywords (car keywords))))
             (new-name (denote--format-file
                        (denote-directory)
                        id
                        keywords
                        (denote--sluggify title)
                        (denote--file-extension))))
        ;; Ask for confirmation when overwriting, but don't throw an error if
        ;; the user declines.
        (ignore-errors (copy-file file new-name 1))

        (with-current-buffer (find-file-noselect new-name t)
          (save-excursion
           (goto-char (point-min))
           (org-set-property "ID" id)
           ;; Fix keywords: org-roam uses default org :tag1:tag2: format, denote
           ;; uses space-separated format.
           ;; Code from `org-roam-set-keyword'.
           (org-roam-set-keyword "filetags" (string-join (ensure-list keywords) " "))
           (org-roam-set-keyword "date" (denote--date decoded-time))

           ;; Ensure newline before first heading as this is how denote looks
           ;; for front matter ending.
           (goto-char (point-min))
           (outline-next-visible-heading 1)
           (unless (looking-back "\n\n")
             (insert "\n"))

           (let ((inhibit-message t))
             (save-buffer))))
        (message "Converted %s -> %s" file new-name))))
  (denote-dired-update-dired-buffers))


(provide 'configure-denote)
