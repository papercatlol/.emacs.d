(defgroup generated-hl-faces nil
  "Generated fonts for highlight"
  :group 'extensions
  :group 'convenience
  :prefix "hl-")

(cl-defmacro make-hl-face (&key fg bg)
  (unless (eq fg bg)
    (let* ((prefix "hl-")
           (group 'generated-hl-faces)
           (fg-string (or fg "default"))
           (bg-string (or bg "default"))
           (facename-string (format "%s%s-%s" prefix fg-string bg-string))
           (facename (intern facename-string))
           (docstring (format "A generated face for hl-mode: %s on %s" fg-string bg-string))
           (attrs (append (and fg (list :foreground fg))
                          (and bg (list :background bg)))))
      `(progn
         (unless (facep ',facename)
           (defface ,facename
               '((t (:foreground ,fg :background ,bg :weight bold)))
             ,docstring
             :group ',group))
         (pushnew ,facename-string hi-lock-face-defaults :test #'equal)))))

(setq hi-lock-face-defaults nil)

(make-hl-face :fg "white" :bg "maroon")
(make-hl-face :fg "white" :bg "red")
(make-hl-face :fg "white" :bg "DarkOrange")
(make-hl-face :fg "white" :bg "gold")
(make-hl-face :fg "white" :bg "magenta")
(make-hl-face :fg "white" :bg "sienna")
(make-hl-face :fg "white" :bg "DarkGreen")
(make-hl-face :fg "white" :bg "LimeGreen")
(make-hl-face :fg "white" :bg "tomato")
(make-hl-face :fg "white" :bg "blue")
(make-hl-face :fg "white" :bg "navy")
(make-hl-face :fg "white" :bg "DarkViolet")


(defun highlight-region-or-symbol ()
  "Highlight regexp using active region or symbol-at-point as an argument.
   With prefix arg select color interactively."
  (interactive)
  (when-let ((regexp
              (or (and (region-active-p)
                       (regexp-quote
                        (buffer-substring-no-properties (region-beginning)
                                                        (region-end))))
                  (when-let ((sym (thing-at-point 'symbol t)))
                    (format "\\_<%s\\_>" (regexp-quote sym)))
                  (regexp-quote (thing-at-point 'sexp t))))
             (hi-lock-auto-select-face t))
    (if-let ((current-face
              (unquote-safe
               (second (third
                        (or (assoc regexp hi-lock-interactive-patterns)
                            (assoc regexp hi-lock-interactive-lighters)))))))
        (let ((next-face (highlight--read-face-name)))
          (unhighlight-regexp regexp)
          (setf hi-lock--unused-faces
                (cl-remove (symbol-name current-face)
                           hi-lock--unused-faces :test #'equal))
          (highlight-regexp regexp next-face))
      (let ((face (highlight--read-face-name)))
        (or (facep face) (setq face 'hl-white-red))
        (highlight-regexp regexp face)))))

(defun unhighlight-region-or-symbol (arg)
  "Unhighlight regexp using active region or symbol-at-point as an argument.
   With prefix arg unhighlight everything."
  (interactive "P")
  (cond (arg (unhighlight-regexp t))
        ((region-active-p)
         (unhighlight-regexp (regexp-quote (buffer-substring (mark) (point)))))
        (t (unhighlight-regexp (hi-lock-regexp-okay
                                ;;(find-tag-default-as-symbol-regexp)
                                (when-let ((sym (thing-at-point 'symbol t)))
                                  (format "\\_<%s\\_>" (regexp-quote sym))))))))

(defun highlight-lines-matching-regexp-autocolor (arg)
  "Same as `highlight-lines-matching-regexp' but with automatic color selection.
   With prefix arg behaves exactly like `highlight-lines-matching-regexp'."
  (interactive "p")
  (let ((hi-lock-auto-select-face arg))
    (call-interactively 'highlight-lines-matching-regexp)))

(defun highlight--read-face-name ()
  (if hi-lock-auto-select-face
      (hi-lock-read-face-name)
    (let ((ivy-inhibit-action t))
      (counsel-faces))))

;; TODO: refactor
(with-eval-after-load 'swiper
  (defun swiper-highlight-regexp ()
    (interactive)
    (with-ivy-window
        (let* ((hi-lock-auto-select-face t)
               (face (highlight--read-face-name))
               (re (string-join (ivy--split ivy-text) ".*?"))) ; from `swiper-occur'
          (highlight-regexp re face))))

  (defun swiper-highlight-line ()
    (interactive)
    (with-ivy-window
        (let* ((hi-lock-auto-select-face t)
               (face (highlight--read-face-name))
               (re (string-join (ivy--split ivy-text) ".*?"))) ; from `swiper-occur'
          (highlight-lines-matching-regexp re face))))

  (defun swiper-unhighlight-regexp ()
    (interactive)
    (with-ivy-window
        (unhighlight-regexp (string-join (ivy--split ivy-text) ".*?"))))

  (define-key swiper-map (kbd "M-h") 'swiper-highlight-regexp)
  (define-key swiper-map (kbd "M-l") 'swiper-highlight-line)
  (define-key swiper-map (kbd "M-u") 'swiper-unhighlight-regexp))

(global-hi-lock-mode 1)
(global-set-key (kbd "M-h") 'highlight-region-or-symbol)
(global-set-key (kbd "M-u") 'unhighlight-region-or-symbol)
(global-set-key (kbd "M-l") 'highlight-lines-matching-regexp-autocolor)

(define-key org-mode-map (kbd "M-h") 'highlight-region-or-symbol)
(define-key org-mode-map (kbd "M-u") 'unhighlight-region-or-symbol)
(define-key org-mode-map (kbd "M-l") 'highlight-lines-matching-regexp-autocolor)



(provide 'configure-highlight)
