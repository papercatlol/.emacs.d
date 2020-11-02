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
      `(unless (facep ',facename)
         (push ,facename-string hi-lock-face-defaults)
         (defface ,facename
           '((t (:foreground ,fg :background ,bg :weight bold)))
           ,docstring
           :group ',group)))))

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
  (if (region-active-p)
      (let* ((hi-lock-auto-select-face t)
             (face (highlight--read-face-name)))
        (or (facep face) (setq face 'hl-white-red))
        (highlight-regexp (buffer-substring (mark) (point)) face))
    (highlight-symbol-at-point)))

(defun unhighlight-region-or-symbol (arg)
  "Unhighlight regexp using active region or symbol-at-point as an argument.
   With prefix arg unhighlight everything."
  (interactive "P")
  (cond (arg (unhighlight-regexp t))
        ((region-active-p)
         (unhighlight-regexp (buffer-substring (mark) (point))))
        (t (unhighlight-regexp (hi-lock-regexp-okay
                                (find-tag-default-as-symbol-regexp))))))

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



(provide 'configure-highlight)
