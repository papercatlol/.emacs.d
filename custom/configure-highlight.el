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
           '((t (:foreground ,fg :background ,bg)))
           ,docstring
           :group ',group)))))

(setq hi-lock-face-defaults nil)

(make-hl-face :fg "white" :bg "red")
(make-hl-face :fg "white" :bg "DarkOrange")
(make-hl-face :fg "white" :bg "gold")
(make-hl-face :fg "white" :bg "cyan")
(make-hl-face :fg "white" :bg "blue")
(make-hl-face :fg "white" :bg "DarkViolet")
(make-hl-face :fg "white" :bg "magenta")
(make-hl-face :fg "white" :bg "tomato")
(make-hl-face :fg "white" :bg "sienna")
(make-hl-face :fg "white" :bg "DarkGreen")
(make-hl-face :fg "white" :bg "LimeGreen")
(make-hl-face :fg "white" :bg "aquamarine")
(make-hl-face :fg "white" :bg "navy")
(make-hl-face :fg "white" :bg "maroon")

(global-hi-lock-mode 1)
(global-set-key (kbd "<f5>") 'highlight-symbol-at-point)

(provide 'configure-highlight)
