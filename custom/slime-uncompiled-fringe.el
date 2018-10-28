(require 'slime)

(define-slime-contrib slime-uncompiled-fringe
  "Same as `slime-highlight-edits.el' using fringe."
  (:authors "papercatlol@gmail.com")
  (:license "GPL")
  (:on-load (progn (add-hook 'slime-mode-hook 'slime-uncompiled-fringe-mode)
                   (add-to-list 'slime-before-compile-functions
                                'slime-uncompiled-fringe-remove)))
  (:on-unload (progn (remove-hook 'slime-mode-hook 'slime-uncompiled-fringe-mode)
                     (setq 'slime-before-compile-functions
                           (remove 'slime-uncompiled-fringe-remove
                                   'slime-before-compile-functions)))))

(defface slime-uncompiled-fringe-face
  '((t (:background "white")))
  "Fringe face."
  :group 'slime-mode-faces)

(defvar slime-uncompiled-fringe-bitmap
  'empty-line
  "Fringe bitmap to use.
https://www.gnu.org/software/emacs/manual/html_node/elisp/Fringe-Bitmaps.html")

(defvar slime-uncompiled-fringe-position
  'left-fringe
  "Which fringe to use. Can be `left-fringe' or `right-fringe'.")

(define-minor-mode slime-uncompiled-fringe-mode 
  "Minor mode to highlight fringe near uncompiled code.")

(add-hook 'slime-uncompiled-fringe-mode-on-hook
          'slime-uncompiled-fringe-init-buffer)

(add-hook 'slime-uncompiled-fringe-mode-off-hook
          'slime-uncompiled-fringe-reset-buffer)

(defun slime-uncompiled-fringe-init-buffer ()
  (make-local-variable 'after-change-functions)
  (add-to-list 'after-change-functions 
               'slime-uncompiled-fringe-add))

(defun slime-uncompiled-fringe-reset-buffer ()
  (setq after-change-functions  
        (remove 'slime-uncompiled-fringe after-change-functions))
  (slime-uncompiled-fringe-remove (point-min) (point-max)))

(defun slime-uncompiled-fringe-add (beg end &optional len)
  (declare (ignore len))
  (slime-uncompiled-fringe-remove beg end)
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'line-prefix (propertize " " 'display (list slime-uncompiled-fringe-position
                                                                     slime-uncompiled-fringe-bitmap
                                                                     'slime-uncompiled-fringe-face)))
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'slime-uncompiled-fringe-overlay t)))

(defun slime-uncompiled-fringe-remove (beg end)
  (remove-overlays beg end 'slime-uncompiled-fringe-overlay t))

(provide 'slime-uncompiled-fringe)
