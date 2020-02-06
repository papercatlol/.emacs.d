;; `avian-swiper-mode' displays avy-style overlays for visible swiper candidates.
;; Type displayed characters and call `avian:swiper-done' to jump.

;; TODO: make sure there are no candidates left after entering avian chars
;; so that swiper doesn't retarget elsewhere. Then binding some `avian:swiper-done-magical-dwim'
;; to Space would be possible.

(defvar avian:keys
  '("f" "c" "d" "g" "s" "a" "e" "v" "z" "r"
    "fd" "cd" "df" "dc" "re"
    "F" "C" "D" "G" "S" "A" "E" "V" "Z" "R"))

;;* overlays
(defvar avian:*overlays* nil
  "A list of (key . ov) pairs of currently active overlays.")

(defface avian-overlay-face
    '((t (:inherit avy-lead-face)))
  "Face for avian-swiper overlays.")

(defvar avian:overlay-position-fn #'avian:overlay-pos-pre
  "(fn (swiper-candidate-beg key)) -> avian overlay beginning position.
See `avian:overlay-pos-at' and `avian:overlay-pos-pre'")

(defun avian:overlay-pos-at (pos key)
  pos)

(defun avian:overlay-pos-pre (pos key)
  (max (save-excursion
        (goto-char pos)
        (line-beginning-position))
       (- pos (length key))))


(defun avian:make-overlay (pos key)
  (let* ((beg (funcall avian:overlay-position-fn pos key))
         (ov (make-overlay beg (+ beg (length key)))))
    (overlay-put ov 'face 'avian-overlay-face)
    (overlay-put ov 'avian-overlay t)
    (overlay-put ov 'display key)
    ov))

(cl-defun avian:cleanup-overlays (&optional (overlays avian:*overlays*))
  (when (eq overlays avian:*overlays*)
    (setq avian:*overlays* nil))
  (dolist (pair overlays)
    (delete-overlay (cdr pair))))

(defun avian:add-overlays (positions)
  (avian:cleanup-overlays)
  (when (>= (length avian:keys) (length positions))
   (loop for pos in positions
         for key in avian:keys
         do (let ((ov (avian:make-overlay pos key)))
              (push (cons key ov) avian:*overlays*)))))

;;* candidate selection
(cl-defun avian:swiper-done ()
  (interactive)
  (when-let* ((max-length (apply #'max (mapcar #'length avian:keys)))
              (ov (save-excursion
                   (loop repeat (1- max-length)
                         with key = ""
                         with overlays = avian:*overlays*
                         do (progn
                              (setq key (concat key (char-to-string (char-before))))
                              (multiple-value-bind (ov prefix)
                                  (avian:find-overlay key overlays)
                                (cond (ov (return ov))
                                      (prefix (setq overlays prefix))
                                      ((null prefix) (return nil))))
                              (backward-char 1)))))
              (buf (overlay-buffer ov))
              (pos (overlay-start ov)))
    (message "found ov in buffer %s" (overlay-buffer ov))
    (ivy-quit-and-run
     (switch-to-buffer buf)
     (goto-char pos))))

(cl-defun avian:find-overlay (str &optional (overlays avian:*overlays*))
  (loop for (key . ov) in overlays
        with prefix
        when (string= key str)
          return (values ov nil)
        when (string-prefix-p str key)
          do (push (cons key ov) prefix)
        finally (return (values nil prefix))))

;;* avian-swiper-mode
(define-minor-mode avian-swiper-mode
  "Display avy-style overlays for swiper candidates in current window."
  :group 'avian
  :global t
  (if avian-swiper-mode
      (avian:enable)
    (avian:disable)))


(defun avian:after-swiper--add-overlays (&rest args)
  (let ((positions (remove-duplicates
                    (remove-if (lambda (pos)
                                 (or (< pos (window-start)) (> pos (window-end))))
                               (reverse (mapcar #'overlay-start isearch-lazy-highlight-overlays)))
                    :key #'line-number-at-pos :from-end t)))
    (avian:add-overlays positions)))

(defun avian:ivy-unwind ()
  (swiper--cleanup)
  (avian:cleanup-overlays))

(defun avian:enable ()
  (advice-add 'swiper--add-overlays :after #'avian:after-swiper--add-overlays)
  (ivy--alist-set 'ivy-unwind-fns-alist 'swiper #'avian:ivy-unwind))

(defun avian:disable ()
  (advice-remove 'swiper--add-overlays #'avian:after-swiper--add-overlays)
  (ivy--alist-set 'ivy-unwind-fns-alist 'swiper #'swiper--cleanup))


(provide 'avian)
