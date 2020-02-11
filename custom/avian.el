;; -*- lexical-binding: t -*-
;; `avian-swiper-mode' displays avy-style overlays for visible swiper candidates.
;; Type displayed characters and call `avian:swiper-done' to jump.

(require 's)

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

(defvar avian:overlay-position-fn #'avian:overlay-pos-at
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
  (let* ((inhibit-modification-hooks t)
         (beg (funcall avian:overlay-position-fn pos key))
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

(cl-defun avian:add-overlays (positions &optional (window (ivy-state-window ivy-last)))
  (avian:cleanup-overlays)
  (when (and positions window (>= (length avian:keys) (length positions)))
    (let* ((prefix (save-excursion
                    (buffer-substring-no-properties
                     (1- ; char at point could be an avian key as well
                      (point))
                     (progn (forward-symbol -1) (point)))))
           (regex (rx-to-string `(seq symbol-start
                                      ,prefix
                                      (group (1+ (not whitespace)))
                                      symbol-end)))
           (suffixes
             (loop for line in ivy--old-cands
                   append (mapcar #'second (s-match-strings-all regex line)))))
      (cl-labels ((%skip-keys (keys)
                    "Skip keys that are valid suffixes for current input."
                    (member-if-not (lambda (key)
                                     (some (lambda (suffix) (string-prefix-p key suffix)) suffixes))
                                   keys)))
        ;; (message "skip-keys: %s" (%skip-keys avian:keys))
        (with-selected-window window
          (loop for pos in positions
                for keys = (%skip-keys avian:keys) then (%skip-keys keys)
                do (let* ((key (car keys))
                          (ov (avian:make-overlay pos key)))
                     (push (cons key ov) avian:*overlays*)
                     (!cdr keys)))
          avian:*overlays*)))))

(cl-defun avian:swiper-visible-overlays (&optional
                                           (window (ivy-state-window ivy-last))
                                           (start (window-start window)))
  (with-selected-window window
    (let* ((start (window-start))
           (end (window-end))
           (visible-overlays
             (save-excursion
              (goto-char (window-start))
              (loop for i to (window-height)
                    append (overlays-in (line-beginning-position) (line-end-position))
                    do (forward-line))))
           (visible-positions
             (remove-duplicates
              (cl-sort (mapcar #'overlay-start
                               (remove-if-not (lambda (ov)
                                                (and (overlay-start ov)
                                                     (memq (overlay-get ov 'face)
                                                           (append swiper-faces swiper-background-faces))))
                                              visible-overlays))
                       #'<)
              :from-end t
              :key #'line-number-at-pos)))
      ;; (message "window: %s positions: %d ovs: %d"
      ;;          (buffer-name (window-buffer (selected-window)))
      ;;          (length visible-positions)
      ;;          (length visible-overlays))
      visible-positions)))

;;* candidate selection
(cl-defun avian:swiper-done ()
  (interactive)
  (when-let* ((ov (avian:selected-overlay))
              (buf (overlay-buffer ov))
              (pos (overlay-start ov)))
    ;; (message "found ov in buffer %s" (overlay-buffer ov))
    (ivy-quit-and-run
     (switch-to-buffer buf)
     (goto-char pos))))

(defvar avian:swiper-maybe-done
  '(menu-item "" avian:swiper-done :filter avian:swiper-maybe-done-filter)
  "Conditional keybinding. Jump to selected avian candidate if any.")

(defun avian:swiper-maybe-done-filter (cmd)
  (when (avian:selected-overlay)
    cmd))

(defun avian:selected-overlay ()
  (let ((max-length (apply #'max (mapcar #'length avian:keys))))
    (save-excursion
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
                (backward-char 1))))))

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
  (when (minibuffer-window-active-p (selected-window))
    (let ((positions (avian:swiper-visible-overlays)))
      (avian:add-overlays positions))))

(defun avian:on-window-scroll (window start)
  (when (and (active-minibuffer-window)
             (eq window (ivy-state-window ivy-last)))
    (let ((positions (avian:swiper-visible-overlays window)))
      ;; (message "win: %s positions: %s" (window-buffer window) positions)
      (avian:add-overlays positions window))))

(defun avian:after-swiper--maybe-recenter ()
  (with-ivy-window
      (let ((positions (avian:swiper-visible-overlays)))
        (avian:add-overlays positions))))

(defun avian:ivy-unwind ()
  (swiper--cleanup)
  (avian:cleanup-overlays))

(defun avian:avy-before (&rest args)
  (avian:cleanup-overlays))

(defun avian:enable ()
  (advice-add 'swiper--add-overlays :after #'avian:after-swiper--add-overlays)
  (add-hook 'window-scroll-functions #'avian:on-window-scroll)
  (ivy--alist-set 'ivy-unwind-fns-alist 'swiper #'avian:ivy-unwind)
  (advice-add 'swiper-avy :before #'avian:avy-before)
  (advice-add 'ivy-avy :before #'avian:avy-before))

(defun avian:disable ()
  (advice-remove 'swiper--add-overlays #'avian:after-swiper--add-overlays)
  (remove-hook 'window-scroll-functions #'avian:on-window-scroll)
  (ivy--alist-set 'ivy-unwind-fns-alist 'swiper #'swiper--cleanup)
  (advice-remove 'swiper-avy #'avian:avy-before)
  (advice-remove 'ivy-avy #'avian:avy-before))



(provide 'avian)
