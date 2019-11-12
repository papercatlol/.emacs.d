;;; -*- lexical-binding: t -*-
(require 'ivy)
(require 'counsel)


(cl-defun counsel-shell-command (cmd &key
                                       (prompt "")
                                       (initial-input "")
                                       (dynamic-collection t)
                                       (keymap counsel-shell-command-map)
                                       args)
  (interactive)
  (let ((ivy-pre-prompt-function csc/ivy-pre-prompt-function)
        (csc/current-cmd cmd)
        (csc/current-args args)
        (ivy-text initial-input))
    (ivy-read prompt
              (if dynamic-collection
                  csc/shell-command-execute-fn
                (funcall csc/shell-command-execute-fn initial-input))
              :initial-input initial-input
              :keymap keymap
              :dynamic-collection dynamic-collection)))


(defvar csc/current-cmd nil)
(defvar csc/current-args nil)
(defvar csc/shell-command-format-fn #'csc/shell-command-format)
(defvar csc/shell-command-execute-fn #'csc/shell-command-execute)

(defun csc/shell-command-format (ivy-text)
  (let* ((cmd csc/current-cmd)
         (args (string-join csc/current-args " "))
         (query (and ivy-text (concat "-- " (shell-quote-argument ivy-text))))
         (full-cmd (string-join
                    (remove nil (list cmd args query))
                    " ")))
    full-cmd))

(defun csc/shell-command-execute (ivy-text)
  (split-string (shell-command-to-string (funcall csc/shell-command-format-fn ivy-text)) "\n" t))

;;* ivy hooks
(defvar csc/ivy-pre-prompt-function #'csc/ivy-pre-prompt)

(defun csc/ivy-pre-prompt ()
  (concat (funcall csc/shell-command-format-fn ivy-text) "\n"))

(defun counsel--shell-command-hook ()
  (setq-local max-mini-window-height (+ 1 max-mini-window-height))
  (set-window-text-height nil (+ 1 (window-height))))

(setf (alist-get #'counsel-shell-command ivy-hooks-alist)
      #'counsel--shell-command-hook)

;;* api
(defvar counsel-shell-command-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m ivy-minibuffer-map)
    m))

(defun csc/toggle-flag (flag)
  "Return an interactive function that toggles FLAG
updates command results. FLAG is a string."
  (assert (stringp flag))
  (lambda ()
    (interactive)
    (setq csc/current-args
          (loop with args
                for (arg . rest) on csc/current-args
                if (string= flag arg)
                  return (append args rest)
                collect arg into args
                finally (return (cons flag args))))
    (ivy--reset-state ivy-last)
    nil))

(define-key counsel-shell-command-map (kbd "C-c d") (csc/toggle-flag "-t d"))

;; TEMP
(defun csc/test ()
  (interactive)
  (counsel-shell-command "fd"
                         :args '("-t d")))
(evil-local-set-key 'normal (kbd "0") 'csc/test)


(when nil
  (let ((csc/current-cmd "fd")
        (csc/current-args '("-t d")))
    (shell-command-to-string (funcall csc/shell-command-format-fn "")))

  (let ((csc/ivy-pre-prompt-function nil))
    (counsel-shell-command "fd"
                           :args '("-t d")
                           :prompt "fd -t d: "))

  (counsel-shell-command "rg" :args '("-M 120"))
  )



(provide 'counsel-shell-command)
