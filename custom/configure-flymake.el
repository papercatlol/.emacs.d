;; -*- lexical-binding: t -*-

(require 'flymake)


;;* ace-flymake
(defun ace-flymake ()
  "Go to a flymake overlay in current window."
  (interactive)
  (avy-with ace-flymake
    (avy-process
     (ace-flymake-collect))))

(defun ace-flymake-collect ()
  (cl-loop for ov in (overlays-in (window-start) (window-end))
        when (and (overlay-get ov 'flymake-diagnostic)
                  ;; skip overlay at point
                  (not (and (<= (overlay-start ov) (point))
                            (>= (overlay-end ov) (point)))))
          collect (overlay-start ov)))

;;* code actions
(defun flymake-lsp-code-action ()
  "Trigger lsp code action depending on backend."
  (interactive)
  (cond ((bound-and-true-p lsp-mode)
         (call-interactively #'lsp-execute-code-action))
        ((bound-and-true-p eglot--managed-mode)
         (call-interactively #'eglot-code-actions))))

;;* hydra-flymake
(defhydra hydra-flymake (:hint nil)
  "
 Flymake
 ---------------------------------------------------------------------------------
 _j_:   ^^next error          _b_: diagnostics buffer
 _k_:   ^^prev error          _l_: log buffer
 _a_,_s_: ace-flymake       _C-k_: stop all syntax checks
 _SPC_: ^^diplay msg          _q_: quit
 _RET_: code actions
"
  ("q" nil)
  ("k" #'flymake-goto-prev-error)
  ("j" #'flymake-goto-next-error)
  ("a" #'ace-flymake)
  ("s" #'ace-flymake)
  ("SPC" #'display-local-help)
  ("RET" #'flymake-lsp-code-action)
  ("C-c C-c" #'eglot-code-actions)
  ("b" #'flymake-show-diagnostics-buffer :color blue)
  ("l" #'flymake-switch-to-log-buffer :color blue)
  ("C-k" #'flymake-proc-stop-all-syntax-checks)

  ("C-l" #'recenter-top-bottom)
  )

(define-key flymake-mode-map (kbd "C-c f") 'hydra-flymake/body)
(define-key flymake-mode-map (kbd "C-c k") 'hydra-flymake/flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "C-c j") 'hydra-flymake/flymake-goto-next-error)
(define-key flymake-mode-map (kbd "C-c a") 'hydra-flymake/ace-flymake)

;;* flymake-diagnostics-buffer
(defun flymake-diagnostics-buffer-show-next-diagnostic ()
  "Show location of next flymake diagnostic."
  (interactive)
  (forward-line)
  (call-interactively #'flymake-show-diagnostic))

(defun flymake-diagnostics-buffer-show-prev-diagnostic ()
  "Show location of previous flymake diagnostic."
  (interactive)
  (forward-line -1)
  (call-interactively #'flymake-show-diagnostic))

(defun flymake-show-diagnostic-avy ()
  "Use avy to show diagnostic."
  (interactive)
  (let ((avy-all-windows nil))
    (avy-goto-line))
  (call-interactively #'flymake-show-diagnostic))

(let ((m flymake-diagnostics-buffer-mode-map))
  (define-key m (kbd "j") 'next-line)
  (define-key m (kbd "k") 'previous-line)
  (define-key m (kbd "C-j") 'flymake-diagnostics-buffer-show-next-diagnostic)
  (define-key m (kbd "C-k") 'flymake-diagnostics-buffer-show-prev-diagnostic)
  (define-key m (kbd "m") 'flymake-show-diagnostic)
  (define-key m (kbd "f") 'flymake-show-diagnostic)
  (define-key m (kbd "C-q") 'bury-buffer)
  (define-key m (kbd "C-f") 'flymake-show-diagnostic-avy)
  )


(provide 'configure-flymake)
