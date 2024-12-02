;; -*- lexical-binding: t -*-
(require 'configure-flymake)

;; Don't highlight symbol at point.
(pushnew :documentHighlightProvider eglot-ignored-server-capabilities)

;;* hydra-eglot (mimicking `eglot-menu')
(pretty-hydra-define hydra-eglot (:quit-key "q")
  ("Flymake"
   (("SPC" #'display-local-help "show error at point")
    ("k" #'flymake-goto-prev-error "next error")
    ("j" #'flymake-goto-next-error "prev error")
    ("a" #'ace-flymake "[s/t] ace-flymake")
    ("s" #'ace-flymake)
    ("t" #'ace-flymake)
    ("b" #'flymake-toggle-buffer-diagnostics "buffer diagnostics")
    ("p" #'flymake-toggle-project-diagnostics "project diagnostics")
    ("l" #'flymake-switch-to-log-buffer "*Flymake log*" :color blue)
    ("C-k" #'flymake-proc-stop-all-syntax-checks "stop syntax checks")
    ("C-l" #'recenter-top-bottom))
   "Xref"
   (;; xref like commands.
    ("?" xref-find-references "find references" :color blue)
    ("A" xref-find-apropos "find apropos" :color blue)
    ("dd" eglot-find-declaration "find declaration" :color blue)
    ("di" eglot-find-implementation "find implementation" :color blue)
    ("dt" eglot-find-typeDefinition "find typeDefinition" :color blue))
   "Eglot"
   (;; LSP-related commands (mostly Eglot's own commands).
    ("r" eglot-rename "rename" :color blue)
    ("f" eglot-format "format")
    ("F" eglot-format-buffer "format buffer"))
   "Code Actions"
   (("RET" #'flymake-lsp-code-action "all code actions at point")
    ("i" eglot-code-action-organize-imports "organize imports")
    ("e" eglot-code-action-extract "extract" :color blue)
    ("I" eglot-code-action-inline "inline" :color blue)
    ("r" eglot-code-action-rewrite "rewrite" :color blue)
    ("q" eglot-code-action-quickfix "quickfix"))))


(defun maybe-hydra-eglot ()
  "`hydra-eglot' if eglot is enabled, otherwise `hydra-flymake'."
  (interactive)
  (display-local-help)
  (if (bound-and-true-p eglot--managed-mode)
      (hydra-eglot/body)
    (hydra-flymake/body)))

(define-key eglot-mode-map (kbd "C-c c") 'maybe-hydra-eglot)
(define-key flymake-mode-map (kbd "C-c f") 'maybe-hydra-eglot)


(provide 'configure-eglot)
