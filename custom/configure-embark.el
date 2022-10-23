;; -*- lexical-binding: t -*-
(require 'embark)

;;* general
(setq embark-prompter 'embark-keymap-prompter)
(setq embark-mixed-indicator-delay 0.5)
(setq embark-indicators
      '(embark-mixed-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))

(global-set-key (kbd "H-a") 'embark-act)
(global-set-key (kbd "H-SPC") 'embark-act)

;;* indentation for embark.el macros
(put 'embark-define-keymap lisp-indent-function '(as defun))
(put 'embark-redefine-keymap lisp-indent-function '(as defun))

;;* avy
(defun avy-action-embark (pt)
  (save-excursion
   (goto-char pt)
   (embark-act)))

(setf (alist-get ?  avy-dispatch-alist) #'avy-action-embark)

;;* fix 'callable' keymaps
;; Embark has this weird hack where it fsets keymaps as functions, which breaks
;; find-definiton for those keymaps (it tries to find a function). To fix this
;; we make function name different from keymap name.
(fset 'embark-encode-map nil)
(fset 'embark-encode-map+ embark-encode-map)
(define-key embark-region-map (kbd ">") 'embark-encode-map+)

(fset 'embark-sort-map nil)
(fset 'embark-sort-map+ embark-sort-map)
(define-key embark-region-map (kbd "s") 'embark-sort-map+)

(fset 'embark-vc-file-map nil)
(fset 'embark-vc-file-map+ embark-vc-file-map)
(define-key embark-file-map (kbd "v") 'embark-vc-file-map)

;;* keymaps
;; Mostly remove commands that are accesible elsewhere to make keymaps nicer.

;;** remove unnecessary keymaps/finders
(setq embark-target-finders
      '(embark-target-top-minibuffer-completion
        embark-target-active-region
        embark-target-text-heading-at-point
        embark-target-collect-candidate
        embark-target-completion-at-point
        embark-target-bug-reference-at-point
        ;;embark-target-package-at-point
        ;;embark-target-email-at-point ; link-hint
        ;;embark-target-url-at-point
        ;;embark-target-file-at-point
        ;;embark-target-custom-variable-at-point
        embark-target-identifier-at-point
        embark-target-library-file-at-point
        ;;embark-target-expression-at-point
        embark-target-sentence-at-point
        embark-target-paragraph-at-point
        ;;embark-target-defun-at-point
        embark-target-prog-heading-at-point))

(setq embark-keymap-alist
      '(
        ;;(file embark-file-map) ; ffap+ivy is good enough for files
        (library embark-library-map)
        (environment-variables embark-file-map) ; they come up in file completion
        ;;(url embark-url-map) ; link-hint
        ;;(email embark-email-map)
        (buffer embark-buffer-map)
        (tab embark-tab-map)
        ;;(expression embark-expression-map)
        (identifier embark-identifier-map)
        ;;(defun embark-defun-map)
        (symbol embark-symbol-map)
        (face embark-face-map)
        (command embark-command-map)
        ;;(variable embark-variable-map)
        #'embark-function-map
        (minor-mode embark-command-map)
        (unicode-name embark-unicode-name-map)
        ;;(package embark-package-map)
        (bookmark embark-bookmark-map)
        (region embark-region-map)
        (sentence embark-sentence-map)
        (paragraph embark-paragraph-map)
        (kill-ring embark-kill-ring-map)
        (heading embark-heading-map)
        (t embark-general-map)))

;;** embark-redefine-keymap
(defmacro embark-redefine-keymap (name doc &rest body)
  (declare (indent defun))
  `(progn (makunbound ',name)
          (embark-define-keymap ,name ,doc ,@body)))

;;** embark-general-map
(embark-redefine-keymap embark-general-map
  "Keymap for Embark general actions."
  :parent embark-meta-map
  ("i" embark-insert)
  ("w" embark-copy-as-kill)
  ("q" embark-toggle-quit)
  ;; These are handled by ivy.
  ;;("E" embark-export)
  ;;("S" embark-collect)
  ;;("L" embark-live)
  ;;("B" embark-become)
  ("A" embark-act-all)                  ; undecided on this one
  )

;;** embark-encode-map
;; Make < go back to `embark-region-map'.
(fset 'embark-region-map+ embark-region-map)
(define-key embark-encode-map (kbd "<") 'embark-region-map+)

;;** embark-region-map
(embark-redefine-keymap embark-region-map
  "Keymap for Embark actions on the active region."
  ("r" embark-eval-replace)
  ("a" align-regexp)
  ("TAB" indent-region)
  ("f" fill-region)
  ("$" ispell-region)
  ("t" org-table-convert-region)
  ("w" write-region)
  ;;("+" append-to-file)
  ("m" apply-macro-to-region-lines)
  ;;("n" narrow-to-region)
  ;;("*" calc-grab-region)
  ;;(":" calc-grab-sum-down)
  ;;("_" calc-grab-sum-across)
  ("R" reverse-region)
  ("d" delete-duplicate-lines)
  ;;("b" browse-url-of-region)
  ;;("h" shr-render-region)
  ;;("'" expand-region-abbrevs)
  ("v" vc-region-history)
  ;;("R" repunctuate-sentences)
  ("s" 'embark-sort-map+)
  (">" 'embark-encode-map+))

;;** embark-library-map
(embark-redefine-keymap embark-library-map
  "Keymap for operations on Emacs Lisp libraries."
  ("f" find-library)
  ("l" load-library)
  ("h" finder-commentary)
  ("m" info-display-manual))

;;** embark-identifier-map
(embark-define-keymap embark-identifier-map
  "Keymap for Embark identifier actions."
  ("." xref-find-definitions)
  ("r" xref-find-references)
  ("I" info-lookup-symbol)
  ;;("n" embark-next-symbol)
  ;;("p" embark-previous-symbol)
  ;;("'" expand-abbrev)
  ;;("$" ispell-word)
  ;;("o" occur)
  )

;;** embark-symbol-map
(defun makunbound-symbol (&optional symbol)
  "Interactively `makunbound' symbol."
  (interactive "sSymbol: ")
  (makunbound symbol))

(embark-redefine-keymap embark-symbol-map
  "Keymap for Embark symbol actions."
  :parent embark-identifier-map
  ("." embark-find-definition)
  ("h" describe-symbol)
  ("s" embark-info-lookup-symbol)
  ("e" pp-eval-expression)
  ("a" apropos)
  ("\\" embark-history-remove)
  ("u" makunbound-symbol))

;;** embark-face-map
;; MAYBE Add to kill-ring instead of inserting.
(define-key embark-face-map (kbd "p") 'face-attributes-pretty-print)

;;** embark-function-map
(embark-redefine-keymap embark-function-map
  "Keymap for Embark function actions."
  :parent embark-symbol-map
  ("m" elp-instrument-function) ;; m=measure
  ("M" 'elp-restore-function) ;; quoted, not autoloaded
  ("b" debug-on-entry)
  ("B" cancel-debug-on-entry)
  ("t" trace-function)
  ("T" 'untrace-function))

;;** embark-command-map
(embark-redefine-keymap embark-command-map
  "Keymap for Embark command actions."
  :parent embark-function-map
  ("x" execute-extended-command)
  ("I" Info-goto-emacs-command-node)
  ("W" where-is)
  ;;("g" global-set-key)
  ;;("l" local-set-key)
  )

;;* TODO merge shadowed keymaps, show a nicer help message (something like
;; hydra/transient). Maybe put each type into its own column, e.g.:
(setq embark-test-message "
General  Identifier         Symbol            Function
i insert . find-definitions h describe        m elp-instrument-function
w copy   r find-references  \\ history-remove M elp-restore-function
         I info-lookup                        b debug-on-entry
                                              B cancel-debug-on-entry
                                              t trace-function
                                              T untrace-function")
;;(lv-message "%s" (substring embark-test-message 2))
;;(lv-delete-window)

;;* TODO better ivy integration

(provide 'configure-embark)
