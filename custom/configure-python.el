;;; -*- lexical-binding: t -*-

;; (defun elpy-init ()
;;   (elpy-enable))
;; (add-hook 'python-mode-hook 'elpy-init)
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(setq python-shell-interpreter "ipython"
      ;; see https://www.emacswiki.org/emacs/PythonProgrammingInEmacs#h5o-52
      python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True"
      python-indent-guess-indent-offset-verbose nil
      python-shell-prompt-detect-failure-warning nil

      ;; from prelude
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
      )

;;* shell completion hack
;; For some reason IPython completion doesn't work, so we remove it for the
;; non-module completion.
(setq python-shell-completion-setup-code
      "
def __PYTHON_EL_get_completions(text):
    completions = []
    completer = None

    try:
        import readline

        try:
            import __builtin__
        except ImportError:
            # Python 3
            import builtins as __builtin__
        builtins = dir(__builtin__)

        is_ipython = ('__IPYTHON__' in builtins or
                      '__IPYTHON__active' in builtins)
        splits = text.split()
        is_module = splits and splits[0] in ('from', 'import')

        if is_ipython and is_module:
            from IPython.core.completerlib import module_completion
            completions = module_completion(text.strip())
        else:
            # Try to reuse current completer.
            completer = readline.get_completer()
            if not completer:
                # importing rlcompleter sets the completer, use it as a
                # last resort to avoid breaking customizations.
                import rlcompleter
                completer = readline.get_completer()
            if getattr(completer, 'PYTHON_EL_WRAPPED', False):
                completer.print_mode = False
            i = 0
            while True:
                completion = completer(text, i)
                if not completion:
                    break
                i += 1
                completions.append(completion)
    except:
        pass
    finally:
        if getattr(completer, 'PYTHON_EL_WRAPPED', False):
            completer.print_mode = True
    return completions")

(defun inferior-python--enable-eldoc ()
  "Enable basic python.el eldoc in python shell."
  (require 'python)
  (setq-local eldoc-documentation-function #'python-eldoc-function))

(add-hook 'inferior-python-mode-hook #'inferior-python--enable-eldoc)

;;* lsp
(require 'configure-lsp)

(add-hook 'python-mode-hook 'configure-lsp:init)

;;* virtual env
(require 'pyvenv)

(provide 'configure-python)


;;* ein
(require 'ein)

(setq ein:output-area-inlined-images t)
