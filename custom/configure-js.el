;; -*- lexical-binding: t -*-


;;* indentation
(setq js-indent-level 2)

;;* electric pairs
(add-hook 'js-mode-hook #'electric-pair-local-mode)
(add-hook 'js2-mode-hook #'electric-pair-local-mode)

;;* disable lockfiles for js buffers since it fucks up webpack file watching
(defun disable-lockfiles-local ()
  (setq-local create-lockfiles nil))

(add-hook 'js-mode-hook #'disable-lockfiles-local)

;;* js2-mode
;; https://emacs.cafe/emacs/javascript/setup/2017/04/23/emacs-setup-javascript.html
;; https://emacs.cafe/emacs/javascript/setup/2017/05/09/emacs-setup-javascript-2.html
(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

;;** swap C-c s and C-c C-s
(define-key js2-mode-map (kbd "C-c C-s") 'counsel-imenu-dwim)
(define-key js2-mode-map (kbd "C-c s") 'js2-mode-show-element)

;;** swap C-c f and C-c C-s
(define-key js2-mode-map (kbd "C-c C-f") 'flyspell-hydra)
(define-key js2-mode-map (kbd "C-c f") 'js2-mode-toggle-hide-functions)

;;** folding
;; HACK handle various `%VAR_NAME% = function` and `export function` cases.
(defun js2-mode-toggle-element--wrapper (fn &rest args)
  (save-excursion
   (re-search-forward "function" (line-end-position) t)
   (apply fn args)))
(advice-add 'js2-mode-toggle-element :around #'js2-mode-toggle-element--wrapper)

(define-key js2-mode-map (kbd "<C-tab>") 'js2-mode-toggle-element)

;;* js2-refactor
(require 'js2-refactor)

(add-hook 'js2-mode-hook #'js2-refactor-mode)

(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;;** js2r hydra
;; https://gist.github.com/anachronic/7af88c62db136727cd1fed17ee0a662f
(defhydra hydra-js2-refactor (:color blue :hint nil)
  "
^Functions^                    ^Variables^               ^Buffer^                      ^sexp^               ^Debugging^
------------------------------------------------------------------------------------------------------------------------------
[_p_] Localize Parameter       [_ev_] Extract variable   [_wi_] Wrap buffer in IIFE    [_k_]  js2 kill      [_l_] log this
[_ef_] Extract function        [_iv_] Inline variable    [_ig_] Inject global in IIFE  [_ss_] split string  [_dt_] debug this
[_ip_] Introduce parameter     [_r_] Rename variable     [_ee_] Expand node at point   [_>_] forward slurp
[_em_] Extract method          [_vt_] Var to this        [_cc_] Contract node at point [_<_] forward barf
[_ao_] Arguments to object     [_sv_] Split var decl.    [_uw_] unwrap
[_tf_] Toggle fun exp and decl [_ag_] Add var to globals
[_ta_] Toggle fun expr and =>  [_ti_] Ternary to if
[_q_]  quit"
  ("ee" js2r-expand-node-at-point)
  ("cc" js2r-contract-node-at-point)
  ("ef" js2r-extract-function)
  ("em" js2r-extract-method)
  ("tf" js2r-toggle-function-expression-and-declaration)
  ("ta" js2r-toggle-arrow-function-and-expression)
  ("ip" js2r-introduce-parameter)
  ("p" js2r-localize-parameter)
  ("wi" js2r-wrap-buffer-in-iife)
  ("ig" js2r-inject-global-in-iife)
  ("ag" js2r-add-to-globals-annotation)
  ("ev" js2r-extract-var)
  ("iv" js2r-inline-var)
  ("r" js2r-rename-var)
  ("vt" js2r-var-to-this)
  ("ao" js2r-arguments-to-object)
  ("ti" js2r-ternary-to-if)
  ("sv" js2r-split-var-declaration)
  ("ss" js2r-split-string)
  ("uw" js2r-unwrap)
  ("l" js2r-log-this)
  ("dt" js2r-debug-this)
  (">" js2r-forward-slurp)
  ("<" js2r-forward-barf)
  ("k" js2r-kill)
  ("q" nil))

(define-key js2-mode-map (kbd "C-c C-x") 'hydra-js2-refactor/body)


;;* xref-js2
(require 'xref-js2)

(setq xref-js2-search-program 'rg)

(defun xref-js2-add-xref-backend ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))
(add-hook 'js2-mode-hook #'xref-js2-add-xref-backend)

(define-key js2-mode-map (kbd "M-.") nil)

(provide 'configure-js)
