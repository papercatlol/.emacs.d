;; -*- eval: (rainbow-mode 1) -*-
(deftheme quasi-monochrome
  "Created 2018-06-04.")

(defface underlined
    '((t (:underline (:color "LightSalmon3"))))
  "Base face for underlined text(e.g. for isearch, grep etc).")

(custom-theme-set-faces
 'quasi-monochrome
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "light gray" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "xos4" :family "xos4 Terminus"))))
 '(variable-pitch ((t (:family "Fira Code 11")))) ;; Hack is also nice
 '(fixed-pitch ((t (:family "Fira Code 11"))))    ;; Hack is also nice
 '(button ((t (:inherit (link)))))
 '(cursor ((t (:background "LimeGreen"))))
 ;; '(cursor ((t (:background "dark orange"))))
 ;; '(region ((t (:background "DarkGreen" :foreground "black" :weight bold))))
 '(region ((t (:foreground "LightSalmon3" :background "gray10"))))
 ;; '(region ((t (:inherit magit-diff-file-heading-selection))))
 '(bold ((t (:inherit (default) :weight bold))))
 '(escape-glyph ((t (:foreground "light gray"))))
 '(font-lock-builtin-face ((t (:foreground "light gray"))))
 '(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:background "gray20" :foreground "gray60" :extend t))))
 '(hl-todo ((t (:inherit font-lock-comment-face :weight extrabold :foreground "#cc9393"))))
 '(font-lock-constant-face ((t (:weight bold :foreground "light gray"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "white" :weight bold))))
 '(font-lock-keyword-face ((((class color) (type tty)) (:foreground "blue")) (t (:weight bold :foreground "white"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((((class color) (type tty)) (:foreground "magenta")) (t (:foreground "dim gray"))))
 '(font-lock-type-face ((t (:foreground "light gray" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "light gray"))))
 '(font-lock-warning-face ((t (:foreground "gold" :weight bold))))
 '(fringe ((t (:background "black"))))
 '(highlight ((t (:background "gray20"))))
 '(isearch ((t (:background "dark red" :foreground "black" :weight bold))))
 '(match ((t (:inherit underlined :bold t))))
 '(lazy-highlight ((t (:inherit match))))
 ;; '(lazy-highlight ((t (:background "firebrick" :foreground "white" :weight bold))))
 '(isearch-fail ((t (:background "red4"))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "light gray"))))
 '(link-visited ((t (:underline (:color foreground-color :style line) :foreground "light gray"))))
 '(minibuffer-prompt ((t (:weight bold :foreground "light gray"))))
 ;; vertical window separator
 '(vertical-border ((t (:foreground "gray60"))))
 ;; mode-line
 '(mode-line ((t (:background "black" :foreground "#c5c9c0" :bold nil
                  ;; :box (:line-width 1 :color "gray50")
                  :overline "gray60"
                  ))))
 '(mode-line-inactive ((t (:background "gray15" :foreground "#41544B" :bold nil))))
 '(mode-line-buffer-id ((t (:background nil :foreground "white" :bold nil))))
 '(mode-line-highlight ((t (:foreground "aquamarine3" :box nil :bold nil))))
 '(mode-line-emphasis ((t (:foreground "aquamarine3"))))
 ;; header-line
 '(header-line ((t (:background "gray15"
                    :foreground "white"
                    :overline nil
                    :bold t
                    ;; :underline (:color "gray60" :style line)
                    ))))
 ;; '(mode-line ((t (:box nil :foreground "black" :background "gray60"))))
 ;; '(mode-line-buffer-id ((t (:weight bold))))
 ;; '(mode-line-emphasis ((t (:weight bold))))
 ;; '(mode-line-highlight ((t (:box (:line-width 2 :color "light slate gray")))))
 ;; '(mode-line-inactive ((((class color) (type tty)) (:background "black" :foreground "white")) (t (:background "gray20" :foreground "black" :box nil))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(secondary-selection ((t (:inherit (region) :background "gray20"))))
 '(shadow ((t (:foreground "gray60"))))
 '(tooltip ((t (:inherit (variable-pitch) :foreground "black" :background "lightyellow"))))
 '(trailing-whitespace ((t (:background "firebrick"))))
 '(show-paren-match ((t (:underline t :foreground "white" :background "black"))))
 '(idle-highlight ((t (:inherit region :weight extra-bold))))
 '(dired-directory ((t (:inherit font-lock-function-name-face :underline t))))
 ;; '(success ((t (:foreground "white" :weight bold))))
 '(success ((t (:foreground "gray40" :weight bold :underline t))))
 '(compilation-line-number ((t (:foreground "gray30" :weight bold))))
 '(show-paren-mismatch ((t (:background "firebrick3" :foreground "white" :weight extra-bold))))
 ;; ace-window
 '(aw-leading-char-face ((t (:background "black"
                             :foreground "lime green"
                             :weight extra-bold
                             :height 120))))
 '(aw-mode-line-face ((t (:foreground "white" :weight extra-bold))))
 '(aw-key-face ((t (:foreground "white" :weight extra-bold))))
 '(popup-tip-face ((t (:inherit font-lock-comment-face))))
 '(popup-face ((t (:background "gray12" :foreground "white"))))
 '(popup-isearch-match ((t (:inherit isearch))))
 '(popup-menu-face ((t (:inherit popup-face :inverse-video nil :weight normal))))
 '(popup-menu-mouse-face ((t (:inherit popup-menu-selection-face :background "blue"))))
 '(popup-menu-selection-face ((t (:inherit popup-menu-face :inverse-video t :weight ultra-bold))))
 '(popup-menu-summary-face ((t (:inherit popup-menu-face :weight normal))))
 '(popup-tip-face ((t (:inherit font-lock-comment-face :weight normal))))
 '(error ((t (:foreground "firebrick" :weight bold))))
 '(hl-line ((t (:background "gray10"))))
 ;; '(ivy-current-match ((t (:background "gray50" :foreground "white"))))
 '(ivy-current-match ((t (:inherit region ;; :foreground "white"
                          ))))
 '(ivy-minibuffer-match-face-1 ((t (:inherit underlined))))
 '(ivy-minibuffer-match-face-2 ((t (:inherit ivy-minibuffer-match-face-1))))
 '(ivy-minibuffer-match-face-3 ((t (:inherit ivy-minibuffer-match-face-1))))
 '(ivy-minibuffer-match-face-4 ((t (:inherit ivy-minibuffer-match-face-1))))
 '(swiper-match-face-1 ((t (:inherit  lazy-highlight))))
 '(swiper-match-face-2 ((t (:inherit swiper-match-face-1))))
 '(swiper-match-face-3 ((t (:inherit swiper-match-face-1))))
 '(swiper-match-face-4 ((t (:inherit swiper-match-face-1))))
 ;; '(avy-lead-face ((t (:background "white" :foreground "black" :weight extra-bold))))
 ;; '(avy-lead-face-0 ((t (:inherit avy-lead-face :foreground "black"))))
 ;; '(avy-lead-face-2 ((t (:inherit avy-lead-face :foreground "dark red"))))
 '(avy-lead-face ((t (:background "darkgreen" :foreground "white"
                      :weight extra-bold
                      :underline nil))))
 ;; '(avy-lead-face ((t (:background "white" :foreground "dark red" :weight extra-bold))))
 '(avy-lead-face-0 ((t (:inherit avy-lead-face))))
 '(avy-lead-face-1 ((t (:inherit avy-lead-face))))
 '(avy-lead-face-2 ((t (:inherit avy-lead-face))))
 '(avy-goto-char-timer-face ((t ;; (:box (:line-width 1 :color "LimeGreen"))
                              (:underline (:color "LimeGreen"))
                              )))
 ;; magit
 '(magit-header-line ((t (:foreground "white" :background "black" :weight regular ;; :underline t
                          ;; :box t
                          :underline t
                          ))))
 '(magit-diff-hunk-heading-highlight ((t (:foreground "grey70" :background "grey35"
                                          :weight bold
                                          :box t
                                          ;; :overline "tomato"
                                          ))))
 '(magit-diff-context-highlight ((t (:foreground "grey70" :background "grey20"
                                     :weight semi-bold
                                     :underline nil))))
 ;; '(magit-diff-removed-highlight ((t (:background "#663333"
 ;;                                     :foreground "#eecccc"
 ;;                                     :extend t))))
 ;; '(magit-section-heading ((t (:inherit magit-header-line))))
 ;; org
 ;; '(org-default ((t (:inherit default :height 140))))
 '(org-default ((t (:inherit default))))
 '(org-level-3 ((t (:inherit org-default))))
 '(org-level-4 ((t (:inherit org-default))))
 '(org-date ((t (:inherit font-lock-string-face :weight bold))))
 '(org-tag ((t (:inherit mode-line-emphasis))))
 '(org-todo ((t (:foreground "red4" :weight bold))))
 '(org-done ((t (:foreground "lime green" :weight bold))))
 '(org-block-begin-line ((t (:inherit default :foreground "grey50"
                             :underline t :extend t))))
 '(org-block-end-line ((t (:inherit org-block-begin-line
                           :underline nil :overline t))))
 '(org-block ((t (:inherit font-lock-comment-face :background "grey10"))))
 '(org-table ((t (:foreground "chocolate2"))))
 ;; ediff
 '(ediff-even-diff-A ((t (:foreground "black" :background "light grey"))))
 '(ediff-even-diff-B ((t (:foreground "black" :background "light grey"))))
 '(ediff-even-diff-C ((t (:foreground "black" :background "light grey"))))
 '(ediff-odd-diff-A ((t (:foreground "black" :background "light grey"))))
 '(ediff-odd-diff-B ((t (:foreground "black" :background "light grey"))))
 '(ediff-odd-diff-C ((t (:foreground "black" :background "light grey"))))
 ;;
 '(dgi-commit-message-face ((t (:inherit font-lock-string-face))))
 '(iedit-occurrence ((t (:box (:color "DarkViolet")))))
 ;; frog-menu
 '(frog-menu-posframe-background-face ((t (:background "black"))))
 '(frog-menu-candidates-face ((t (:foreground "white"))))
 ;; equake
 '(equake-shell-type-shell ((t (:foreground "white"))))
 '(sh-heredoc ((t (:inherit font-lock-string-face))))
 ;; loccur
 '(loccur-face ((t (:inherit match :underline (:color "DarkViolet")))))
 ;; which-key
 '(which-key-separator-face ((t (:inherit font-lock-comment-face
                                 :background "black"))))
 ;; fill column
 '(fill-column-indicator ((t (:foreground "grey20"))))
 ;; which-func
 '(which-func ((t (:inherit font-lock-keyword-face :foreground "grey60"))))
 )

(provide-theme 'quasi-monochrome)
