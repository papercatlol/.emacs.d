;; -*- eval: (rainbow-mode 1) -*-
(deftheme quasi-monochrome
  "Created 2018-06-04.")

(defface underlined
    '((t (:underline (:color "purple"))))
  "Base face for underlined text(e.g. for isearch, grep etc).")

(defface style-warning-face
    '((t (:underline (:color "firebrick" :style wave))))
  "Base face for highlighting style warnings (e.g. trailing whitespaces).")

;; ediff
(defface ediff-diff-base
  '((t (:inherit default)))
  "Base face for ediff regions.")

(defface ediff-even-diff-base
  '((t (:inherit ediff-diff-base)))
  "Base face for even ediff regions.")

(defface ediff-odd-diff-base
  '((t (:inherit ediff-diff-base)))
  "Base face for odd ediff regions.")

;; w3m
(defface w3m-tab-base
    '((t (:inherit default :background "grey20" :weight normal)))
  "Base face for w3m tab bar.")

(custom-theme-set-faces
 'quasi-monochrome
 ;;'(default ((t (:family "Iosevka Custom Terminus"
 ;;               :width normal
 ;;               ;;:width expanded
 ;;               ;;:weight light
 ;;               :weight normal
 ;;               :background "black"
 ;;               :foreground "light gray"
 ;;               :height 120
 ;;               :inverse-video nil))))
 '(default ((t (:font "-ADBO-Source Code Pro-normal-normal-normal-*-23-*-*-*-m-0-iso10646-1"
                :background "black"
                :foreground "light gray"))))
 '(default ((t (:font "-AX86-Terminess Nerd Font Mono-bold-normal-normal-*-28-*-*-*-m-0-iso10646-1"
                ;;:weight regular
                :background "black"
                :foreground "light gray"))))
 ;;'(default ((t (:font "-UKWN-iA Writer Mono S-normal-normal-normal-*-22-*-*-*-*-0-iso10646-1"
 ;;              :background "black"
 ;;              :foreground "light gray"))))
 ;;'(default ((t (:font "-ADBO-SourceCodeVF-light-normal-normal-*-27-*-*-*-m-0-iso10646-1"
 ;;               :background "black"
 ;;               :foreground "light gray"))))
 ;;'(default ((t (:family "Monoid"
 ;;               :height 100
 ;;               :background "black"
 ;;               :foreground "light gray"))))
 ;;'(default ((t (:font "-fsdf-PragmataPro-normal-normal-normal-*-23-*-*-*-m-0-iso10646-1"
 ;;               :background "black"
 ;;               :foreground "light gray"))))
 ;;'(default ((t (:inherit nil :stipple nil
 ;;               :background "black" :foreground "light gray"
 ;;               :inverse-video nil
 ;;               :box nil :strike-through nil :overline nil :underline nil
 ;;               :slant normal :weight normal
 ;;               :height 140 :width normal
 ;;               :foundry "xos4"
 ;;               :family "xos4 Terminus"))))
 ;; fixed & ariable-pitch fonts
 ;;'(variable-pitch ((t (:family "Fira Code" :height 110))))
 ;;'(variable-pitch ((t (:family "Hack" :height 110))))
 ;;'(variable-pitch ((t (:family "Input Sans Narrow" :height 110))))
 ;;'(variable-pitch ((t (:family "Fira Code 11" :height 150))))
 ;;'(variable-pitch ((t (:family "Input Sans Condensed" :height 150))))
 '(variable-pitch ((t (:family "Bookerly" :height 200
                       :foreground "grey"))))
 ;;'(fixed-pitch ((t (:family "Fira Code 11" :height 110))))
 ;;'(fixed-pitch ((t (:family "Input Sans Narrow" :height 110))))
 '(fixed-pitch ((t (:family "Input Sans Condensed" :height 110))))
 ;;'(fixed-pitch-serif ((t (:family "Fira Code" :height 110))))
 '(fixed-pitch-serif ((t (:family "Input Serif Narrow" :height 110))))
 '(button ((t (:inherit (link)))))
 '(cursor ((t (:background "DarkOrange3"))))
 ;; '(region ((t (:background "DarkGreen" :foreground "black" :weight normal))))
 '(region ((t (:foreground "LightSalmon3" :background "gray10"))))
 ;; '(region ((t (:inherit magit-diff-file-heading-selection))))
 '(bold ((t (:inherit (default) :weight semi-bold))))
 '(eldoc-highlight-function-argument ((t (:inherit default :weight bold))))
 '(escape-glyph ((t (:foreground "light gray"))))
 '(font-lock-builtin-face ((t (:foreground "#9090a0"
                               ;; #d3d3d3
                               ))))
 '(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:background "gray20" :foreground "gray60" :extend t))))
 '(hl-todo ((t (:inherit font-lock-comment-face :weight extrabold :foreground "#cc9393"))))
 '(font-lock-constant-face ((t (;;:weight normal
                                :foreground "light gray"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "white" ;;:weight normal
                                     ))))
 '(font-lock-keyword-face ((t (;;:weight normal
                               :foreground "white"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((((class color) (type tty)) (:foreground "magenta")) (t (:foreground "dim gray"))))
 '(font-lock-type-face ((t (:foreground "light gray" ;;:weight normal
                            ))))
 '(font-lock-variable-name-face ((t (:foreground "light gray"))))
 '(font-lock-warning-face ((t (:foreground "gold2" ;;:weight normal
                               ))))
 '(fringe ((t (:background "black"))))
 '(highlight ((t (:background "gray20"))))
 '(isearch ((t (:background "dark red" :foreground "black" :weight normal))))
 '(match ((t (:inherit underlined :weight normal))))
 '(lazy-highlight ((t (:inherit match))))
 ;; '(lazy-highlight ((t (:background "firebrick" :foreground "white" :weight normal))))
 '(isearch-fail ((t (:background "red4"))))
 '(link ((t (:underline (:color foreground-color :style line)))))
 '(link-visited ((t (:underline (:color foreground-color :style line) :foreground "light gray"))))
 '(minibuffer-prompt ((t (;;:weight normal
                          :foreground "light gray"))))
 '(completions-annotations ((t (:inherit font-lock-string-face))))
 ;; vertical window separator
 '(vertical-border ((t (:foreground "gray60"))))
 ;; mode-line
 '(mode-line ((t (:background "black" :foreground "#c5c9c0" :bold nil
                  :box (:line-width 1 :color "gray70")))))
 '(mode-line-inactive ((t (:background "gray10" :foreground "#41544B"
			   :overline "gray70"))))
 '(mode-line-buffer-id ((t (:background nil :foreground "white"))))
 '(mode-line-highlight ((t (:foreground "aquamarine3" :box t))))
 '(mode-line-emphasis ((t (:foreground "aquamarine3"))))
 ;; header-line
 '(header-line ((t (:background "#1f2f2f"
                    :foreground "white"
                    :overline nil
                    ;;:weight normal
                    ))))
 ;; '(mode-line ((t (:box nil :foreground "black" :background "gray60"))))
 ;; '(mode-line-buffer-id ((t (:weight normal))))
 ;; '(mode-line-emphasis ((t (:weight normal))))
 ;; '(mode-line-highlight ((t (:box (:line-width 2 :color "light slate gray")))))
 ;; '(mode-line-inactive ((((class color) (type tty)) (:background "black" :foreground "white")) (t (:background "gray20" :foreground "black" :box nil))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(secondary-selection ((t (:inherit (region) :background "gray20"))))
 '(shadow ((t (:foreground "gray60"))))
 '(tooltip ((t (:inherit (variable-pitch) :foreground "black" :background "lightyellow"))))
 '(trailing-whitespace ((t (:inherit style-warning-face))))
 '(whitespace-tab ((t (:inherit trailing-whitespace))))
 '(show-paren-match ((t (:foreground "white" :background "black"
                         :weight ultrabold))))
 '(idle-highlight ((t (:inherit region :weight extra-bold))))
 '(dired-directory ((t (:inherit font-lock-function-name-face :underline t))))
 ;; '(success ((t (:foreground "white" :weight normal))))
 '(success ((t (:foreground "gray40" ;;:weight normal
                :underline t))))
 '(compilation-line-number ((t (:foreground "gray30" :weight normal))))
 '(show-paren-mismatch ((t (:background "firebrick3" :foreground "white" :weight extra-bold))))
 '(popup-tip-face ((t (:inherit font-lock-comment-face))))
 '(popup-face ((t (:background "gray12" :foreground "white"))))
 '(popup-isearch-match ((t (:inherit isearch))))
 '(popup-menu-face ((t (:inherit popup-face :inverse-video nil :weight normal))))
 '(popup-menu-mouse-face ((t (:inherit popup-menu-selection-face :background "blue"))))
 '(popup-menu-selection-face ((t (:inherit popup-menu-face :inverse-video t :weight ultra-bold))))
 '(popup-menu-summary-face ((t (:inherit popup-menu-face :weight normal))))
 '(popup-tip-face ((t (:inherit font-lock-comment-face :weight normal))))
 '(error ((t (:foreground "firebrick" :weight bold))))
 '(hl-line ((t (:background "#2c2c3c"))))
 '(ivy-current-match ((t (:inherit region
                          :background "SteelBlue4"
                          :foreground "white"))))
 '(ivy-minibuffer-match-face-1 ((t (:inherit underlined))))
 '(ivy-minibuffer-match-face-2 ((t (:inherit ivy-minibuffer-match-face-1))))
 '(ivy-minibuffer-match-face-3 ((t (:inherit ivy-minibuffer-match-face-1))))
 '(ivy-minibuffer-match-face-4 ((t (:inherit ivy-minibuffer-match-face-1))))
 '(swiper-match-face-1 ((t (:inherit  lazy-highlight))))
 '(swiper-match-face-2 ((t (:inherit swiper-match-face-1))))
 '(swiper-match-face-3 ((t (:inherit swiper-match-face-1))))
 '(swiper-match-face-4 ((t (:inherit swiper-match-face-1))))
 ;; avy
 '(avy-lead-face ((t (:foreground "black" :background "LimeGreen" ;;:weight normal
                      ))))
 '(avy-lead-face-0 ((t (:inherit avy-lead-face))))
 '(avy-lead-face-1 ((t (:inherit avy-lead-face))))
 '(avy-lead-face-2 ((t (:inherit avy-lead-face))))
 '(avy-goto-char-timer-face ((t (:underline (:color "LimeGreen")))))
 ;; ace-window
 '(aw-leading-char-face ((t (:inherit avy-lead-face :box nil :height 200))))
 '(aw-mode-line-face ((t (:foreground "white" :weight extra-bold))))
 '(aw-key-face ((t (:foreground "white" :weight extra-bold))))
 ;; diff
 '(diff-removed ((t (:background "#3f0001"))))
 '(diff-added ((t (:background "#002800"))))
 '(diff-refine-added ((t (:background "#006000"))))
 '(diff-refine-removed ((t (:background "#710001"))))

 ;; magit
 '(magit-header-line ((t (:foreground "white" :background "black" :weight regular ;; :underline t
                          ;; :box t
                          :underline t
                          ))))
 '(magit-diff-hunk-heading-highlight ((t (:foreground "grey80" :background "DarkOrange3"
                                          :weight bold))))
 '(magit-diff-context-highlight ((t (:foreground "grey70" :background "grey20"
                                     :weight semi-bold
                                     :underline nil))))
 '(magit-section-highlight ((t (:background  "grey10"))))
 '(magit-section-heading ((t (:foreground "#43666b" :underline t :weight bold))))
 '(magit-filename ((t (;;:weight normal
                       :inherit font-lock-string-face))))
 '(magit-diff-file-heading ((t (:extend t :weight bold
                                :inherit font-lock-keyword-face))))
 '(magit-tag ((t (:foreground "salmon"))))
 '(magit-branch-local ((t (:foreground "LightSkyBlue2"))))
 '(magit-branch-remote ((t (:foreground "DarkSeaGreen3"))))
 '(magit-log-author ((t (:foreground "tomato" :slant normal :width normal))))
 ;; '(magit-diff-removed-highlight ((t (:background "#663333"
 ;;                                     :foreground "#eecccc"
 ;;                                     :extend t))))
 ;; '(magit-section-heading ((t (:inherit magit-header-line))))
 ;; org
 ;; '(org-default ((t (:inherit default :height 140))))
 '(org-default ((t (:inherit default))))
 '(org-level-1 ((t (:inherit outline-1))))
 '(org-level-2 ((t (:inherit org-level-1 ;;:weight normal
                    ))))
 '(org-level-3 ((t (:inherit org-level-2))))
 '(org-level-4 ((t (:inherit org-default))))
 '(org-date ((t (:inherit font-lock-string-face :weight normal))))
 '(org-tag ((t (:inherit mode-line-emphasis))))
 '(org-todo ((t ;;(:foreground "#833333" :weight bold)
              (:foreground "#ee3039" :weight bold))))
 '(org-done ((t (:foreground "#55df54" ;;"#44cf44" ;;"#338333"
                 :weight bold))))
 '(org-block-begin-line ((t (:inherit default :foreground "grey50"
                             :underline t :extend t))))
 '(org-block-end-line ((t (:inherit org-block-begin-line
                           :underline nil :overline t))))
 '(org-block ((t (:inherit font-lock-comment-face :background "grey10"))))
 '(org-table ((t (:foreground "CadetBlue4"))))
 '(org-formula ((t (:foreground "CadetBlue1"))))
 '(org-headline-done ((t (:inherit org-default))))

 ;; ediff
 '(ediff-current-diff-A ((t (:inherit diff-removed))))
 '(ediff-current-diff-B ((t (:inherit diff-added))))
 '(ediff-fine-diff-A ((t (:inherit diff-refine-removed))))
 '(ediff-fine-diff-B ((t (:inherit diff-refine-added))))
 '(ediff-current-diff-C ((t (:background "#7a3002"))))
 '(ediff-fine-diff-C ((t (:background "DarkGoldenrod"))))

 '(ediff-diff-base ((t (:foreground "cadet blue" :background "grey10"))))
 '(ediff-even-diff-A ((t (:inherit ediff-even-diff-base))))
 '(ediff-even-diff-B ((t (:inherit ediff-even-diff-base))))
 '(ediff-even-diff-C ((t (:inherit ediff-even-diff-base))))
 '(ediff-odd-diff-A ((t (:inherit ediff-odd-diff-base))))
 '(ediff-odd-diff-B ((t (:inherit ediff-odd-diff-base))))
 '(ediff-odd-diff-C ((t (:inherit ediff-odd-diff-base))))

 ;;
 '(dgi-commit-message-face ((t (:inherit font-lock-string-face))))
 '(iedit-occurrence ((t (:box (:color "DarkViolet")))))
 ;; frog-menu
 '(frog-menu-posframe-background-face ((t (:background "black"))))
 '(frog-menu-candidates-face ((t (:foreground "white"))))
 ;; equake
 '(equake-shell-type-shell ((t (:foreground "white"))))
 '(sh-heredoc ((t (:inherit font-lock-string-face))))
 '(comint-highlight-input ((t (:weight bold :box "green4"))))
 ;; loccur
 '(loccur-face ((t (:inherit match :underline (:color "DarkViolet")))))
 ;; which-key
 '(which-key-separator-face ((t (:inherit font-lock-comment-face
                                 :background "black"))))
 ;; fill column
 '(fill-column-indicator ((t (:foreground "grey20"))))
 ;; which-func
 '(which-func ((t (:inherit font-lock-keyword-face :foreground "grey60"))))
 ;; mu4e
 '(mu4e-header-face ((t (:inherit font-lock-string-face :weight normal))))
 '(mu4e-header-title-face ((t (:inherit font-lock-type-face))))
 '(mu4e-header-value-face ((t (:inherit font-lock-type-face))))
 '(mu4e-unread-face ((t (:inherit font-lock-keyword-face :weight normal))))
 '(mu4e-header-highlight-face ((t (:inherit hl-line :extend t
                                   :underline nil :weight normal))))
 '(mu4e-related-face ((t (:inherit mu4e-header-face :slant italic))))
 ;; js2-mode
 '(js2-function-param ((t (:inherit default))))
 '(js2-jsdoc-value ((t (:inherit js2-jsdoc-tag))))
 '(js2-jsdoc-type ((t (:inherit js2-jsdoc-tag))))
 ;; w3m-mode
 '(w3m-anchor ((t (:inherit font-lock-keyword-face :underline t))))
 '(w3m-header-line-title ((t (:inherit default :foreground "cadet blue"
                              :weight normal))))
 '(w3m-header-line-content ((t (:inherit w3m-header-line-title :bold nil))))
 ;; w3m-mode tabs
 '(w3m-tab-base ((t (:inherit default :background "grey20" :weight normal))))
 '(w3m-tab-background ((t (:inherit w3m-tab-base))))
 '(w3m-tab-selected ((t (:inherit w3m-tab-base :inverse-video t))))
 '(w3m-tab-selected-background ((t (:inherit w3m-tab-base))))
 '(w3m-tab-selected-retrieving ((t (:inherit w3m-tab-selected :background "red"))))
 '(w3m-tab-unselected ((t (:inherit w3m-tab-base))))
 '(w3m-tab-unselected-unseen ((t (:inherit w3m-tab-base))))
 '(w3m-tab-unselected-retrieving ((t (:inherit w3m-tab-base :underline "red"))))
 '(w3m-tab-mouse ((t (:inherit w3m-tab-base
                      :background "Gray75" :foreground "white"))))
 ;; tldr
 '(tldr-title ((t (:weight bold :height 1.2))))
 '(tldr-description ((t (:inherit default))))
 '(tldr-command-itself ((t (:inherit font-lock-keyword-face))))
 '(tldr-code-block ((t (:inherit font-lock-string-face))))
 '(tldr-command-argument ((t (:foreground "#eee"))))
 '(tldr-command-argument ((t (:inherit default))))
 ;; denote
 '(denote-dired-field-date ((t (:foreground "#354b0f"))))
 '(denote-dired-field-keywords ((t (:inherit bold :foreground "light slate gray"))))
 ;; gnus
 '(gnus-header-from ((t (:inherit magit-log-author))))
 '(gnus-header-subject ((t (:foreground "white"))))
 '(gnus-header-newsgroups ((t (:foreground "yellow" :slant italic))))
 '(gnus-header-name ((t (:foreground "grey45"))))
 '(gnus-header-content ((t (:foreground "light gray" :slant italic))))
 ;; ein
 '(ein:codecell-input-area-face ((t (:inherit default))))
 '(ein:codecell-input-prompt-face ((t (:inherit cursor))))
 ;; slime
 '(sldb-restartable-frame-line-face ((t (:foreground "lime green"))))
 '(sldb-restartable-frame-line-face ((t (:inherit bold :foreground "light slate gray"))))
 '(sldb-restartable-frame-line-face ((t (:inherit bold :foreground "gray60"))))
 ;;'(sldb-restartable-frame-line-face ((t (:inherit 'default))))
 ;; transient
 '(transient-argument ((t (:inherit font-lock-string-face :weight bold
                           :foreground "lime green"))))
 )

(provide-theme 'quasi-monochrome)
