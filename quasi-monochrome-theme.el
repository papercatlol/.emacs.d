(deftheme quasi-monochrome
  "Created 2018-06-04.")

(custom-theme-set-faces
 'quasi-monochrome
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "light gray" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "xos4" :family "Terminus"))))
 '(button ((t (:inherit (link)))))
 '(cursor ((t (:background "gainsboro" :foreground "black"))))
 '(bold ((t (:inherit (default) :weight bold))))
 '(escape-glyph ((t (:foreground "light gray"))))
 '(font-lock-builtin-face ((t (:foreground "light gray"))))
 '(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:background "gray20" :foreground "gray60"))))
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
 '(fringe ((t (:background "gray20"))))
 '(header-line ((t (:background "gray15" :foreground "white"))))
 '(highlight ((t (:background "gray20"))))
 '(isearch ((t (:background "dark red" :foreground "black" :weight bold))))
 '(lazy-highlight ((t (:background "firebrick" :foreground "black" :weight bold))))
 '(isearch-fail ((t (:background "red4"))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "light gray"))))
 '(link-visited ((t (:underline (:color foreground-color :style line) :foreground "light gray"))))
 '(match ((t (:background "RoyalBlue3"))))
 '(minibuffer-prompt ((t (:weight bold :foreground "light gray"))))
 '(mode-line ((t (:box nil :foreground "black" :background "gray60"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "light slate gray")))))
 '(mode-line-inactive ((((class color) (type tty)) (:background "black" :foreground "white")) (t (:background "gray20" :foreground "black" :box nil))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(secondary-selection ((t (:background "gray15"))))
 '(shadow ((t (:foreground "gray60"))))
 '(tooltip ((t (:inherit (variable-pitch) :foreground "black" :background "lightyellow"))))
 '(trailing-whitespace ((t (:background "firebrick"))))
 '(show-paren-match ((t (:underline t :foreground "white" :background "black"))))
 '(idle-highlight ((t (:inherit region :weight extra-bold))))
 '(dired-directory ((t (:inherit font-lock-function-name-face :underline t))))
 '(success ((t (:foreground "white" :weight bold))))
 '(show-paren-mismatch ((t (:background "firebrick3" :foreground "white" :weight extra-bold))))
 '(match ((t (:background "white" :foreground "black" :weight bold))))
 '(region ((t (:background "dark orange" :foreground "black"))))
 '(aw-leading-char-face ((t (:background "black" :foreground "white" :weight extra-bold))))
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
 '(ivy-minibuffer-match-face-1 ((t (:inherit lazy-highlight))))
 '(ivy-minibuffer-match-face-2 ((t (:inherit lazy-highlight))))
 '(ivy-minibuffer-match-face-3 ((t (:inherit lazy-highlight))))
 '(ivy-minibuffer-match-face-4 ((t (:inherit lazy-highlight))))
 '(ivy-current-match ((t (:background "white" :foreground "black" :weight extra-bold))))
 )

(provide-theme 'quasi-monochrome)
