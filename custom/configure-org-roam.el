;; -*- lexical-binding: t -*-

;; `org-roam' config, mostly stolen from abo-abo:
;; https://oremacs.com/2020/12/31/happy-new-year/
;; https://github.com/abo-abo/oremacs/blob/github/modes/ora-org-roam.el

;;* vars
(setq org-roam-directory (expand-file-name "roam" org-directory))
(setq org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
(setq org-roam-buffer-position 'bottom)
(setq org-roam-completion-system 'ivy)

;;* [experimental] completion everywhere
(setq org-roam-completion-everywhere t)

;;* templates
(setq org-roam-capture-templates
      '(("d"
         "default"
         plain
         #'org-roam-capture--get-point
         "%?"
         :file-name "%<%Y-%m-%d_%H:%M>-${slug}"
         :head "#+title: ${title}\n"
         :unnarrowed t)))

;;* enable
(org-roam-mode)

;;* TODO: org-roam-capture: parse #tags from title on new capture
;; (capture "test #foo #bar") => new file "test" with tags #foo and #bar


;;* TODO: org-roam-capture-ref-templates, org-roam-protocol

;;* hydra-org-roam
(defhydra hydra-org-roam (:exit t)
  "org-roam"
  ("i" org-roam-insert "insert")
  ("f" org-roam-find-file "find-file")
  ("<f6>" org-roam-capture "capture")
  ("r" org-roam-random-note "random")
  ("v" org-roam-buffer-activate "view backlinks")
  ;; ("b" ora-org-roam-find-backlink "find backlink")
  ;; ("t" ora-roam-todo "todo")
  ("j" org-roam-find-index "index")

  ;; inside org roam file
  ("t" org-roam-tag-add "add tag")
  ("a" org-roam-alias-add "add alias"))


(provide 'configure-org-roam)
