;; -*- lexical-binding: t -*-

(require 'org-jira)

(setq org-jira-working-dir (expand-file-name "org-jira" user-emacs-directory))

(setq jiralib-url "https://franz-projects.atlassian.net")
(setq jiralib-user "ivanl@franz.com")

;; default keybindings
;;(define-key org-jira-map (kbd "C-c pg") 'org-jira-get-projects)
;;(define-key org-jira-map (kbd "C-c ib") 'org-jira-browse-issue)
;;(define-key org-jira-map (kbd "C-c ig") 'org-jira-get-issues)
;;(define-key org-jira-map (kbd "C-c ij") 'org-jira-get-issues-from-custom-jql)
;;(define-key org-jira-map (kbd "C-c ih") 'org-jira-get-issues-headonly)
;;(define-key org-jira-map (kbd "C-c iu") 'org-jira-update-issue)
;;(define-key org-jira-map (kbd "C-c iw") 'org-jira-progress-issue)
;;(define-key org-jira-map (kbd "C-c in") 'org-jira-progress-issue-next)
;;(define-key org-jira-map (kbd "C-c ia") 'org-jira-assign-issue)
;;(define-key org-jira-map (kbd "C-c ir") 'org-jira-refresh-issue)
;;(define-key org-jira-map (kbd "C-c iR") 'org-jira-refresh-issues-in-buffer)
;;(define-key org-jira-map (kbd "C-c ic") 'org-jira-create-issue)
;;(define-key org-jira-map (kbd "C-c ik") 'org-jira-copy-current-issue-key)
;;(define-key org-jira-map (kbd "C-c sc") 'org-jira-create-subtask)
;;(define-key org-jira-map (kbd "C-c sg") 'org-jira-get-subtasks)
;;(define-key org-jira-map (kbd "C-c cc") 'org-jira-add-comment)
;;(define-key org-jira-map (kbd "C-c cu") 'org-jira-update-comment)
;;(define-key org-jira-map (kbd "C-c wu") 'org-jira-update-worklogs-from-org-clocks)
;;(define-key org-jira-map (kbd "C-c tj") 'org-jira-todo-to-jira)
;;(define-key org-jira-map (kbd "C-c if") 'org-jira-get-issues-by-fixversion)


(provide 'configure-jira)
