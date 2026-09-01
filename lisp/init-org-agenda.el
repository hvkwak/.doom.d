;;; lisp/init-org-agenda.el --- org-agenda configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; GTD-ish agenda setup: TODO workflow, capture templates, refile,
;;; logging, agenda display tuning, and a single-key "Dashboard" view.
;;; Code:

;; `org-directory' is set in init-org.el (must happen before org loads).

(after! org
  ;; ---------------------------------------------------------------------------
  ;; 1. Core Agenda & Files
  ;; ---------------------------------------------------------------------------
  (setq org-agenda-files '("~/Documents/notes/2026/2026TODOs.org"
                           "~/Documents/notes/2026/2026StudyTODOs.org"
                           "~/Documents/notes/2026/2026WorkTODOs.org")
        org-default-notes-file "~/Documents/notes/2026/2026TODOs.org")

  ;; ---------------------------------------------------------------------------
  ;; 2. TODO Workflow & Colors
  ;; ---------------------------------------------------------------------------
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|"
                    "DONE(d!)" "OKAY(o)" "NO(x)" "KILL(k@)"))
        org-todo-repeat-to-state "TODO")

  ;; Modern & vibrant keyword colors
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "#e53935" :weight bold)   ; Bright Crimson
          ("NEXT" :foreground "#2563eb" :weight bold)   ; Electric Blue
          ("WAIT" :foreground "#d97706" :weight bold)   ; Warm Amber
          ("DONE" :foreground "#059669" :weight bold)   ; Emerald Green
          ("OKAY" :foreground "#0d9488" :weight bold)   ; Teal
          ("NO"   :foreground "#6b7280" :weight bold)   ; Slate Gray
          ("KILL" :foreground "#4b5563" :weight bold))) ; Dark Slate

  ;; ---------------------------------------------------------------------------
  ;; 3. Logging & Refile
  ;; ---------------------------------------------------------------------------
  (setq org-log-done 'time
        org-log-into-drawer t
        org-log-reschedule 'time
        org-log-redeadline 'time
        
        org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)

  ;; ---------------------------------------------------------------------------
  ;; 4. Visual & UI Formatting Settings
  ;; ---------------------------------------------------------------------------
  (setq org-priority-default ?C
        org-priority-lowest ?D
        org-agenda-start-on-weekday nil
        org-agenda-span 'day
        org-agenda-start-day "0d"
        org-deadline-warning-days 14
        
        ;; Hide redundant entries
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-timestamp-if-done t
        
        ;; Aesthetic Layout Tuning
        org-agenda-tags-column -100
        org-agenda-block-separator ?─
        org-agenda-compact-blocks nil
        
        ;; Modern Time Grid Layout
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "─────────────────")
        org-agenda-current-time-string "◄─ NOW ───────────────────"
        
        org-agenda-window-setup 'current-window
        org-agenda-restore-windows-after-quit t)

  ;; Add vertical breathing room in agenda buffer
  (add-hook 'org-agenda-mode-hook (lambda () (setq line-spacing 0.15)))

  ;; ---------------------------------------------------------------------------
  ;; 5. Capture Templates
  ;; ---------------------------------------------------------------------------
  (let ((study-file "~/Documents/notes/2026/2026StudyTODOs.org")
        (orga-file  "~/Documents/notes/2026/2026TODOs.org")
        (work-file  "~/Documents/notes/2026/2026WorkTODOs.org"))

    (setq org-capture-templates
          `(("s" "📚 Study")
            ("st" "Study: todo" entry
             (file+headline ,study-file "Inbox")
             "* TODO %?\n%U\n%i" :empty-lines 1)
            ("ss" "Study: todo, scheduled" entry
             (file+headline ,study-file "Inbox")
             "* TODO %?\nSCHEDULED: %^{when}t\n%U\n%i" :empty-lines 1)
            ("sd" "Study: todo, deadline" entry
             (file+headline ,study-file "Inbox")
             "* TODO %?\nDEADLINE: %^{due}t\n%U\n%i" :empty-lines 1)

            ("o" "📁 Orga")
            ("ot" "Orga: todo" entry
             (file+headline ,orga-file "Inbox")
             "* TODO %?\n%U\n%i" :empty-lines 1)
            ("os" "Orga: todo, scheduled" entry
             (file+headline ,orga-file "Inbox")
             "* TODO %?\nSCHEDULED: %^{when}t\n%U\n%i" :empty-lines 1)
            ("od" "Orga: todo, deadline" entry
             (file+headline ,orga-file "Inbox")
             "* TODO %?\nDEADLINE: %^{due}t\n%U\n%i" :empty-lines 1)

            ("w" "💼 Work")
            ("wt" "Work: todo" entry
             (file+headline ,work-file "Inbox")
             "* TODO %?\n%U\n%i" :empty-lines 1)
            ("ws" "Work: todo, scheduled" entry
             (file+headline ,work-file "Inbox")
             "* TODO %?\nSCHEDULED: %^{when}t\n%U\n%i" :empty-lines 1)
            ("wd" "Work: todo, deadline" entry
             (file+headline ,work-file "Inbox")
             "* TODO %?\nDEADLINE: %^{due}t\n%U\n%i" :empty-lines 1)

            ("n" "📝 Quick Note -> Orga inbox" entry
             (file+headline ,orga-file "Inbox")
             "* %?\n%U\n%i" :empty-lines 1))))

  ;; ---------------------------------------------------------------------------
  ;; 6. Custom Agenda Views
  ;; ---------------------------------------------------------------------------
  (setq org-agenda-custom-commands
        '(("d" "Dashboard - Today & Upcoming"
           ((agenda ""
                    ((org-agenda-span 'day)
                     (org-agenda-start-day "0d")
                     (org-agenda-start-with-log-mode t)
                     (org-agenda-skip-scheduled-if-done nil)
                     (org-agenda-skip-deadline-if-done nil)
                     (org-agenda-skip-timestamp-if-done nil)
                     (org-agenda-overriding-header "⚡ Today Dashboard")
                     (org-super-agenda-groups
                      '(;; Overdue tasks take top priority so they aren't missed
                        (:name "⚠️ Overdue"
                               :and (:scheduled past :not (:log t))
                               :and (:deadline past :not (:log t)))
                        ;; Done tasks completed today
                        (:name "✔ Done - Study" :and (:category "Study" :log t))
                        (:name "✔ Done - Orga"  :and (:category "Orga" :log t))
                        (:name "✔ Done - Work"  :and (:category "Work" :log t))
                        ;; Active tasks scheduled/deadlined for today
                        (:name "📚 Today - Study" :and (:category "Study" :scheduled today))
                        (:name "📚 Today - Study" :and (:category "Study" :deadline today))
                        (:name "📁 Today - Orga"  :and (:category "Orga" :scheduled today))
                        (:name "📁 Today - Orga"  :and (:category "Orga" :deadline today))
                        (:name "💼 Today - Work"  :and (:category "Work" :scheduled today))
                        (:name "💼 Today - Work"  :and (:category "Work" :deadline today))
                        (:discard (:anything t))))))
            (agenda ""
                    ((org-agenda-span 6)
                     (org-agenda-start-day "+1d")
                     (org-deadline-warning-days 0)
                     (org-agenda-overriding-header "📅 Upcoming 6 Days")
                     (org-super-agenda-groups nil)
                     ;; (org-super-agenda-groups
                     ;;  '((:name "📚 Study" :category "Study")
                     ;;    (:name "📁 Orga"  :category "Orga")
                     ;;    (:name "💼 Work"  :category "Work")))
                     ))))

          ("S" "Study Plan" alltodo ""
           ((org-agenda-files '("~/Documents/notes/2026/2026StudyTODOs.org"))
            (org-agenda-overriding-header "📚 Study Plan Backlog")
            (org-super-agenda-groups
             '((:name "🚀 In Progress" :todo "NEXT")
               (:name "📅 Scheduled"   :scheduled t)
               (:auto-parent t)))))

          ("O" "Organisatorisches Plan" alltodo ""
           ((org-agenda-files '("~/Documents/notes/2026/2026TODOs.org"))
            (org-agenda-overriding-header "📁 Organisatorisches Backlog")
            (org-super-agenda-groups
             '((:name "🚀 In Progress" :todo "NEXT")
               (:name "📅 Scheduled"   :scheduled t)
               (:auto-parent t)))))

          ("W" "Work Plan" alltodo ""
           ((org-agenda-files '("~/Documents/notes/2026/2026WorkTODOs.org"))
            (org-agenda-overriding-header "💼 Work Backlog")
            (org-super-agenda-groups
             '((:name "🚀 In Progress" :todo "NEXT")
               (:name "📅 Scheduled"   :scheduled t)
               (:auto-parent t)))))

          ("r" "Three-Week Review" agenda ""
           ((org-agenda-span 21)
            (org-agenda-start-on-weekday 1)
            (org-deadline-warning-days 0)
            (org-agenda-overriding-header "🔍 Three-Week Review")
            (org-super-agenda-groups
             '((:name "📚 Study" :category "Study")
               (:name "📁 Orga"  :category "Orga")
               (:name "💼 Work"  :category "Work"))))))))
;; -----------------------------------------------------------------------------
;; 7. org-super-agenda Setup
;; -----------------------------------------------------------------------------
(use-package! org-super-agenda
  :after org-agenda
  :config
  ;; Fix Evil keybinding issues on header lines
  (setq org-super-agenda-header-map (make-sparse-keymap))
  
  ;; 헤더 좌측의 자동 들여쓰기 공백 제거 (핵심!)
  (setq org-super-agenda-header-prefix "")

  ;; Global fallbacks for plain views
  (setq org-super-agenda-groups
        '((:name "⚠️ Overdue" :scheduled past :deadline past)
          (:name "⚡ Today"   :time-grid t :scheduled today :deadline today)
          (:name "📚 Study"   :category "Study")
          (:name "📁 Orga"    :category "Orga")
          (:name "💼 Work"    :category "Work")))
  (org-super-agenda-mode))

(provide 'init-org-agenda)
;;; init-org-agenda.el ends here
