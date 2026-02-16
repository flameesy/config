;;; dashboard.el --- Enhanced Solo Developer Dashboard  -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Dashboard - Optimized for Solo Developer Workflow
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Icons first (needed by dashboard)
(use-package nerd-icons
  :ensure t
  :if (display-graphic-p))

(use-package dashboard
  :ensure t
  :after (nerd-icons)
  :init
  ;; Set the title with time-based greeting
  (setq dashboard-banner-logo-title 
        (format "%s - %s" 
                (let ((hour (string-to-number (format-time-string "%H"))))
                  (cond ((< hour 12) "Good Morning")
                        ((< hour 18) "Good Afternoon")
                        (t "Good Evening")))
                "Let's have a great time"))
  
  ;; Set the banner
  (setq dashboard-startup-banner 
        (cons (concat user-emacs-directory "extras/logo.png") "alt_pic"))
  
  ;; Center content
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  
  ;; Items to show - reordered by workflow priority
  (setq dashboard-items '((agenda    . 10)   ; Most important: what's due
                          (projects  . 8)    ; Active projects
                          (recents   . 7)    ; Recently edited
                          (bookmarks . 5)))  ; Important locations
  
  ;; Show icons
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  
  ;; Format path
  (setq dashboard-path-style 'truncate-middle)
  (setq dashboard-path-max-length 60)  ; Increased for better context
  
  ;; Show init info
  (setq dashboard-set-init-info t)
  (setq dashboard-init-info 
        (format "Ready in %.2f seconds with %d packages"
                (float-time (time-subtract after-init-time before-init-time))
                (length package-activated-list)))
  
  ;; Custom footer with productivity tips
  (setq dashboard-footer-messages 
        '("pluh"))
  
  ;; Projects backend
  (setq dashboard-projects-backend 'project-el)
  
  ;; Org-agenda integration - critical for project management
  (setq dashboard-week-agenda t)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  
  ;; Custom sections for solo developer workflow
  (setq dashboard-startupify-list
        '(dashboard-insert-banner
          dashboard-insert-newline
          dashboard-insert-banner-title
          dashboard-insert-newline
          dashboard-insert-navigator
          dashboard-insert-newline
          dashboard-insert-init-info
          dashboard-insert-items
          dashboard-insert-newline
          dashboard-insert-footer))
  
  :config
  
  ;; Enhanced navigator with solo dev workflow in mind
  (setq dashboard-navigator-buttons
        `(;; Row 1: Code & Version Control
          ((,""
            "GitHub"
            "Browse repositories"
            (lambda (&rest _) (browse-url "https://github.com/flameesy")))
           (,""
            "Projects"
            "Switch project"
            (lambda (&rest _) (call-interactively 'project-switch-project)))
           (,""
            "Notes"
            "Quick note"
            (lambda (&rest _) (org-capture))))
          
          ;; Row 2: Communication & Management
          ((,""
            "Email"
            "Check messages"
            (lambda (&rest _) (call-interactively 'notmuch)))  ; or mu4e
           (,""
            "Agenda"
            "Full agenda view"
            (lambda (&rest _) (org-agenda nil "a")))
           (""
            "Time"
            "Clock in/out"
            (lambda (&rest _) (org-clock-goto))))
          
          ;; Row 3: Maintenance & Tools
          ((,""
            "Update"
            "Update packages"
            (lambda (&rest _) (package-refresh-contents)))
           (,""
            "Config"
            "Edit init"
            (lambda (&rest _) (find-file user-init-file)))
           (,""
            "Docs"
            "Open documentation"
            (lambda (&rest _) (info))))))
  
  ;; Custom dashboard heading names
  (setq dashboard-heading-icons
        '((recents   . "nf-oct-history")
          (bookmarks . "nf-oct-bookmark")
          (agenda    . "nf-oct-calendar")
          (projects  . "nf-oct-repo")))
  
  (dashboard-setup-startup-hook)
  
  ;; Show dashboard in new frames
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))))

;; Project.el enhanced configuration
(use-package project
  :ensure nil
  :config
  (setq project-switch-commands
        '((project-find-file "Find file" "f")
          (project-find-dir "Find directory" "d")
          (project-dired "Dired" "D")
          (project-vc-dir "VC-Dir" "v")
          (project-eshell "Eshell" "e")
          (magit-project-status "Magit" "g")  ; If using magit
          (project-compile "Compile" "c"))))

;; Enhanced key bindings
(global-set-key (kbd "C-c d") 'dashboard-refresh-buffer)
(global-set-key (kbd "C-c h") 'dashboard-open)  ; Quick return to dashboard

;; Optional: Add a dashboard item counter in mode-line
(defun my/dashboard-mode-line ()
  "Custom mode line for dashboard."
  (setq mode-line-format
        '("%e" mode-line-front-space
          "Dashboard | "
          (:eval (format-time-string "%H:%M | %a %b %d"))
          " | "
          (:eval (format "%d open buffers" (length (buffer-list))))
          mode-line-end-spaces)))

(add-hook 'dashboard-mode-hook 'my/dashboard-mode-line)

;; Optional: Dashboard widgets for solo dev productivity
(defun my/dashboard-insert-quick-stats (list-size)
  "Insert productivity stats."
  (dashboard-insert-heading "Quick Stats"
                           (nerd-icons-mdicon "nf-md-chart_line")
                           (dashboard-get-shortcut 'quick-stats))
  (insert "\n")
  (insert (format "    • Org tasks: %d\n" (length (org-map-entries t))))
  (insert (format "    • Git repos: %d\n" (length (project-known-project-roots))))
  (insert (format "    • Active time today: %s\n" 
                  (or (org-clock-sum-today) "0:00")))
  (insert "\n"))

;; Add the custom widget (optional)
;; (add-to-list 'dashboard-item-generators '(quick-stats . my/dashboard-insert-quick-stats))
;; (add-to-list 'dashboard-items '(quick-stats) t)

(provide 'dashboard)
;;; dashboard.el ends here