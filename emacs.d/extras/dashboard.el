;;; Emacs Bedrock
;;;
;;; Extra config: Dashboard

;;; Usage: Append or require this file from init.el to get a nice startup dashboard

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Dashboard
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Icons first (needed by dashboard)
(use-package nerd-icons
  :ensure t
  :if (display-graphic-p))

(use-package dashboard
  :ensure t
  :after (nerd-icons)  ; Load after nerd-icons
  :init
  ;; Set the title
  (setq dashboard-banner-logo-title "Enjoi.")
  
  ;; Set the banner
  (setq dashboard-startup-banner 'logo)
  
  ;; Center content
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)

  ;; Items to show on dashboard
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 5)))

  ;; Show icons
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)

  ;; Format path
  (setq dashboard-path-style 'truncate-middle)
  (setq dashboard-path-max-length 50)

  ;; Show init info
  (setq dashboard-set-init-info t)

  ;; Footer
  (setq dashboard-footer-messages 
        '("pluh"))

  ;; Projects backend
  (setq dashboard-projects-backend 'project-el)

  ;; Org-agenda integration
  (setq dashboard-week-agenda t)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)

  :config
  
  ;; Navigator buttons (now nerd-icons is loaded)
  (setq dashboard-navigator-buttons
        `((,(if (display-graphic-p)
                (nerd-icons-faicon "nf-fa-github" :height 1.1 :v-adjust 0.0)
              "")
           "GitHub"
           "Browse GitHub"
           (lambda (&rest _) (browse-url "https://github.com")))
          (,(if (display-graphic-p)
                (nerd-icons-faicon "nf-fa-refresh" :height 1.1 :v-adjust 0.0)
              "")
           "Update"
           "Update packages"
           (lambda (&rest _) (package-refresh-contents)))))

  (dashboard-setup-startup-hook)
  
  ;; Show dashboard in new frames
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))))

;; Project.el integration
(use-package project
  :ensure nil
  :config
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-find-dir "Find directory")
          (project-dired "Dired"))))

;; Key bindings
(global-set-key (kbd "C-c d") 'dashboard-refresh-buffer)