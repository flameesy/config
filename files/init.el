;;; init.el --- Ultimate Common Lisp Development Environment
;;; Commentary:
;; Comprehensive Emacs configuration for Common Lisp development with CLOG
;; Author: Your Name
;; Created: 2026-02-15

;;; Code:

;; ============================================================================
;; PERFORMANCE OPTIMIZATION
;; ============================================================================

;; Increase garbage collection threshold during startup
(setq gc-cons-threshold (* 50 1000 1000))

;; Restore after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))

;; ============================================================================
;; PACKAGE MANAGEMENT
;; ============================================================================

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ============================================================================
;; THEME & UI
;; ============================================================================

(use-package nord-theme
  :config
  (load-theme 'nord t)
  (setq nord-uniform-mode-lines t))

;; Clean UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

;; Font (adjust size as needed)
(set-face-attribute 'default nil :font "Fira Code" :height 120)
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 120)
(set-face-attribute 'variable-pitch nil :font "DejaVu Sans" :height 120)

;; Icons (must run M-x all-the-icons-install-fonts after first start)
(use-package all-the-icons
  :if (display-graphic-p))

;; Dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-items '((recents  . 10)
                         (projects . 10)
                         (bookmarks . 5))))

;; ============================================================================
;; BASIC EDITOR SETTINGS
;; ============================================================================

;; Line numbers
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Highlight current line
(global-hl-line-mode t)

;; Show matching parens
(show-paren-mode t)
(setq show-paren-delay 0)

;; Column number in modeline
(column-number-mode t)

;; Delete selection when typing
(delete-selection-mode t)

;; Auto-revert files when changed on disk
(global-auto-revert-mode t)

;; Save place in files
(save-place-mode t)

;; Better scrolling
(setq scroll-conservatively 101
      scroll-margin 3
      scroll-preserve-screen-position t)

;; Use spaces, not tabs
(setq-default indent-tabs-mode nil
              tab-width 4)

;; UTF-8 everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Backup and autosave settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Remember recently opened files
(recentf-mode t)
(setq recentf-max-saved-items 100)

;; ============================================================================
;; KEYBINDING DISCOVERY
;; ============================================================================

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

;; ============================================================================
;; COMPLETION FRAMEWORK (IVY/COUNSEL/SWIPER)
;; ============================================================================

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package swiper)

;; Better ivy interface
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; ============================================================================
;; PROJECT MANAGEMENT
;; ============================================================================

(use-package projectile
  :diminish projectile-mode
  :config 
  (projectile-mode)
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-indexing-method 'alien)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects"))))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; ============================================================================
;; FILE MANAGEMENT
;; ============================================================================

(use-package treemacs
  :defer t
  :bind (("M-0" . treemacs-select-window)
         ("C-x t 1" . treemacs-delete-other-windows)
         ("C-x t t" . treemacs)
         ("C-x t B" . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag))
  :config
  (setq treemacs-width 30
        treemacs-follow-mode t
        treemacs-filewatch-mode t))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

;; Dired improvements
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-dwim-target t))

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; ============================================================================
;; VERSION CONTROL (GIT)
;; ============================================================================

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Show git changes in margin
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (global-diff-hl-mode))

;; Time machine for git history
(use-package git-timemachine
  :defer t)

;; ============================================================================
;; CODE COMPLETION
;; ============================================================================

(use-package company
  :diminish
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t))

;; ============================================================================
;; SNIPPETS
;; ============================================================================

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

;; ============================================================================
;; SYNTAX CHECKING
;; ============================================================================

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; ============================================================================
;; NAVIGATION
;; ============================================================================

(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; ============================================================================
;; EDITING ENHANCEMENTS
;; ============================================================================

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; ============================================================================
;; COMMON LISP DEVELOPMENT
;; ============================================================================

;; SLY - Superior Lisp Interaction Mode for Emacs
(use-package sly
  :config
  (setq inferior-lisp-program "sbcl"  ; Change to your CL implementation
        sly-lisp-implementations
        '((sbcl ("sbcl" "--dynamic-space-size" "2048"))
          (ccl ("ccl"))
          (ecl ("ecl"))))
  :hook (lisp-mode . sly-mode))

;; Paredit - Structural editing of S-expressions
(use-package paredit
  :diminish
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (scheme-mode . paredit-mode)
         (ielm-mode . paredit-mode)))

;; Rainbow delimiters - Colorize nested parentheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Aggressive indent - Auto-formatting
(use-package aggressive-indent
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode)))

;; Highlight matching parentheses
(use-package highlight-parentheses
  :diminish
  :hook (prog-mode . highlight-parentheses-mode))

;; ============================================================================
;; DATABASE (SQL/POSTGRESQL)
;; ============================================================================

(use-package sql
  :config
  (setq sql-postgres-login-params
        '((user :default "postgres")
          (database :default "businessapp")
          (server :default "localhost")
          (port :default 5432)))
  
  ;; Auto-uppercase SQL keywords
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode))

(use-package sqlup-mode
  :diminish
  :hook ((sql-mode . sqlup-mode)
         (sql-interactive-mode . sqlup-mode)))

;; ============================================================================
;; ORG MODE - NOTE-TAKING & PROJECT MANAGEMENT
;; ============================================================================

(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :config
  (setq org-directory "~/org"
        org-default-notes-file (concat org-directory "/notes.org")
        org-agenda-files (list org-directory)
        
        ;; Better appearance
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-startup-indented t
        org-startup-folded 'content
        
        ;; TODO states
        org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))
        
        ;; Priorities
        org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success))
        
        ;; Babel - execute code blocks
        org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t))
  
  ;; Enable languages for code execution
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (lisp . t)
     (python . t)
     (shell . t)
     (sql . t))))

;; Org capture templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        
        ("b" "Bug" entry (file+headline "~/org/bugs.org" "Bugs")
         "* TODO [#A] %?\n  DEADLINE: %^t\n  Context: %a\n  %i")
        
        ("f" "Feature" entry (file+headline "~/org/features.org" "Features")
         "* TODO %?\n  Business Value: \n  Technical Notes: \n  %i")
        
        ("s" "Security Issue" entry (file+headline "~/org/security.org" "Security")
         "* TODO [#A] %?\n  Severity: \n  Impact: \n  Mitigation: \n  %a")
        
        ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
         "* %?\n  %U\n  %i")
        
        ("m" "Meeting" entry (file+headline "~/org/meetings.org" "Meetings")
         "* %?\n  %U\n  Attendees: \n  Notes: ")))

;; Org keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Better org appearance
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

;; Modern org appearance
(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star 'replace
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2))

;; ============================================================================
;; ORG-ROAM - ZETTELKASTEN NOTE SYSTEM
;; ============================================================================

(use-package org-roam
  :custom
  (org-roam-directory "~/org-roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  
  ;; Templates for different note types
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          
          ("p" "project" plain
           "* Overview\n%?\n* Goals\n\n* Tasks\n\n* Resources\n\n"
           :target (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :project:\n")
           :unnarrowed t)
          
          ("c" "concept" plain
           "* Definition\n%?\n* Examples\n\n* Related Concepts\n\n"
           :target (file+head "concepts/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :concept:\n")
           :unnarrowed t)
          
          ("b" "bug" plain
           "* Description\n%?\n* Reproduction Steps\n\n* Expected Behavior\n\n* Actual Behavior\n\n* Solution\n\n"
           :target (file+head "bugs/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :bug:\n")
           :unnarrowed t))))

;; ============================================================================
;; DEFT - QUICK NOTE SEARCH
;; ============================================================================

(use-package deft
  :bind ("C-c n d" . deft)
  :commands (deft)
  :config
  (setq deft-directory "~/org"
        deft-extensions '("org" "md" "txt")
        deft-recursive t
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t))

;; ============================================================================
;; MUSIC PLAYER (EMMS)
;; ============================================================================

(use-package emms
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  
  (setq emms-source-file-default-directory "~/Music/"
        emms-playlist-buffer-name "*Music*"
        emms-info-asynchronously t
        emms-show-format "♪ %s")
  
  ;; Use mpv as player
  (require 'emms-player-mpv)
  (add-to-list 'emms-player-list 'emms-player-mpv)
  
  :bind (("C-c m p" . emms-pause)
         ("C-c m n" . emms-next)
         ("C-c m b" . emms-previous)
         ("C-c m s" . emms-stop)
         ("C-c m m" . emms-smart-browse)
         ("C-c m l" . emms-playlist-mode-go)))

;; ============================================================================
;; WEB DEVELOPMENT
;; ============================================================================

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.js\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (css-mode . emmet-mode)))

;; ============================================================================
;; REST CLIENT (for API testing)
;; ============================================================================

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

;; ============================================================================
;; TERMINAL
;; ============================================================================

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

;; Better shell
(use-package eshell
  :bind ("C-c e" . eshell)
  :config
  (setq eshell-history-size 10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

;; ============================================================================
;; HELPFUL - BETTER HELP SYSTEM
;; ============================================================================

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-c C-d" . helpful-at-point)))

;; ============================================================================
;; CUSTOM KEYBINDINGS
;; ============================================================================

;; Window management
(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x -") 'split-window-vertically)

;; Buffer management
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Quick file access
(defun open-init-file ()
  "Open init.el file."
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c i") 'open-init-file)

;; Comment/uncomment region
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

;; ============================================================================
;; CUSTOM FUNCTIONS
;; ============================================================================

;; Create directories when opening files
(defun my/create-non-existent-directory ()
  "Ask to create parent directories if they don't exist."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'my/create-non-existent-directory)

;; Open current file in external program
(defun my/open-in-external-app ()
  "Open the current file in external application."
  (interactive)
  (let ((file-path (buffer-file-name)))
    (if file-path
        (shell-command (concat "xdg-open " (shell-quote-argument file-path)))
      (message "No file associated with this buffer"))))

(global-set-key (kbd "C-c o") 'my/open-in-external-app)

;; ============================================================================
;; MODELINE
;; ============================================================================

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-buffer-encoding t
        doom-modeline-indent-info nil
        doom-modeline-checker-simple-format t
        doom-modeline-vcs-max-length 12
        doom-modeline-env-version t
        doom-modeline-project-detection 'projectile
        doom-modeline-lsp t
        doom-modeline-github nil
        doom-modeline-github-interval (* 30 60)))

;; ============================================================================
;; DIMINISH - HIDE MINOR MODES FROM MODELINE
;; ============================================================================

(use-package diminish)

;; ============================================================================
;; FINAL SETUP
;; ============================================================================

;; Create necessary directories
(dolist (dir '("~/org" "~/org-roam" "~/projects" "~/Music"))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; Server mode (allows emacsclient)
(server-start)

;; Display startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))