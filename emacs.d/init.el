;;; init.el --- Initial stuff -*- lexical-binding: t; -*-
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package initialization
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; Turn off Welcome Screen
;; (setopt inhibit-splash-screen t)

(setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setopt display-time-default-load-average nil) ; this information is useless for most

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Save history of minibuffer
(savehist-mode)

;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control)

;; Fix archaic defaults
(setopt sentence-end-double-space nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Bell only visual
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun knoglerdev--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath)) ; remove Windows drive letter
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~"))))
    ;; NOTE: Make sure directory exists
    (make-directory (file-name-directory backupFilePath) t)
    backupFilePath))
(setopt make-backup-file-name-function 'knoglerdev--backup-file-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Discovery aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook 'help-quick)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package rg
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setopt enable-recursive-minibuffers t)
(setopt completion-cycle-threshold 1)
(setopt completions-detailed t)
(setopt tab-always-indent 'complete)
(setopt completion-styles '(basic initials substring))
(setopt completion-auto-help 'always)
(setopt completions-max-height 20)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setopt line-number-mode t)
(setopt column-number-mode t)

(setopt x-underline-at-descent-line nil)
(setopt switch-to-buffer-obey-display-actions t)

(setopt show-trailing-whitespace nil)
(setopt indicate-buffer-boundaries 'left)

(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

(setopt tab-width 2)

(blink-cursor-mode -1)
(pixel-scroll-precision-mode 1)

(cua-mode 1)
(xterm-mouse-mode 1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)

(add-hook 'text-mode-hook 'visual-line-mode)

(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keep tab-bar enabled (can be used as workspaces), but don't show it by default
(tab-bar-mode 1)
(setopt tab-bar-show nil)

(setq tab-bar-new-tab-choice "*dashboard*")
(setq tab-bar-close-button-show nil)
(setq tab-bar-new-button-show nil)

(global-set-key (kbd "C-c w 1") (lambda () (interactive) (tab-bar-select-tab 1)))
(global-set-key (kbd "C-c w 2") (lambda () (interactive) (tab-bar-select-tab 2)))
(global-set-key (kbd "C-c w 3") (lambda () (interactive) (tab-bar-select-tab 3)))
(global-set-key (kbd "C-c w n") #'tab-bar-new-tab)
(global-set-key (kbd "C-c w c") #'tab-bar-close-tab)

(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setopt display-time-format "%a %F %T")
(setopt display-time-interval 1)
(display-time-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   VSCode Tabs (centaur-tabs) + project.el + consult/embark friendly
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package centaur-tabs
  :ensure t
  :demand t
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-height 28)
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-icons t)
  ;; Du nutzt in base.el nerd-icons, nicht all-the-icons.
  ;; Daher hier auf 'nerd-icons stellen (oder nil, wenn du keine Icons willst).
  (centaur-tabs-icon-type 'nerd-icons)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "●")
  (centaur-tabs-close-button "×")
  (centaur-tabs-show-new-tab-button nil)
  (centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-enable-key-bindings nil)
  :config
  (centaur-tabs-mode 1)

  ;; Native-comp warning workaround (harmlos, aber so ist es clean)
  (declare-function centaur-tabs-2str "centaur-tabs")

  ;; --- Grouping via project.el ---------------------------------------------
  (require 'project)

  (defun knoglerdev/project-name ()
    "Return current project name via project.el or nil."
    (when-let ((pr (project-current nil)))
      (file-name-nondirectory (directory-file-name (project-root pr)))))

  (defun knoglerdev/centaur-tabs-buffer-groups ()
    "Group buffers by project.el project."
    (list
     (cond
      ((string-prefix-p "*" (buffer-name)) "Emacs")
      ((knoglerdev/project-name))
      (t "No Project"))))

  (setq centaur-tabs-buffer-groups-function #'knoglerdev/centaur-tabs-buffer-groups)

  ;; --- Hide some buffers from tabs -----------------------------------------
  (defun knoglerdev/centaur-tabs-hide-tab (bufname)
    "Non-nil if BUFNAME should be hidden from centaur-tabs."
    (let ((name bufname))
      (or
       (string-prefix-p " " name)
       (with-current-buffer (get-buffer name)
         (derived-mode-p
          'completion-list-mode
          'help-mode
          ;; wenn du Dired als Tab willst: nimm 'dired-mode hier raus
          'dired-mode)))))

  (setq centaur-tabs-hide-tabs-hooks '(knoglerdev/centaur-tabs-hide-tab))

  ;; Terminal: aus
  (when (not (display-graphic-p))
    (centaur-tabs-mode -1))

  ;; “Neuer Tab” = neuer Buffer
  (defun knoglerdev/new-empty-tab ()
    "Create a new empty buffer and switch to it (shows as a centaur-tab)."
    (interactive)
    (switch-to-buffer (generate-new-buffer "untitled"))
    (fundamental-mode))

  :bind
  (("C-<prior>" . centaur-tabs-backward)
   ("C-<next>"  . centaur-tabs-forward)
   ("C-c t g"   . centaur-tabs-switch-group)
   ("C-c t l"   . centaur-tabs-list-tabs)
   ("C-c t n"   . knoglerdev/new-empty-tab)
   ("C-c t w"   . kill-current-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'custom-theme-load-path (expand-file-name "themes/" user-emacs-directory))
(load-theme 'nord t)

(defun knoglerdev/fix-centaur-tabs-colors ()
  (set-face-attribute 'header-line nil
                      :background "#2E3440" :box nil :inherit nil)
  (centaur-tabs-headline-match))

(add-hook 'after-init-hook #'knoglerdev/fix-centaur-tabs-colors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Optional extras
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file (expand-file-name "extras/base.el" user-emacs-directory))
(load-file (expand-file-name "extras/org.el" user-emacs-directory))
(load-file (expand-file-name "extras/dashboard.el" user-emacs-directory))
(load-file (expand-file-name "extras/dev.el" user-emacs-directory))
(load-file (expand-file-name "extras/workspace.el" user-emacs-directory))

(custom-set-variables
 '(package-selected-packages nil))
(custom-set-faces)
