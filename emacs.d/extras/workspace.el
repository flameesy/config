;;; workspace.el --- Workspace setup -*- lexical-binding: t; -*-

(defvar knoglerdev-default-dev-directory
  (expand-file-name "~/projects/"))

(unless (file-exists-p knoglerdev-default-dev-directory)
  (make-directory knoglerdev-default-dev-directory t))

(defun knoglerdev-setup-workspace ()
  "Setup workspace layout."
  (interactive)
  
  (delete-other-windows)
  
  ;; LEFT: Treemacs
  (when (fboundp 'treemacs)
    (let ((treemacs-buffer (get-buffer "*Treemacs*")))
      (if treemacs-buffer
          (progn
            (display-buffer-in-side-window treemacs-buffer '((side . left)))
            (select-window (get-buffer-window treemacs-buffer)))
        (condition-case nil
            (treemacs)
          (error nil)))
      
      (unless (treemacs-is-path knoglerdev-default-dev-directory :in-workspace)
        (ignore-errors
          (treemacs-do-add-project-to-workspace 
           knoglerdev-default-dev-directory
           (file-name-nondirectory (directory-file-name knoglerdev-default-dev-directory))))))
    
    (other-window 1))
  
  ;; RIGHT: Dashboard + bottom shell
  (let ((main-window (selected-window)))
    
    (let ((dashboard-buffer (get-buffer "*dashboard*")))
      (when dashboard-buffer
        (switch-to-buffer dashboard-buffer)))
    
    ;; BOTTOM: Shell
    (let ((bottom-window (split-window-below -8)))
      (select-window bottom-window)
      
      (let ((eshell-buffer (seq-find (lambda (buf)
                                       (with-current-buffer buf
                                         (derived-mode-p 'eshell-mode)))
                                     (buffer-list))))
        (if eshell-buffer
            (switch-to-buffer eshell-buffer)
          (eshell)))
      
      (window-resize bottom-window (- 20 (window-height bottom-window))))
    
    (select-window main-window)))

(defun knoglerdev-move-repl-to-bottom ()
  "Move SLY REPL to bottom-right, creating split if needed."
  (interactive)
  (when-let ((repl-buffer (seq-find (lambda (buf)
                                      (string-match-p "^\\*sly-mrepl" (buffer-name buf)))
                                    (buffer-list))))
    ;; Find bottom window
    (let ((windows (window-list))
          (bottom-left nil)
          (max-top 0))
      (dolist (win windows)
        (let ((top (window-pixel-top win)))
          (when (> top max-top)
            (setq max-top top
                  bottom-left win))))
      
      (when bottom-left
        ;; Select bottom-left window and split right
        (select-window bottom-left)
        (unless (window-right bottom-left)
          (split-window-right))
        
        ;; Move to right window and show REPL
        (other-window 1)
        (switch-to-buffer repl-buffer)
        
        ;; Close REPL in other windows
        (dolist (win (get-buffer-window-list repl-buffer nil t))
          (unless (eq win (selected-window))
            (delete-window win)))))))

(defun knoglerdev-new-terminal ()
  "Open a new eshell in the current window."
  (interactive)
  (eshell t))

(add-hook 'emacs-startup-hook #'knoglerdev-setup-workspace)

(global-set-key (kbd "C-c w r") 'knoglerdev-setup-workspace)
(global-set-key (kbd "C-c w t") 'knoglerdev-new-terminal)
(global-set-key (kbd "C-c w s") 'knoglerdev-move-repl-to-bottom)

;; Workspace menu
(with-eval-after-load 'menu-bar
  (defvar knoglerdev-workspace-menu (make-sparse-keymap "Workspace"))
  
  (define-key knoglerdev-workspace-menu [restore]
    '(menu-item "Restore Layout" knoglerdev-setup-workspace
                :help "Restore workspace layout (C-c w r)"))
  
  (define-key knoglerdev-workspace-menu [new-terminal]
    '(menu-item "New Terminal" knoglerdev-new-terminal
                :help "Open new eshell (C-c w t)"))
  
  (define-key knoglerdev-workspace-menu [move-repl]
    '(menu-item "Move REPL to Bottom" knoglerdev-move-repl-to-bottom
                :help "Move SLY REPL to bottom-right (C-c w s)"))
  
  (define-key knoglerdev-workspace-menu [separator-1] '(menu-item "--"))
  
  (define-key knoglerdev-workspace-menu [tab-1]
    '(menu-item "Tab 1" (lambda () (interactive) (tab-bar-select-tab 1))
                :help "Switch to tab 1 (C-c w 1)"))
  
  (define-key knoglerdev-workspace-menu [tab-2]
    '(menu-item "Tab 2" (lambda () (interactive) (tab-bar-select-tab 2))
                :help "Switch to tab 2 (C-c w 2)"))
  
  (define-key knoglerdev-workspace-menu [tab-3]
    '(menu-item "Tab 3" (lambda () (interactive) (tab-bar-select-tab 3))
                :help "Switch to tab 3 (C-c w 3)"))
  
  (define-key knoglerdev-workspace-menu [new-tab]
    '(menu-item "New Tab" tab-bar-new-tab
                :help "Create new tab (C-c w n)"))
  
  (define-key knoglerdev-workspace-menu [close-tab]
    '(menu-item "Close Tab" tab-bar-close-tab
                :help "Close current tab (C-c w c)"))
  
  (define-key-after global-map [menu-bar workspace]
    (cons "Workspace" knoglerdev-workspace-menu)
    'tools))

(provide 'workspace)

;;; workspace.el ends here
