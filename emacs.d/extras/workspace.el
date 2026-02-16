;;; workspace.el --- Workspace setup -*- lexical-binding: t; -*-
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Custom workspace layout
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set default development directory
(defvar knoglerdev-default-dev-directory
  (expand-file-name "~/projects/")
  "Default directory for development projects.")

;; Ensure the directory exists
(unless (file-exists-p knoglerdev-default-dev-directory)
  (make-directory knoglerdev-default-dev-directory t))

;; Function to setup the workspace layout
(defun knoglerdev-setup-workspace ()
  "Setup workspace: treemacs + emms left, dashboard top, help bar bottom (6 lines)."
  (interactive)
  
  ;; Start clean
  (delete-other-windows)
  
  ;; === LEFT SIDE: Treemacs + EMMS ===
  (when (fboundp 'treemacs)
    (condition-case nil
        (progn
          (treemacs)
          ;; Try to add dev directory to workspace
          (ignore-errors
            (treemacs-do-add-project-to-workspace 
             knoglerdev-default-dev-directory
             (file-name-nondirectory (directory-file-name knoglerdev-default-dev-directory)))))
      (error nil))
    
    ;; If EMMS is loaded, show it below treemacs
    (when (and (featurep 'emms) (get-buffer "*EMMS Playlist*"))
      (let ((treemacs-window (selected-window)))
        ;; Split treemacs window vertically (bottom for EMMS)
        (let ((emms-window (split-window-below -10)))  ; 10 lines for EMMS
          (select-window emms-window)
          (switch-to-buffer "*EMMS Playlist*")
          ;; Make sure EMMS window is exactly 10 lines
          (window-resize emms-window (- 10 (window-height emms-window)))
          ;; Return to treemacs window, then move to main area
          (select-window treemacs-window))))
    
    (other-window 1))
  
  ;; === RIGHT SIDE: Dashboard + Help ===
  (let ((main-window (selected-window)))
    
    ;; Show dashboard in main window
    (when (get-buffer "*dashboard*")
      (switch-to-buffer "*dashboard*"))
    
    ;; Create help window at bottom with fixed height
    (let ((help-window (split-window-below -8)))  ; negative means from bottom
      (select-window help-window)
      
      ;; Show help buffer
      (if (get-buffer "*Quick Help*")
          (switch-to-buffer "*Quick Help*")
        (help-quick))
      
      ;; Set window height to exactly 6 lines
      (window-resize help-window (- 6 (window-height help-window))))
    
    ;; Return focus to dashboard
    (select-window main-window)))

;; Setup workspace after Emacs initialization
(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-timer 1.0 nil
                            (lambda ()
                              (condition-case err
                                  (knoglerdev-setup-workspace)
                                (error (message "Error setting up workspace: %S" err)))))))

;; Keybinding to restore the workspace layout
(global-set-key (kbd "C-c w r") 'knoglerdev-setup-workspace)