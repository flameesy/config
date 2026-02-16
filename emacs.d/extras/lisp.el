;;; lisp.el --- Common Lisp development stuff  -*- lexical-binding: t; -*-
;;
;; USAGE:
;; - Open a .lisp file and run: M-x sly
;; - C-c C-c: Compile defun
;; - C-c C-k: Compile and load file
;; - C-c C-z: Switch to REPL
;; - M-.: Jump to definition
;; - M-,: Pop back from definition
;; - C-c C-d h: Hyperspec lookup

;;; Code:

;;;
;;; SLY - Superior Lisp Interaction Mode for Emacs
;;;

(use-package sly
  :ensure t
  :defer t
  :custom
  (inferior-lisp-program "sbcl")
  
  ;; Alternative: if you have multiple implementations
  ;; (sly-lisp-implementations
  ;;  '((sbcl ("sbcl" "--dynamic-space-size" "2048"))
  ;;    (ccl ("ccl"))
  ;;    (ecl ("ecl"))))
  
  ;; Completion style
  (sly-complete-symbol-function 'sly-flex-completions)
  
  :config
  ;; Better indentation for Common Lisp
  (setq lisp-indent-function 'common-lisp-indent-function)
  
  ;; Enable SLY for .lisp and .cl files
  (add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.asd\\'" . lisp-mode))
  
  ;; Keybindings for convenience
  :bind (:map lisp-mode-map
              ("C-c C-z" . sly-switch-to-output-buffer)
              ("C-c M-z" . sly-mrepl-sync)
              ("C-c C-d C-d" . sly-describe-symbol)
              ("C-c C-d h" . sly-hyperspec-lookup)))

;;;
;;; SLY Extensions
;;;

;; SLY-ASDF: ASDF system integration
(use-package sly-asdf
  :ensure t
  :after sly
  :config
  ;; Keybinding to load ASDF systems
  (define-key sly-mode-map (kbd "C-c C-a") 'sly-asdf-load-system))

;; SLY-Quicklisp: Quicklisp integration
(use-package sly-quicklisp
  :ensure t
  :after sly
  :config
  ;; Automatically available in SLY REPL
  (add-to-list 'sly-contribs 'sly-quicklisp 'append))

;; SLY-REPL-ANSI-COLOR: Colorize REPL output
(use-package sly-repl-ansi-color
  :ensure t
  :after sly
  :config
  (push 'sly-repl-ansi-color sly-contribs))

;;;
;;; Paredit - Structured editing for Lisp
;;;

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :hook ((lisp-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (scheme-mode . paredit-mode)
         (sly-mrepl-mode . paredit-mode))
  :config
  ;; Make paredit work better with delete-selection-mode
  (put 'paredit-forward-delete 'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  
  :bind (:map paredit-mode-map
              ;; Some additional helpful bindings
              ("M-[" . paredit-wrap-square)
              ("M-{" . paredit-wrap-curly)))

;;;
;;; Highlight-Parentheses - Additional visual aid
;;;

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :hook ((lisp-mode . highlight-parentheses-mode)
         (emacs-lisp-mode . highlight-parentheses-mode))
  :custom
  (highlight-parentheses-colors '("red" "green" "yellow" "cyan" "magenta")))

;;;
;;; Aggressive Indent - Auto-indentation
;;;

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :hook ((lisp-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode))
  :custom
  ;; Don't indent when typing in comments
  (aggressive-indent-comments-too nil))

;;;
;;; Company integration for SLY (company-mode)
;;;

;; If you have company-mode configured elsewhere, this adds SLY completion
(with-eval-after-load 'company
  (add-hook 'sly-mode-hook 'company-mode))
  
;;;
;;; Eldoc - Show function signatures in minibuffer
;;;

;; Already built-in to Emacs, just ensure it's enabled for Lisp modes
(add-hook 'lisp-mode-hook #'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

;;;
;;; Project integration
;;;

;; SLY automatically integrates with project.el
;; When you M-x sly in a project directory, it creates a project-specific REPL

;;;
;;; Additional helpful settings
;;;

;; Show matching parenthesis
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Use visual line mode in the REPL for long output
(add-hook 'sly-mrepl-mode-hook #'visual-line-mode)

;; Better default for Common Lisp indentation
(setq lisp-indent-function 'common-lisp-indent-function
      lisp-loop-forms-indentation 2
      lisp-simple-loop-indentation 2)

;; Make the REPL more comfortable
(with-eval-after-load 'sly-mrepl
  ;; History settings
  (setq sly-mrepl-history-file-name
        (expand-file-name "sly-mrepl-history" user-emacs-directory))
  
  ;; Enable company in REPL if available
  (when (fboundp 'company-mode)
    (add-hook 'sly-mrepl-mode-hook #'company-mode)))

;;;
;;; Custom functions for Common Lisp workflow
;;;

(defun my/sly-eval-buffer ()
  "Evaluate the entire buffer in SLY."
  (interactive)
  (sly-eval-region (point-min) (point-max)))

(defun my/sly-load-project ()
  "Load the current ASDF project."
  (interactive)
  (when (sly-connected-p)
    (sly-asdf-load-system (sly-current-package))))

;; Keybindings for custom functions
(with-eval-after-load 'sly
  (define-key sly-mode-map (kbd "C-c C-b") 'my/sly-eval-buffer)
  (define-key sly-mode-map (kbd "C-c C-l") 'my/sly-load-project))

;;;
;;; Package system helpers
;;;

;; Quick function to define a package at the top of a file
(defun my/insert-package-definition ()
  "Insert a basic Common Lisp package definition."
  (interactive)
  (insert "(defpackage #:my-package\n"
          "  (:use #:cl)\n"
          "  (:export))\n\n"
          "(in-package #:my-package)\n\n"))

(global-set-key (kbd "C-c l p") 'my/insert-package-definition)

;;;
;;; Documentation
;;;

;; Quick reference for keybindings
;; Add to your help menu with:
;; (define-key help-map (kbd "l") 'my/lisp-help)

(defun my/lisp-help ()
  "Show Common Lisp development keybindings."
  (interactive)
  (with-help-window "*Common Lisp Help*"
    (princ "Common Lisp Development with SLY - Quick Reference\n")
    (princ "=====================================================\n\n")
    (princ "Starting/Connecting:\n")
    (princ "  M-x sly              Start SLY and connect to Lisp\n")
    (princ "  M-x sly-connect      Connect to running Lisp\n\n")
    (princ "Evaluation:\n")
    (princ "  C-c C-c              Compile defun at point\n")
    (princ "  C-c C-k              Compile and load file\n")
    (princ "  C-c C-b              Evaluate entire buffer\n")
    (princ "  C-c C-r              Evaluate region\n")
    (princ "  C-c C-e              Evaluate last expression\n\n")
    (princ "Navigation:\n")
    (princ "  M-.                  Jump to definition\n")
    (princ "  M-,                  Pop back from definition\n")
    (princ "  C-c C-z              Switch to REPL\n")
    (princ "  C-c M-z              Sync package in REPL\n\n")
    (princ "Documentation:\n")
    (princ "  C-c C-d C-d          Describe symbol\n")
    (princ "  C-c C-d h            HyperSpec lookup\n")
    (princ "  C-c h                HyperSpec lookup (alt)\n")
    (princ "  C-c C-d a            Apropos\n\n")
    (princ "ASDF/Quicklisp:\n")
    (princ "  C-c C-a              Load ASDF system\n")
    (princ "  C-c C-l              Load current project\n\n")
    (princ "Paredit (Structural Editing):\n")
    (princ "  C-M-f                Forward sexp\n")
    (princ "  C-M-b                Backward sexp\n")
    (princ "  C-M-u                Up sexp\n")
    (princ "  C-M-d                Down sexp\n")
    (princ "  M-(                  Wrap with parentheses\n")
    (princ "  M-[                  Wrap with square brackets\n")
    (princ "  M-s                  Splice (remove parens)\n")
    (princ "  M-r                  Raise (replace parent with current)\n\n")
    (princ "Debugging:\n")
    (princ "  C-c C-t              Toggle trace\n")
    (princ "  M-x sly-restart-inferior-lisp  Restart Lisp\n")))

(provide 'lisp)
;;; lisp.el ends here
