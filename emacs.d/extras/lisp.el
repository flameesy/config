;;; lisp.el --- Common Lisp development stuff  -*- lexical-binding: t; -*-
;;
;;
;; Enable SLY REPL in workspace
(setq knoglerdev-auto-start-sly t)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLY - Superior Lisp Interaction Mode for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  ;; HyperSpec root — use local copy if available, fall back to online
  (setq common-lisp-hyperspec-root
        (if (file-directory-p (expand-file-name "~/HyperSpec/"))
            (concat "file://" (expand-file-name "~/HyperSpec/"))
          "https://www.lispworks.com/documentation/HyperSpec/"))

  ;; Enable SLY for .lisp and .cl files
  (add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.asd\\'" . lisp-mode))

  ;; Auto-start SLY when opening a Lisp file (respects knoglerdev-auto-start-sly)
  (when knoglerdev-auto-start-sly
    (add-hook 'lisp-mode-hook
              (lambda ()
                (unless (sly-connected-p)
                  (sly)))))

  :bind (:map lisp-mode-map
              ("C-c C-z" . sly-switch-to-output-buffer)
              ("C-c M-z" . sly-mrepl-sync)
              ("C-c C-d C-d" . sly-describe-symbol)
              ("C-c C-d h" . sly-hyperspec-lookup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLY Extensions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SLY-ASDF: ASDF system integration
(use-package sly-asdf
  :ensure t
  :after sly
  :bind (:map sly-mode-map
              ("C-c C-a" . sly-asdf-load-system)))

;; SLY-Quicklisp: Quicklisp integration
(use-package sly-quicklisp
  :ensure t
  :after sly
  :config
  (add-to-list 'sly-contribs 'sly-quicklisp 'append))

;; SLY-REPL-ANSI-COLOR: Colorize REPL output
(use-package sly-repl-ansi-color
  :ensure t
  :after sly
  :config
  (push 'sly-repl-ansi-color sly-contribs))

;; SLY-MACROSTEP: Inline macro expansion with C-c RET
;; Essential for reading/writing macro-heavy code (CLOG, ASDF, etc.)
(use-package sly-macrostep
  :ensure t
  :after sly)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Paredit - Structured editing for Lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :hook ((lisp-mode       . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (scheme-mode     . paredit-mode)
         (sly-mrepl-mode  . paredit-mode))
  :config
  ;; Make paredit work better with delete-selection-mode
  (put 'paredit-forward-delete  'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)

  :bind (:map paredit-mode-map
              ("M-[" . paredit-wrap-square)
              ("M-{" . paredit-wrap-curly)))

;; Rebind RET in REPL — paredit overwrites it, breaking REPL input submission
(add-hook 'sly-mrepl-mode-hook
          (lambda ()
            (paredit-mode 1)
            (define-key paredit-mode-map (kbd "RET") nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight-Parentheses - Additional visual aid
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :hook ((lisp-mode       . highlight-parentheses-mode)
         (emacs-lisp-mode . highlight-parentheses-mode))
  :custom
  (highlight-parentheses-colors '("red" "green" "yellow" "cyan" "magenta")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Aggressive Indent - Auto-indentation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :hook ((lisp-mode       . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode))
  :custom
  (aggressive-indent-comments-too nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Company integration for SLY (company-mode)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hook directly into sly-mode and sly-mrepl-mode to avoid load-order races
(add-hook 'sly-mode-hook
          (lambda ()
            (when (fboundp 'company-mode)
              (company-mode 1))))

(add-hook 'sly-mrepl-mode-hook
          (lambda ()
            (when (fboundp 'company-mode)
              (company-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Eldoc - Show function signatures in minibuffer
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'lisp-mode-hook      #'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Additional helpful settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show matching parenthesis
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Use visual line mode in the REPL for long output
(add-hook 'sly-mrepl-mode-hook #'visual-line-mode)

;; Better default for Common Lisp indentation
(setq lisp-indent-function      'common-lisp-indent-function
      lisp-loop-forms-indentation 2
      lisp-simple-loop-indentation 2)

;; Make the REPL more comfortable
(with-eval-after-load 'sly-mrepl
  (setq sly-mrepl-history-file-name
        (expand-file-name "sly-mrepl-history" user-emacs-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Custom functions for Common Lisp workflow
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun knoglerdev/sly-load-project ()
  "Interactively load an ASDF system. Prompts for system name with completion."
  (interactive)
  (when (sly-connected-p)
    (call-interactively #'sly-asdf-load-system)))

;; Note: knoglerdev/sly-eval-buffer removed — use C-c C-k (sly-compile-and-load-file)
;; instead: it compiles the buffer and surfaces compiler notes inline, which is
;; strictly superior to silently evaluating forms.

;; Bind load-project to C-c C-p (C-c C-l is taken by sly-load-file)
(with-eval-after-load 'sly
  (define-key sly-mode-map (kbd "C-c C-p") 'knoglerdev/sly-load-project))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Package system helpers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun knoglerdev/insert-package-definition ()
  "Insert a basic Common Lisp package definition."
  (interactive)
  (insert "(defpackage #:my-package\n"
          "  (:use #:cl)\n"
          "  (:export))\n\n"
          "(in-package #:my-package)\n\n"))

(global-set-key (kbd "C-c l p") 'knoglerdev/insert-package-definition)

;; Lisp development menu
(with-eval-after-load 'menu-bar
  (defvar knoglerdev-lisp-menu (make-sparse-keymap "Lisp"))

  (define-key knoglerdev-lisp-menu [insert-package]
    '(menu-item "Insert Package Definition" knoglerdev/insert-package-definition
                :help "Insert Common Lisp package definition (C-c l p)"))

  (define-key-after global-map [menu-bar lisp-dev]
    (cons "Lisp" knoglerdev-lisp-menu)
    'dashboard-menu))

(provide 'lisp)
