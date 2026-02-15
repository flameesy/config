;;; init-lisp.el --- Common Lisp setup for Centaur Emacs -*- lexical-binding: t; -*-

;; ----------------------------------------
;; Package management (Centaur uses straight.el)
;; ----------------------------------------

;; Ensure use-package is available
(straight-use-package 'use-package)

(setq use-package-always-ensure t)

;; ----------------------------------------
;; Common Lisp major mode
;; ----------------------------------------

(use-package lisp-mode
  :ensure nil
  :hook ((lisp-mode . show-paren-mode)
         (lisp-mode . rainbow-delimiters-mode)))

;; ----------------------------------------
;; SLY – Common Lisp IDE
;; ----------------------------------------

(use-package sly
  :commands (sly sly-connect)
  :init
  ;; Default Lisp implementation
  ;; Change this if you use something else (e.g. ccl, ecl)
  (setq inferior-lisp-program "sbcl")

  ;; Faster startup, better UX
  (setq sly-lisp-implementations
        '((sbcl ("sbcl") :coding-system utf-8-unix)))

  :config
  ;; Enable useful contribs
  (setq sly-contribs
        '(sly-fancy
          sly-indentation
          sly-xref-browser
          sly-mrepl
          sly-repl-ansi-color))

  ;; Auto-start SLY when opening Lisp files
  (add-hook 'lisp-mode-hook
            (lambda ()
              (unless (sly-connected-p)
                (save-excursion (sly))))))

;; ----------------------------------------
;; REPL quality-of-life
;; ----------------------------------------

(use-package sly-repl-ansi-color
  :after sly
  :hook (sly-mrepl-mode . sly-repl-ansi-color-mode))

;; ----------------------------------------
;; Structural editing
;; ----------------------------------------

(use-package paredit
  :hook ((lisp-mode . paredit-mode)
         (sly-mrepl-mode . paredit-mode)))

(use-package rainbow-delimiters
  :hook ((lisp-mode . rainbow-delimiters-mode)
         (sly-mrepl-mode . rainbow-delimiters-mode)))

;; ----------------------------------------
;; Completion & minibuffer
;; ----------------------------------------

(use-package company
  :hook ((lisp-mode . company-mode)
         (sly-mrepl-mode . company-mode)))

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode 1))

;; ----------------------------------------
;; Documentation & navigation
;; ----------------------------------------

(use-package eldoc
  :hook ((lisp-mode . eldoc-mode)
         (sly-mrepl-mode . eldoc-mode)))

;; ----------------------------------------
;; Formatting helpers
;; ----------------------------------------

(defun my/lisp-format-buffer ()
  "Indent whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(define-key lisp-mode-map (kbd "C-c C-f") #'my/lisp-format-buffer)

;; ----------------------------------------
;; Convenience keybindings
;; ----------------------------------------

(with-eval-after-load 'sly
  (define-key sly-mode-map (kbd "C-c C-z") #'sly-mrepl)
  (define-key sly-mode-map (kbd "C-c C-k") #'sly-compile-file)
  (define-key sly-mode-map (kbd "C-c C-c") #'sly-compile-defun))

(provide 'init-lisp)
;;; init-lisp.el ends here
