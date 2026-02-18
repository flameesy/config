;;; flutter.el --- Flutter/Dart setup (lsp-mode + lsp-dart + flutter.el + project.el)  -*- lexical-binding: t; -*-
;;;
;;; Prereqs (must be installed / done on the machine):
;;;
;;; 1) Flutter SDK installed and on PATH (Windows + Linux):
;;;    - `flutter --version` works in a normal terminal
;;;    - ideally also: `dart --version`
;;;
;;; 2) From time to time run (especially after Flutter updates):
;;;    - `flutter doctor`
;;;
;;; 3) Emacs packages (MELPA):
;;;    - dart-mode
;;;    - lsp-mode
;;;    - lsp-ui (optional but recommended)
;;;    - lsp-dart
;;;    - flutter

(require 'cl-lib)

;; ---------------------------------------------------------------------------
;; Dart editing
;; ---------------------------------------------------------------------------

(use-package dart-mode
  :ensure t
  :mode "\\.dart\\'")

;; ---------------------------------------------------------------------------
;; LSP (lsp-mode + lsp-dart)
;; ---------------------------------------------------------------------------

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-idle-delay 0.3)
  (lsp-log-io nil)
  (lsp-enable-snippet t)
  (lsp-headerline-breadcrumb-enable nil)
  :hook
  (dart-mode . lsp-deferred))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t))

(use-package lsp-dart
  :ensure t
  :after (lsp-mode dart-mode)
  :custom
  ;; If flutter/dart are in PATH, you can usually leave SDK dirs unset.
  ;; If Windows GUI Emacs can't find them, uncomment and set explicit paths:
  ;; (lsp-dart-flutter-sdk-dir "C:/src/flutter/")
  ;; (lsp-dart-sdk-dir "C:/src/flutter/bin/cache/dart-sdk/")
  (lsp-dart-show-todos t)
  (lsp-dart-outline t)
  (lsp-dart-flutter-widget-guides t)
  (lsp-dart-flutter-outline t))

;; Format on save via LSP (safe: ignore errors)
(defun anna/dart-lsp-format-on-save ()
  "Format Dart buffers on save using LSP."
  (when (and (eq major-mode 'dart-mode)
             (bound-and-true-p lsp-mode))
    (ignore-errors (lsp-format-buffer))))

(add-hook 'before-save-hook #'anna/dart-lsp-format-on-save)

;; ---------------------------------------------------------------------------
;; Flutter commands (run/hot-reload/tests/device)
;; ---------------------------------------------------------------------------

(use-package flutter
  :ensure t
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x"   . flutter-run-or-hot-reload)
              ("C-c f r" . flutter-run)
              ("C-c f R" . flutter-hot-reload)
              ("C-c f q" . flutter-quit)
              ("C-c f d" . flutter-select-device)
              ("C-c f t a" . flutter-test-all)
              ("C-c f t f" . flutter-test-current-file)
              ("C-c f t t" . flutter-test-at-point))
  :custom
  ;; Optional when flutter is on PATH:
  (flutter-sdk-path "C:/Users/s.knogler/dev/flutter/")
  ;; (flutter-sdk-path "~/dev/flutter/")
  )

;; ---------------------------------------------------------------------------
;; project.el integration: detect Flutter projects via pubspec.yaml
;; + set default project-compile command to `flutter run`
;; ---------------------------------------------------------------------------

(use-package project
  ;; built-in, no :ensure
  :config
  ;; Our project type is (flutter . "/path/to/root/")
  (cl-defmethod project-root ((project (head flutter)))
    (cdr project))

  (defun anna/project-try-flutter (dir)
    "Detect a Flutter project by searching for pubspec.yaml above DIR."
    (let ((root (locate-dominating-file dir "pubspec.yaml")))
      (when root
        (cons 'flutter (expand-file-name root)))))

  ;; Make it available (prepend-ish). add-hook adds to front by default when APPEND is nil.
  (add-hook 'project-find-functions #'anna/project-try-flutter)

  ;; Default compile command for Flutter projects.
  ;; This affects `C-x p c` (project-compile) inside Flutter projects.
  (cl-defmethod project-compilation-command ((project (head flutter)))
    "flutter run"))

;; Flutter menu (Menu Bar) — robust (only adds entries that exist)
(with-eval-after-load 'menu-bar
  (defvar knoglerdev-flutter-menu (make-sparse-keymap "Flutter")
    "Menu for Flutter/Dart helpers.")

  (defun knoglerdev--menu-add (key title fn &optional help)
    "Add menu item KEY with TITLE calling FN if FN is defined."
    (when (fboundp fn)
      (define-key knoglerdev-flutter-menu (vector key)
        `(menu-item ,title ,fn :help ,(or help title)))))

  (defun knoglerdev--menu-add-sep (key)
    (define-key knoglerdev-flutter-menu (vector key)
      '(menu-item "--")))

  ;; Helper: show flutter process/buffer (best-effort)
  (defun knoglerdev-flutter-show-buffer ()
    "Show the Flutter process buffer (best-effort)."
    (interactive)
    (cond
     ;; some versions expose `flutter-buffer`
     ((and (boundp 'flutter-buffer) (bufferp flutter-buffer))
      (pop-to-buffer flutter-buffer))
     ;; common buffer names
     ((get-buffer "*Flutter*")
      (pop-to-buffer "*Flutter*"))
     ((get-buffer "*flutter*")
      (pop-to-buffer "*flutter*"))
     (t
      (message "No Flutter buffer found. Start with `flutter-run` first."))))

  ;; Run / Reload / Restart / Quit / Device
  (knoglerdev--menu-add 'flutter-run "Run (flutter run)" #'flutter-run
                        "Run the app (C-c f r)")

  (knoglerdev--menu-add 'flutter-run-or-hot-reload "Run or Hot Reload" #'flutter-run-or-hot-reload
                        "Run or hot reload (C-M-x)")

  (knoglerdev--menu-add 'flutter-hot-reload "Hot Reload" #'flutter-hot-reload
                        "Hot reload (C-c f R)")

  ;; Hot Restart exists in some flutter.el versions. If your package doesn't have it,
  ;; this menu entry will simply not show up.
  (knoglerdev--menu-add 'flutter-hot-restart "Hot Restart" #'flutter-hot-restart
                        "Hot restart (if supported by flutter.el)")

  (define-key knoglerdev-flutter-menu [flutter-show-buffer]
    '(menu-item "Show Flutter Buffer" knoglerdev-flutter-show-buffer
                :help "Show Flutter process output buffer"))

  (knoglerdev--menu-add 'flutter-select-device "Select Device" #'flutter-select-device
                        "Select a device (C-c f d)")

  (knoglerdev--menu-add 'flutter-quit "Quit Flutter" #'flutter-quit
                        "Stop flutter run (C-c f q)")

  (knoglerdev--menu-add-sep 'sep-1)

  ;; Tests
  (knoglerdev--menu-add 'flutter-test-all "Test: All" #'flutter-test-all
                        "Run all tests (C-c f t a)")
  (knoglerdev--menu-add 'flutter-test-current-file "Test: Current File" #'flutter-test-current-file
                        "Run tests in current file (C-c f t f)")
  (knoglerdev--menu-add 'flutter-test-at-point "Test: At Point" #'flutter-test-at-point
                        "Run test at point (C-c f t t)")

  (knoglerdev--menu-add-sep 'sep-2)

  ;; Project default action: flutter run via project-compile
  (define-key knoglerdev-flutter-menu [project-compile]
    '(menu-item "Project: Run (project-compile ? flutter run)" project-compile
                :help "Run default project action (C-x p c)"))

  ;; Add menu to menu-bar (placed after Dashboard if it exists; otherwise after Calendar)
  (define-key-after global-map [menu-bar flutter-menu]
    (cons "Flutter" knoglerdev-flutter-menu)
    (if (boundp 'knoglerdev-dashboard-menu) 'dashboard-menu 'calendar)))


(provide 'flutter)
;;; flutter.el ends here
