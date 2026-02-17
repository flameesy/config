;;; clang.el --- C and C++ development setup  -*- lexical-binding: t; -*-
;;;
;;; Voraussetzungen:
;;;
;;;   LSP (clangd):
;;;     sudo apt install clangd        (Debian/Ubuntu)
;;;     sudo pacman -S clang           (Arch)
;;;     brew install llvm              (macOS)
;;;
;;;   clangd braucht eine compile_commands.json im Projektroot.
;;;   Erzeugen mit CMake:   cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..
;;;   Oder mit bear:        bear -- make
;;;
;;;   Tree-sitter (optional, empfohlen):
;;;     M-x treesit-install-language-grammar RET c RET
;;;     M-x treesit-install-language-grammar RET cpp RET

;;; Contents:
;;;
;;;  - lsp-mode + lsp-ui
;;;  - C/C++ Grundeinstellungen
;;;  - Tree-sitter Integration
;;;  - cmake-mode
;;;  - Debugging mit gdb-mi

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   lsp-mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  ((c-mode      . lsp-deferred)
   (c++-mode    . lsp-deferred)
   (c-ts-mode   . lsp-deferred)
   (c++-ts-mode . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Alle LSP-Befehle unter C-c l
  (lsp-idle-delay 0.1)                  ; Wie schnell LSP auf Änderungen reagiert
  (lsp-log-io nil)                      ; Kein IO-Logging (Performance)
  (lsp-clangd-binary-path (or (executable-find "clangd") "clangd"))
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  ;; Sideline: zeigt Fehler/Warnungen inline neben dem Code
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil)       ; hover lieber explizit via C-c l h
  (lsp-ui-sideline-show-code-actions t)
  ;; Doc-Popup beim Hovern über Symbolen
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-show-with-cursor nil)      ; nur bei explizitem Aufruf...
  (lsp-ui-doc-show-with-mouse t)         ; ...oder Maus drüber
  ;; Breadcrumb (Pfad im Header: Datei > Funktion > ...)
  (lsp-headerline-breadcrumb-enable t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   C/C++ Grundeinstellungen
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cc-mode
  :hook
  ((c-mode   . knoglerdev--c-setup)
   (c++-mode . knoglerdev--c-setup))
  :config
  (defun knoglerdev--c-setup ()
    "Gemeinsame Einstellungen für C und C++."
    (c-set-style "k&r")
    (setq-local c-basic-offset 4)
    (setq-local tab-width 4)
    (setq-local indent-tabs-mode nil)
    (electric-pair-local-mode 1)
    (display-fill-column-indicator-mode 1)
    (setq-local fill-column 100)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tree-sitter Integration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (treesit-available-p)
  (add-to-list 'major-mode-remap-alist '(c-mode   . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))

  (add-to-list 'auto-mode-alist '("\\.c\\'"   . c-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'"   . c-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cc\\'"  . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.hxx\\'" . c++-ts-mode))

  (add-hook 'c-ts-mode-hook   #'knoglerdev--c-setup)
  (add-hook 'c++-ts-mode-hook #'knoglerdev--c-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   cmake-mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Debugging mit GDB
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gdb-mi
  :custom
  (gdb-many-windows t)
  (gdb-show-main t))

(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "C-c C-c") #'compile)
  (define-key c-mode-base-map (kbd "C-c C-r") #'recompile))

(provide 'clang)
;;; clang.el ends here
