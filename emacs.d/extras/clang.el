;;; clang.el --- C and C++ development setup  -*- lexical-binding: t; -*-
;;;
;;; Voraussetzungen:
;;;   LSP (clangd)
;;;   clangd braucht eine compile_commands.json im Projektroot.
;;;   Erzeugen mit CMake:   cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..
;;;   Oder mit bear:        bear -- make
;;;
;;;   Tree-sitter Grammatiken einmalig installieren:
;;;     M-x clang-setup-install-grammars
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tree-sitter Grammatiken installieren
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Einmalig aufrufen: M-x clang-setup-install-grammars
(defun clang-setup-install-grammars ()
  "Installiert Tree-sitter Grammatiken für C und C++, falls nicht vorhanden."
  (interactive)
  (dolist (grammar
           '((c   . ("https://github.com/tree-sitter/tree-sitter-c"))
             (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))))
    (add-to-list 'treesit-language-source-alist grammar)
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar))))
  (message "C/C++ Tree-sitter Grammatiken installiert – bitte Emacs neu starten."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   C/C++ Grundeinstellungen
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun knoglerdev--c-setup ()
  "Gemeinsame Einstellungen für C und C++."
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)
  (setq-local treesit-simple-indent-offset 4)
  (electric-pair-local-mode 1)
  (display-fill-column-indicator-mode 1)
  (setq-local fill-column 100))

(add-hook 'c-ts-mode-hook   #'knoglerdev--c-setup)
(add-hook 'c++-ts-mode-hook #'knoglerdev--c-setup)

;; Dateiendungen und Remaps – nur wenn Grammatiken tatsächlich installiert sind
(when (and (treesit-available-p)
           (treesit-language-available-p 'c)
           (treesit-language-available-p 'cpp))
  (add-to-list 'major-mode-remap-alist '(c-mode   . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.c\\'"   . c-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'"   . c-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cc\\'"  . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.hxx\\'" . c++-ts-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   lsp-mode + clangd
;;;
;;; WICHTIG: lsp-mode kennt c-ts-mode und c++-ts-mode standardmäßig NICHT.
;;; Wir müssen clangd explizit für diese Modes registrieren, sonst startet
;;; der LSP-Server nicht und es gibt kein Highlighting/Completion.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  ((c-ts-mode   . lsp-deferred)
   (c++-ts-mode . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-idle-delay 0.1)
  (lsp-log-io nil)
  (lsp-clients-clangd-args '("--background-index"
                              "--clang-tidy"
                              "--completion-style=detailed"
                              "--header-insertion=never"
                              "-j=4"))
  :config
  (lsp-enable-which-key-integration t)

  ;; clangd für c-ts-mode und c++-ts-mode registrieren.
  ;; lsp-mode erkennt diese Modes standardmäßig nicht, daher müssen wir
  ;; die Sprachzuordnung manuell eintragen.
  (add-to-list 'lsp-language-id-configuration '(c-ts-mode   . "c"))
  (add-to-list 'lsp-language-id-configuration '(c++-ts-mode . "cpp")))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-headerline-breadcrumb-enable t))

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

(with-eval-after-load 'c-ts-mode
  (define-key c-ts-mode-map (kbd "C-c C-c") #'compile)
  (define-key c-ts-mode-map (kbd "C-c C-r") #'recompile))

(with-eval-after-load 'c++-ts-mode
  (define-key c++-ts-mode-map (kbd "C-c C-c") #'compile)
  (define-key c++-ts-mode-map (kbd "C-c C-r") #'recompile))

(provide 'clang)
;;; clang.el ends here
