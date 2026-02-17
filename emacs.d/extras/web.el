;;;; web.el --- Modern React/TypeScript development setup -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Setup Instructions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STEP 1: Tree-sitter Grammatiken installieren
;;   Run: M-x web-setup-install-grammars
;;   Das lädt und kompiliert die Parser für TypeScript, TSX, JSON, etc.
;;
;; STEP 2: LSP Server installieren
;;   Run: M-x lsp-install-server
;;   Wähle: ts-ls (TypeScript)
;;   Nochmal für: eslint
;;   Nochmal für: tailwindcss (optional)
;;
;; STEP 3: (Optional aber empfohlen) emacs-lsp-booster installieren
;;   Verbessert LSP-Performance durch asynchrones JSON-Parsing.
;;   https://github.com/blahgeek/emacs-lsp-booster
;;
;;   Linux/macOS:  cargo install --git https://github.com/blahgeek/emacs-lsp-booster
;;   Windows:      Binary von der Releases-Seite herunterladen
;;   Danach sicherstellen, dass 'emacs-lsp-booster' im PATH ist.
;;
;; STEP 4: Prettier installieren (für Code-Formatierung)
;;   npm install -g prettier
;;   Oder projekt-lokal (empfohlen)
;;
;; STEP 5: LSP-Gesundheit prüfen
;;   Run: M-x lsp-doctor
;;
;; USAGE:
;;   - .ts, .tsx, .js, .jsx öffnen → LSP startet automatisch
;;   - C-c l         = LSP Prefix
;;   - C-c C-d       = Dokumentation für Symbol under point
;;   - C-c o         = Combobulate Prefix (strukturelles Editieren)
;;   - M-n / M-p     = Nächster/vorheriger Fehler
;;   - Speichern formatiert automatisch mit Prettier

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tree-sitter: Grammatiken + Dateiendungen
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hilfsfunktion zum Installieren aller nötigen Grammatiken
(defun web-setup-install-grammars ()
  "Installiert Tree-sitter Grammatiken für Web-Entwicklung, falls nicht vorhanden."
  (interactive)
  (dolist (grammar
           '((css        . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
             (bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
             (html       . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
             (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
             (json       . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
             (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
             (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
             (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
    (add-to-list 'treesit-language-source-alist grammar)
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))

;; Dateiendungen den ts-Modes zuordnen (nur wenn treesit verfügbar)
(when (treesit-available-p)
  ;; Alte Modes auf Tree-sitter-Varianten umleiten
  (dolist (mapping '((typescript-mode . typescript-ts-mode)
                     (js-mode         . typescript-ts-mode)
                     (js2-mode        . typescript-ts-mode)
                     (json-mode       . json-ts-mode)
                     (css-mode        . css-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  ;; Dateiendungen direkt den ts-Modes zuordnen
  (dolist (entry '(("\\.ts\\'"  . typescript-ts-mode)
                   ("\\.tsx\\'" . tsx-ts-mode)
                   ("\\.js\\'"  . typescript-ts-mode)
                   ("\\.jsx\\'" . tsx-ts-mode)
                   ("\\.mjs\\'" . typescript-ts-mode)
                   ("\\.mts\\'" . typescript-ts-mode)
                   ("\\.cjs\\'" . typescript-ts-mode)
                   ("\\.json\\'" . json-ts-mode)))
    (add-to-list 'auto-mode-alist entry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Completion mit Corfu
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  :bind (:map corfu-map
              ("TAB"     . corfu-next)
              ([tab]     . corfu-next)
              ("S-TAB"   . corfu-previous)
              ([backtab] . corfu-previous)
              ("RET"     . corfu-insert))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Linting mit Flycheck
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   LSP Mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Performance-Einstellungen
(setenv "LSP_USE_PLISTS" "true")
(setq read-process-output-max (* 10 1024 1024)) ; 10MB
(setq gc-cons-threshold 200000000)

(use-package lsp-mode
  :ensure t
  :diminish "LSP"
  :hook
  ((lsp-mode          . lsp-diagnostics-mode)
   (lsp-mode          . lsp-enable-which-key-integration)
   (tsx-ts-mode       . lsp-deferred)
   (typescript-ts-mode . lsp-deferred)
   (js-ts-mode        . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :none)           ; Corfu übernimmt Completion
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)
  (lsp-keep-workspace-alive nil)
  (lsp-idle-delay 0.5)
  (lsp-enable-xref t)
  (lsp-auto-configure t)
  (lsp-eldoc-enable-hover t)
  (lsp-enable-dap-auto-configure t)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)              ; Prettier übernimmt Formatierung
  (lsp-enable-links nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-suggest-server-download t)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-text-document-color nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-diagnostic-max-lines 20)
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t)
  (lsp-enable-snippet t)
  (lsp-completion-show-kind t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-signature-doc-lines 1)
  (lsp-ui-doc-use-childframe t)
  (lsp-eldoc-render-all nil)
  (lsp-lens-enable nil)
  (lsp-semantic-tokens-enable nil)          ; Tree-sitter übernimmt Highlighting
  :preface
  ;; lsp-booster: schnelleres JSON-Parsing wenn Binary installiert ist
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Versucht Bytecode statt JSON zu parsen."
    (or (when (equal (following-char) ?#)
          (let ((bytecode (read (current-buffer))))
            (when (byte-code-function-p bytecode)
              (funcall bytecode))))
        (apply old-fn args)))

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Hängt emacs-lsp-booster vor den LSP-Befehl."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)
               (not (file-remote-p default-directory))
               lsp-use-plists
               (not (functionp 'json-rpc-connection))
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  :init
  (setq lsp-use-plists t)
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(use-package lsp-completion
  :no-require
  :hook ((lsp-mode . lsp-completion-mode)))

(use-package lsp-ui
  :ensure t
  :commands (lsp-ui-doc-show lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-ui-doc-glance))
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'at-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   ESLint LSP
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-eslint
  :demand t
  :after lsp-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tailwind LSP (optional)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-tailwindcss
  :ensure t
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (tw-major-mode '(css-mode css-ts-mode
                           typescript-mode typescript-ts-mode
                           tsx-ts-mode js2-mode js-ts-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Prettier Formatierung mit Apheleia
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package apheleia
  :ensure t
  :diminish ""
  :config
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (apheleia-global-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Strukturelles Editieren mit Combobulate (optional)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package combobulate
  :ensure t
  :preface (setq combobulate-key-prefix "C-c o")
  :hook ((python-ts-mode    . combobulate-mode)
         (js-ts-mode        . combobulate-mode)
         (html-ts-mode      . combobulate-mode)
         (css-ts-mode       . combobulate-mode)
         (yaml-ts-mode      . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (json-ts-mode      . combobulate-mode)
         (tsx-ts-mode       . combobulate-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Menu bar
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'menu-bar
  (defvar knoglerdev-web-menu (make-sparse-keymap "Web Dev"))

  (define-key knoglerdev-web-menu [lsp-doctor]
    '(menu-item "LSP Doctor" lsp-doctor
                :help "LSP-Konfiguration prüfen"))

  (define-key knoglerdev-web-menu [lsp-install-server]
    '(menu-item "Install LSP Server" lsp-install-server
                :help "TypeScript/ESLint/Tailwind Server installieren"))

  (define-key knoglerdev-web-menu [install-grammars]
    '(menu-item "Install Tree-sitter Grammars" web-setup-install-grammars
                :help "Alle Web-Grammatiken installieren"))

  (define-key-after global-map [menu-bar web-dev]
    (cons "Web Dev" knoglerdev-web-menu)
    'lisp-dev))

(provide 'web)
;;; web.el ends here
