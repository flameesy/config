;;; custom.el --- Persönliche Centaur EMACS Konfiguration

(custom-set-variables
 ;; Custom variables
 '(centaur-full-name "Simon")         ; Dein Name
 '(centaur-mail-address "simon@knogler.dev")  ; Deine E-Mail
 
 ;; Editor Einstellungen
 '(delete-selection-mode t)               ; Markierung beim Tippen ersetzen
 '(tab-width 2)                           ; Tab-Breite
 '(indent-tabs-mode nil)                  ; Spaces statt Tabs
 
 ;; Visual settings
 (setq centaur-icon t)                          ; Display icons (t to enable, nil to disable)
 '(setq centaur-theme 'night)               ; Oder 'auto für System-Theme
 
 ;; Dashboard
 '(centaur-dashboard t)                   ; Dashboard aktivieren
 
 ;; Language Server Protocol
 '(lsp-keymap-prefix "C-c l")             ; LSP Prefix
 
 ;; Common Lisp
 '(inferior-lisp-program "sbcl")          ; Dein Lisp-Interpreter
 )
 
;; Feature toggles
(setq centaur-tree-sitter nil)                 ; Enable tree-sitter (requires Emacs 29+)
(setq centaur-player t)                        ; Enable media player controls
;;; custom.el ends here