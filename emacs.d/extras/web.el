;;; web.el --- Web development additions  -*- lexical-binding: t; -*-
;;;
;;; Additions to dev.el for web development:
;;; - TypeScript/JavaScript modes
;;; - Web-mode for HTML/CSS/JSX
;;; - Emmet for HTML abbreviations
;;; - Tree-sitter integration
;;;
;;; REQUIREMENTS (optional - only if you want LSP features):
;;; Tree-sitter grammars (one-time setup):
;;;   M-x treesit-install-language-grammar RET typescript RET
;;;   M-x treesit-install-language-grammar RET tsx RET
;;;   M-x treesit-install-language-grammar RET javascript RET
;;;
;;; TypeScript Language Server (only if you want IDE features):
;;;   npm install -g typescript-language-server typescript
;;;   (or use Deno/Bun which have built-in TypeScript support)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Web-Mode - For HTML/CSS/JS mixed files
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.jsx?\\'" . web-mode)  ; Can use web-mode for JSX too
         ("\\.tsx?\\'" . web-mode)  ; Or for TSX
         ("\\.vue\\'" . web-mode)
         ("\\.svelte\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-current-element-highlight t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tree-Sitter Support
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tree-sitter is configured in dev.el via major-mode-remap-alist
;; Just install the grammars once (one-time setup):
;;   M-x treesit-install-language-grammar RET typescript RET
;;   M-x treesit-install-language-grammar RET tsx RET
;;   M-x treesit-install-language-grammar RET javascript RET

;; Indentation settings for TypeScript/JavaScript
(setq typescript-ts-mode-indent-offset 2
      js-indent-level 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Eglot TS Integration (optional - only if you installed the language server
;;;                                          (and actually need TypeScript))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(with-eval-after-load 'eglot
  ;; (add-hook 'typescript-ts-mode-hook #'eglot-ensure)
  ;; (add-hook 'tsx-ts-mode-hook #'eglot-ensure)
  ;; (add-hook 'js-ts-mode-hook #'eglot-ensure)
  
  ;; Configure TypeScript server for web-mode
  (add-to-list 'eglot-server-programs
               '(web-mode . ("typescript-language-server" "--stdio"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Emmet - HTML/CSS abbreviation expansion (optional)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Type: div.container>ul>li*3 then hit C-j to expand
;; Uncomment if you want it:

;; (use-package emmet-mode
;;   :ensure t
;;   :hook ((web-mode . emmet-mode)
;;          (html-mode . emmet-mode)
;;          (css-mode . emmet-mode))
;;   :custom
;;   (emmet-move-cursor-between-quotes t)
;;   :bind (:map emmet-mode-keymap
;;               ("C-j" . emmet-expand-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Additional helpful settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sensible indentation defaults
(setq js-indent-level 2
      css-indent-offset 2)

;; Auto-close pairs
(add-hook 'typescript-ts-mode-hook #'electric-pair-mode)
(add-hook 'js-ts-mode-hook #'electric-pair-mode)
(add-hook 'web-mode-hook #'electric-pair-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Project-specific settings via .dir-locals.el
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Example .dir-locals.el for your projects:
;;
;; ((typescript-mode . ((typescript-indent-level . 4)
;;                      (eglot-workspace-configuration
;;                       . (:typescript (:preferences (:quoteStyle "single")))))))
;;
;; This lets you customize settings per-project

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Documentation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/web-dev-help ()
  "Show web development quick start."
  (interactive)
  (with-help-window "*Web Development Help*"
    (princ "Web Development in Emacs\n")
    (princ "=================================\n\n")
    (princ "Basic Setup (NO npm required!):\n")
    (princ "  Just install tree-sitter grammars:\n")
    (princ "    M-x treesit-install-language-grammar RET typescript\n")
    (princ "    M-x treesit-install-language-grammar RET tsx\n")
    (princ "    M-x treesit-install-language-grammar RET javascript\n\n")
    (princ "Optional IDE Features (requires npm):\n")
    (princ "  If you want autocomplete/jump-to-def/rename:\n")
    (princ "    npm install -g typescript-language-server typescript\n")
    (princ "  Then uncomment the Eglot hooks in web.el\n\n")
    (princ "File Types:\n")
    (princ "  .ts/.tsx     → TypeScript with tree-sitter\n")
    (princ "  .js/.jsx     → JavaScript with tree-sitter\n")
    (princ "  .html/.css   → Web-mode (mixed HTML/CSS/JS)\n\n")
    (princ "Key Features:\n")
    (princ "  - Syntax highlighting (tree-sitter)\n")
    (princ "  - Auto-pairing brackets\n")
    (princ "  - Smart indentation\n")
    (princ "  - Optional: LSP features with Eglot\n")))

(provide 'web)
