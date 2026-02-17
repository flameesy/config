;;;; clang.el --- C/C++ development setup -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Setup Instructions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STEP 1: Install Tree-sitter grammars
;;   Run: M-x clang-setup-install-grammars
;;   This downloads and compiles the C/C++ parsers
;;
;; STEP 2: Install clangd LSP server
;;   - Linux: sudo dnf install clangd  (or pacman -S clang)
;;   - Windows: Download from LLVM releases
;;   Verify: clangd --version
;;
;; STEP 3: (Optional) Install CMake language server
;;   pip install cmake-language-server
;;
;; STEP 4: Project setup
;;   C/C++ projects need a compile_commands.json for best LSP experience
;;   
;;   For CMake projects:
;;     cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .
;;   
;;   For Make projects:
;;     Use Bear: bear -- make
;;   
;;   This tells clangd how to compile your code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tree-sitter for C/C++
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clang-setup-install-grammars ()
  "Install Tree-sitter grammars for C/C++ development."
  (interactive)
  (dolist (grammar
           '((c "https://github.com/tree-sitter/tree-sitter-c")
             (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
             (cmake "https://github.com/uyha/tree-sitter-cmake")))
    (add-to-list 'treesit-language-source-alist grammar)
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))

;; Auto-install grammars on first load
(when (treesit-available-p)
  (clang-setup-install-grammars))

;; File associations
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-ts-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; C/C++ Style and Formatting
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq c-basic-offset 4)           ; 4 spaces per indent level
(setq c-default-style "linux")    ; Linux kernel style (K&R-ish)

;; Alternative styles: "gnu", "k&r", "bsd", "stroustrup", "whitesmith", "ellemtel"
;; Customize per-mode if needed:
;; (setq c-default-style '((java-mode . "java")
;;                         (awk-mode . "awk")
;;                         (other . "linux")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Eglot LSP integration (uses clangd)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'eglot
  ;; Enable eglot for C/C++ modes
  (add-hook 'c-ts-mode-hook 'eglot-ensure)
  (add-hook 'c++-ts-mode-hook 'eglot-ensure)
  (add-hook 'cmake-ts-mode-hook 'eglot-ensure)
  
  ;; Clangd-specific settings
  (add-to-list 'eglot-server-programs
               '((c-ts-mode c++-ts-mode) 
                 . ("clangd"
                    "--background-index"
                    "--clang-tidy"
                    "--completion-style=detailed"
                    "--header-insertion=iwyu"
                    "--header-insertion-decorators")))
  
  ;; CMake LSP
  (add-to-list 'eglot-server-programs
               '(cmake-ts-mode . ("cmake-language-server"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compilation and debugging
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make compile command smarter
(setq compilation-scroll-output t)  ; Auto-scroll compilation buffer

;; Common compile commands
(defun clang-compile-make ()
  "Compile with make."
  (interactive)
  (compile "make -j$(nproc)"))

(defun clang-compile-cmake ()
  "Compile with cmake."
  (interactive)
  (compile "cmake --build build -j$(nproc)"))

(defun clang-run-executable ()
  "Run the compiled executable."
  (interactive)
  (let ((exe (read-file-name "Executable: " default-directory)))
    (compile exe)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GDB integration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gdb-many-windows t)          ; Use multi-window GDB layout
(setq gdb-show-main t)             ; Show main source window

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Keybindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clang-setup-keybindings ()
  "Setup C/C++ keybindings."
  (local-set-key (kbd "C-c C-c") 'compile)
  (local-set-key (kbd "C-c C-m") 'clang-compile-make)
  (local-set-key (kbd "C-c C-k") 'clang-compile-cmake)
  (local-set-key (kbd "C-c C-r") 'clang-run-executable)
  (local-set-key (kbd "C-c C-d") 'gdb))

(add-hook 'c-ts-mode-hook 'clang-setup-keybindings)
(add-hook 'c++-ts-mode-hook 'clang-setup-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Menu bar
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'menu-bar
  (defvar knoglerdev-clang-menu (make-sparse-keymap "C/C++"))
  
  (define-key knoglerdev-clang-menu [gdb]
    '(menu-item "Debug with GDB" gdb
                :help "Start GDB debugger (C-c C-d)"))
  
  (define-key knoglerdev-clang-menu [run]
    '(menu-item "Run Executable" clang-run-executable
                :help "Run compiled program (C-c C-r)"))
  
  (define-key knoglerdev-clang-menu [separator] '(menu-item "--"))
  
  (define-key knoglerdev-clang-menu [cmake]
    '(menu-item "Compile (CMake)" clang-compile-cmake
                :help "Build with CMake (C-c C-k)"))
  
  (define-key knoglerdev-clang-menu [make]
    '(menu-item "Compile (Make)" clang-compile-make
                :help "Build with Make (C-c C-m)"))
  
  (define-key knoglerdev-clang-menu [compile]
    '(menu-item "Compile..." compile
                :help "Run compile command (C-c C-c)"))
  
  (define-key knoglerdev-clang-menu [separator-2] '(menu-item "--"))
  
  (define-key knoglerdev-clang-menu [install-grammars]
    '(menu-item "Install Tree-sitter Grammars" clang-setup-install-grammars
                :help "Install C/C++ grammars"))
  
  (define-key-after global-map [menu-bar clang]
    (cons "C/C++" knoglerdev-clang-menu)
    'web-dev))

(provide 'clang)
;;; clang.el ends here