# Chapter 2: Advanced SLY and Common Lisp Development

**Building on Chapter 1: From Koans to Real Projects**

Now that you're comfortable with the basics from the Lisp Koans, let's dive into professional Common Lisp development with SLY. This guide covers project structure, debugging, testing, package management, REPL-driven development, and the advanced features that make Lisp development uniquely powerful.

---

## Table of Contents

1. [Project Structure and ASDF](#project-structure-and-asdf)
2. [REPL-Driven Development Workflow](#repl-driven-development-workflow)
3. [Advanced SLY Features](#advanced-sly-features)
4. [Debugging Like a Pro](#debugging-like-a-pro)
5. [Package Management with Quicklisp](#package-management-with-quicklisp)
6. [Testing Your Code](#testing-your-code)
7. [Interactive Development Patterns](#interactive-development-patterns)
8. [Performance and Optimization](#performance-and-optimization)
9. [Working with Multiple Lisps](#working-with-multiple-lisps)
10. [Customizing Your Environment](#customizing-your-environment)

---

## Project Structure and ASDF

### What is ASDF?

ASDF (Another System Definition Facility) is Common Lisp's build system. Think of it like Make or Cargo, but Lispy.

### Creating Your First Project

**Manual Method:**

```
my-project/
├── my-project.asd          # System definition
├── package.lisp            # Package definitions
├── main.lisp              # Main code
├── utils.lisp             # Utility functions
└── tests/
    └── test-suite.lisp    # Tests
```

**Using Quickproject (recommended):**

```lisp
;; In REPL:
(ql:quickload :quickproject)
(quickproject:make-project #p"~/projects/my-app/" 
                           :depends-on '(:alexandria :str))
```

### Anatomy of an ASD File

**my-project.asd:**
```lisp
(defsystem "my-project"
  :version "0.1.0"
  :author "Your Name <your.email@example.com>"
  :license "MIT"
  :depends-on (:alexandria        ; Utilities
               :str               ; String manipulation
               :local-time        ; Time handling
               :cl-ppcre)         ; Regex
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "main" :depends-on ("package" "utils")))
  :description "Description of my project"
  :in-order-to ((test-op (test-op "my-project/tests"))))

;; Test system
(defsystem "my-project/tests"
  :depends-on (:my-project
               :fiveam)           ; Testing framework
  :components ((:module "tests"
                :components ((:file "test-suite"))))
  :perform (test-op (o c) (symbol-call :fiveam :run! 
                                       (find-symbol* :my-project-tests 
                                                     :my-project/tests))))
```

### Package Definition Best Practices

**package.lisp:**
```lisp
(defpackage #:my-project
  (:use #:cl)
  (:import-from #:alexandria
                #:when-let
                #:if-let
                #:hash-table-keys)
  (:import-from #:str
                #:concat
                #:split
                #:trim)
  (:export #:main
           #:process-data
           #:*config*))

(in-package #:my-project)
```

**Key Points:**
- Use `#:` for symbols to avoid interning in the current package
- Import commonly used utilities to avoid prefixes
- Export only your public API
- One package per file is common, but not required

### Loading Your Project in SLY

**Method 1: Using ASDF directly**
```lisp
;; Tell ASDF where your project is (one time setup)
(pushnew #p"~/projects/my-project/" asdf:*central-registry* :test #'equal)

;; Load your system
(asdf:load-system :my-project)

;; Switch to your package
(in-package :my-project)
```

**Method 2: Using Quicklisp local-projects**
```bash
# Symlink or move your project to quicklisp/local-projects/
ln -s ~/projects/my-project ~/quicklisp/local-projects/
```

```lisp
;; Then just:
(ql:quickload :my-project)
(in-package :my-project)
```

**SLY Shortcuts:**
- `C-c C-k` in your .asd file loads the entire system
- `C-c ~` to switch package in REPL (with completion!)
- `C-c M-k` to compile all files in current system

---

## REPL-Driven Development Workflow

This is where Lisp really shines. You don't write code and then run it—you grow your program interactively.

### The Basic Flow

```
1. Write function skeleton
2. Compile it (C-M-x)
3. Test it in REPL
4. Discover edge case or bug
5. Fix in editor
6. Recompile (C-M-x)
7. Re-test same REPL session (all state preserved!)
8. Repeat
```

### Example: Building a User Management System

**Step 1: Start with data structure**
```lisp
(defstruct user
  id
  name
  email
  (created-at (local-time:now)))
```

Compile it (`C-M-x`), then in REPL:
```lisp
CL-USER> (make-user :id 1 :name "Alice" :email "alice@example.com")
#S(USER :ID 1 :NAME "Alice" :EMAIL "alice@example.com" :CREATED-AT @2026-02-16...)
```

**Step 2: Add a storage mechanism**
```lisp
(defvar *users* (make-hash-table :test 'equal))

(defun add-user (name email)
  (let* ((id (hash-table-count *users*))
         (user (make-user :id id :name name :email email)))
    (setf (gethash id *users*) user)
    user))
```

Compile and test immediately:
```lisp
CL-USER> (add-user "Bob" "bob@example.com")
CL-USER> (add-user "Carol" "carol@example.com")
CL-USER> *users*  ; Inspect the hash table
```

**Step 3: Discover you need validation**
```lisp
CL-USER> (add-user "" "invalid")  ; Oops, this shouldn't work!
```

Fix it without restarting:
```lisp
(defun add-user (name email)
  (assert (and (stringp name) (> (length name) 0)) (name)
          "Name must be a non-empty string")
  (assert (cl-ppcre:scan "^[^@]+@[^@]+\\.[^@]+$" email) (email)
          "Invalid email format")
  (let* ((id (hash-table-count *users*))
         (user (make-user :id id :name name :email email)))
    (setf (gethash id *users*) user)
    user))
```

Compile (`C-M-x`) and your new version is live—no restart needed!

### Key Insight

Your REPL session is a living, evolving program. You can:
- Redefine functions
- Modify global variables
- Keep complex state between compilations
- Test immediately without build/run cycles

This is **exponentially faster** than the compile→run→crash→restart cycle.

---

## Advanced SLY Features

### Inspector: `C-c I`

The inspector lets you visually explore any object.

```lisp
;; In REPL:
CL-USER> (inspect *users*)
```

Or from code: put cursor on a variable and press `C-c I`.

**Inspector Commands:**
- `RET` on a field to inspect it
- `l` to go back
- `q` to quit
- `e` to evaluate in context
- `d` to describe
- `>` / `<` to navigate history

**Use cases:**
- Explore complex data structures
- Examine object slots
- See what's actually in that hash table
- Debug closure environments

### Cross-Reference: Understanding Your Codebase

**Who calls this function?**
- `M-x sly-who-calls` (`C-c C-w C-c`)

**Who references this variable?**
- `M-x sly-who-references` (`C-c C-w C-r`)

**Who sets this variable?**
- `M-x sly-who-sets` (`C-c C-w C-s`)

**Who binds this variable?**
- `M-x sly-who-binds` (`C-c C-w C-b`)

**Where is this macro used?**
- `M-x sly-who-macroexpands` (`C-c C-w C-m`)

**Example workflow:**
```lisp
;; You have a function you want to refactor
(defun calculate-total (items)
  ...)

;; Press C-c C-w C-c on 'calculate-total' to see everywhere it's called
;; Now you know the impact of changing its interface
```

### Macroexpansion: `C-c M-m`

Put cursor on a macro call and press `C-c M-m` to see what it expands to.

```lisp
(when-let ((user (find-user id)))
  (process user))

;; C-c M-m shows:
(let ((user (find-user id)))
  (when user
    (process user)))
```

**Useful for:**
- Understanding macros
- Debugging macro-generated code
- Learning from library macros

**Variants:**
- `C-c M-m`: Expand once
- `C-c C-m`: Expand all (recursive)
- `C-c M-t`: Macrostep (step through expansion interactively)

### Tracing: See Function Calls in Real Time

```lisp
;; In REPL or with C-c C-t:
(trace add-user find-user)

;; Now call your code:
(add-user "Dave" "dave@example.com")

;; Output shows:
;;   0: (ADD-USER "Dave" "dave@example.com")
;;   0: ADD-USER returned #S(USER ...)

;; Untrace:
(untrace)
```

**Advanced tracing:**
```lisp
;; Trace with conditions
(trace add-user :break t)  ; Break when called

;; In SLY:
(sly-toggle-trace-fdefinition)  ; C-c C-t on function name
```

### Disassemble: See the Assembly

For performance-critical code:
```lisp
(disassemble 'my-function)
```

Or in editor: `C-c M-d` on function name.

### Profiling

**Time a form:**
```lisp
(time (expensive-operation))
```

**Statistical profiler (SBCL):**
```lisp
(require :sb-sprof)
(sb-sprof:with-profiling (:report :flat :loop nil)
  (expensive-operation))
```

**SLY profiler integration:**
- `M-x sly-toggle-profile-fdefinition`
- `M-x sly-profile-package`
- `M-x sly-profiler-report`

---

## Debugging Like a Pro

### The Debugger is Your Friend

When an error occurs, SLY drops you into the debugger. Don't panic!

**Debugger Commands:**
```
v - View source (shows where error occurred)
e - Evaluate in frame (inspect variables)
t - Toggle backtrace view
r - Invoke a restart
a - Abort to top level
q - Quit debugger
c - Continue (if possible)
0-9 - Invoke restart by number
```

### Example Debug Session

You write:
```lisp
(defun divide-all (numbers divisor)
  (mapcar (lambda (n) (/ n divisor)) numbers))

(divide-all '(10 20 30) 0)  ; Oops!
```

**Debugger appears:**
```
Division by zero.
   [Condition of type DIVISION-BY-ZERO]

Restarts:
 0: [RETRY] Retry SLIME REPL evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING>)

Backtrace:
  0: (/ 10 0)
  1: (LAMBDA (N) :IN DIVIDE-ALL)
  2: (MAPCAR #<CLOSURE> (10 20 30))
  3: (DIVIDE-ALL (10 20 30) 0)
```

**Actions you can take:**

1. **Press `v`** to see the exact line of code
2. **Press `e`** in frame 0 and evaluate:
   ```lisp
   n         ; Check the number
   divisor   ; Check the divisor - ah, it's 0!
   ```
3. **Fix the function** in your editor:
   ```lisp
   (defun divide-all (numbers divisor)
     (assert (not (zerop divisor)) (divisor)
             "Divisor cannot be zero")
     (mapcar (lambda (n) (/ n divisor)) numbers))
   ```
4. **Compile** with `C-M-x`
5. **Press `0`** to restart from the beginning with new code

The key: **You don't lose your REPL state**. Fix and continue!

### Strategic Break Points

**Using `break`:**
```lisp
(defun complex-calculation (data)
  (let ((intermediate (process-step-1 data)))
    (break "Check intermediate: ~A" intermediate)  ; Pause here
    (process-step-2 intermediate)))
```

When `break` is hit:
- You're in the debugger
- Press `e` to inspect local variables
- Press `c` to continue

**Using `assert`:**
```lisp
(defun withdraw (account amount)
  (assert (>= (account-balance account) amount) ()
          "Insufficient funds: ~A < ~A" 
          (account-balance account) amount)
  (decf (account-balance account) amount))
```

Assertions are **contracts**—they document and enforce invariants.

### Stepping Through Code

For complex logic:
```lisp
(step (complex-function arg1 arg2))
```

The stepper shows each evaluation step. Use:
- `s` - Step into
- `n` - Next (over)
- `c` - Continue
- `q` - Quit

### Conditional Breakpoints

```lisp
(defun process-items (items)
  (dolist (item items)
    (when (and (item-expensive-p item)
               (> (item-price item) 1000))
      (break "Found expensive item: ~A" item))
    (process-item item)))
```

---

## Package Management with Quicklisp

### Installing Libraries

```lisp
;; Search for a library
(ql:system-apropos "json")

;; Install it
(ql:quickload :cl-json)

;; Use it
(use-package :json)
(encode-json-to-string '(("name" . "Alice") ("age" . 30)))
```

### Essential Libraries for Projects

**Utilities:**
```lisp
(ql:quickload :alexandria)    ; General utilities
(ql:quickload :serapeum)      ; More utilities
(ql:quickload :str)           ; String manipulation
```

**Web Development:**
```lisp
(ql:quickload :hunchentoot)   ; Web server
(ql:quickload :clack)         ; Web framework abstraction
(ql:quickload :caveman2)      ; Web framework
(ql:quickload :djula)         ; Templates
```

**Database:**
```lisp
(ql:quickload :postmodern)    ; PostgreSQL
(ql:quickload :mito)          ; ORM
(ql:quickload :sxql)          ; SQL DSL
```

**Testing:**
```lisp
(ql:quickload :fiveam)        ; Testing framework
(ql:quickload :prove)         ; Another testing framework
(ql:quickload :mockingbird)   ; Mocking
```

**JSON/Data:**
```lisp
(ql:quickload :cl-json)       ; JSON
(ql:quickload :jonathan)      ; Fast JSON
(ql:quickload :yason)         ; JSON
```

**Date/Time:**
```lisp
(ql:quickload :local-time)    ; Date/time handling
```

**HTTP Client:**
```lisp
(ql:quickload :drakma)        ; HTTP client
(ql:quickload :dexador)       ; Modern HTTP client
```

### Creating a Project Dependencies File

Good practice: Pin your dependencies.

**quicklisp/local-projects/my-project/my-project.asd:**
```lisp
(defsystem "my-project"
  :depends-on (:alexandria
               :str
               :local-time
               :cl-json
               :dexador
               :postmodern)
  ...)
```

Then just:
```lisp
(ql:quickload :my-project)  ; Installs all deps automatically
```

---

## Testing Your Code

### FiveAM: The Standard Testing Framework

**tests/test-suite.lisp:**
```lisp
(defpackage #:my-project/tests
  (:use #:cl #:fiveam #:my-project))

(in-package #:my-project/tests)

(def-suite my-project-tests
  :description "Main test suite for my-project")

(in-suite my-project-tests)

(test simple-addition
  "Test basic addition"
  (is (= 4 (+ 2 2)))
  (is (= 0 (+ -1 1))))

(test user-creation
  "Test user creation and validation"
  (let ((user (add-user "Test User" "test@example.com")))
    (is (user-p user))
    (is (string= "Test User" (user-name user)))
    (is (string= "test@example.com" (user-email user))))
  
  ;; Test validation
  (signals error
    (add-user "" "test@example.com"))
  
  (signals error
    (add-user "Test" "invalid-email")))

(test find-user-by-email
  "Test finding users by email"
  (let ((user (add-user "Alice" "alice@example.com")))
    (is (eq user (find-user-by-email "alice@example.com")))
    (is (null (find-user-by-email "nonexistent@example.com")))))
```

**Running Tests in SLY:**

```lisp
;; Load test system
(ql:quickload :my-project/tests)

;; Run all tests
(fiveam:run! 'my-project-tests)

;; Run specific test
(fiveam:run! 'user-creation)
```

**From command line:**
```bash
sbcl --eval "(ql:quickload :my-project/tests)" \
     --eval "(fiveam:run! 'my-project-tests)" \
     --quit
```

### TDD Workflow in SLY

1. **Write test first**
2. **Compile test** (`C-M-x`)
3. **Run test** - it fails (good!)
4. **Write implementation**
5. **Compile implementation** (`C-M-x`)
6. **Re-run test** in same REPL - no restart needed!
7. **Refine** until it passes

**Pro tip:** Keep a REPL buffer split with your code:
```
┌─────────────────┬──────────────────┐
│   Code Buffer   │   Test Buffer    │
│                 │                  │
├─────────────────┴──────────────────┤
│          REPL                      │
└────────────────────────────────────┘
```

Run tests with `C-c C-c (fiveam:run! 'suite)` or bind to key.

### Testing Assertions

FiveAM provides:
```lisp
(is (= expected actual))           ; Test equality
(is-true expr)                     ; Test truth
(is-false expr)                    ; Test falsity
(signals condition-type expr)      ; Test that error is raised
(finishes expr)                    ; Test that expr completes
(skip "Reason")                    ; Skip test
```

### Test Fixtures

For setup/teardown:
```lisp
(def-fixture user-db ()
  (let ((*users* (make-hash-table :test 'equal)))
    (&body)))

(test with-fixture
  (with-fixture user-db ()
    (add-user "Test" "test@example.com")
    (is (= 1 (hash-table-count *users*)))))
```

---

## Interactive Development Patterns

### The Scratch Buffer Technique

Keep a scratch file (`scratch.lisp`) for experiments:

```lisp
(in-package :my-project)

;; Quick test of new feature
(let ((data (fetch-data-from-api)))
  (inspect data)  ; See what you got
  (process-data data))

;; Prototype new function
(defun new-feature (input)
  (break)  ; Pause to inspect state
  ...)

;; Try different approaches
(time (approach-1 test-data))
(time (approach-2 test-data))
```

**Workflow:**
- Write experimental code
- `C-M-x` to evaluate
- See results immediately
- Iterate rapidly
- Move working code to real files

### Building Complex Functions Bottom-Up

Instead of top-down, build from the bottom:

```lisp
;; Start with the smallest piece
(defun parse-date (string)
  (local-time:parse-timestring string))

;; Test it:
(parse-date "2026-02-16")  ; Works!

;; Build next layer
(defun extract-dates (records)
  (mapcar (lambda (r) (parse-date (getf r :date))) records))

;; Test:
(extract-dates '((:date "2026-02-16") (:date "2026-02-17")))

;; Build complete function
(defun analyze-records (records)
  (let ((dates (extract-dates records)))
    (compute-statistics dates)))
```

Each piece is tested before moving up!

### Redefining Functions While Users Are Connected

In production (carefully!):

```lisp
;; Your server is running with old buggy code
(defun calculate-price (item)
  (item-cost item))  ; Bug: forgot tax!

;; Fix it live:
(defun calculate-price (item)
  (* (item-cost item) 1.20))  ; Now with tax

;; Compile with C-M-x
;; All new requests use the new code immediately!
;; No restart required!
```

This is powerful but **dangerous**—use carefully:
- Test in REPL first
- Have rollback ready
- Monitor behavior
- Use for hot-fixes, not regular deployment

### The Power of `*` and Friends

In the REPL:
```lisp
CL-USER> (+ 2 3)
5
CL-USER> *        ; Last result
5
CL-USER> (+ * 10)
15
CL-USER> **       ; Second-to-last result
5
CL-USER> ***      ; Third-to-last result
```

Also:
- `+`, `++`, `+++` - Last three input forms
- `-` - Current input form being evaluated
- `/`, `//`, `///` - Last three lists of multiple values

**Use case:**
```lisp
CL-USER> (fetch-user 42)
#S(USER :ID 42 :NAME "Alice" ...)

CL-USER> (setf my-user *)  ; Save that result!

CL-USER> (update-user my-user :name "Alicia")
```

---

## Performance and Optimization

### Declaration Basics

Help the compiler optimize:

```lisp
(defun fast-sum (numbers)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type list numbers))
  (loop for n of-type fixnum in numbers
        sum n))
```

**Optimize levels (0-3):**
- `speed` - How fast
- `safety` - Runtime checks
- `space` - Memory usage
- `debug` - Debugging info
- `compilation-speed` - Compile time

**Common pattern:**
```lisp
;; During development:
(declaim (optimize (speed 0) (safety 3) (debug 3)))

;; For production:
(declaim (optimize (speed 3) (safety 1) (debug 1)))
```

### Type Declarations

```lisp
(defun process-numbers (numbers)
  (declare (type (simple-array double-float (*)) numbers))
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (loop for n across numbers
          do (incf sum n))
    sum))
```

Check your declarations worked:
```lisp
(disassemble 'process-numbers)  ; Look for type checks
```

### Inline Functions

```lisp
(declaim (inline square))
(defun square (x)
  (* x x))

(defun sum-of-squares (a b)
  (+ (square a) (square b)))  ; 'square' inlined here
```

### Avoiding Consing

```lisp
;; Bad: Creates new list every time
(defun add-item (item list)
  (cons item list))

;; Better for hot loops:
(defun add-item! (item place)
  (push item place))  ; Modifies in place
```

### Profiling Workflow

1. **Write correct code first**
2. **Profile to find bottleneck**:
   ```lisp
   (time (my-function large-input))
   ```
3. **Optimize the bottleneck only**
4. **Re-profile to verify improvement**
5. **Stop when fast enough**

**Don't optimize prematurely!** Clarity > Speed until proven otherwise.

---

## Working with Multiple Lisps

SLY can connect to multiple Lisp implementations simultaneously.

### Switching Implementations

```lisp
;; Start SBCL
M-x sly

;; Start CMUCL in another connection
M-x sly-connect RET cmucl RET

;; Start CCL
M-x sly-connect RET ccl RET
```

**Switch between them:**
- `C-c C-z` jumps to current connection's REPL
- `M-x sly-mrepl` shows all connections
- `C-c C-x c` - choose connection

### Why Multiple Implementations?

- **Portability testing**: Ensure code works on SBCL, CCL, ECL, etc.
- **Performance comparison**: See which is faster for your use case
- **Platform-specific features**: Some libs only work on certain impls
- **Cross-implementation development**: iOS app (CCL) + server (SBCL)

### Example: Test on Multiple Lisps

```lisp
;; In connection 1 (SBCL):
CL-USER> (load "my-project.asd")
CL-USER> (asdf:test-system :my-project)

;; Switch to connection 2 (CCL):
CL-USER> (load "my-project.asd")
CL-USER> (asdf:test-system :my-project)

;; Compare results!
```

---

## Customizing Your Environment

### Essential .emacs Additions for SLY

```elisp
;; Automatically enable paredit in Lisp modes
(add-hook 'sly-mode-hook #'enable-paredit-mode)
(add-hook 'sly-repl-mode-hook #'enable-paredit-mode)

;; Better completion
(setq sly-complete-symbol-function 'sly-flex-completions)

;; Colorful parentheses
(add-hook 'sly-mode-hook #'rainbow-delimiters-mode)

;; Show argument list in minibuffer
(add-hook 'sly-mode-hook #'eldoc-mode)

;; Auto-compile when loading files
(setq sly-load-failed-fasl 'always)

;; Fuzzy completion
(add-to-list 'sly-contribs 'sly-fancy)
(add-to-list 'sly-contribs 'sly-scratch)
(add-to-list 'sly-contribs 'sly-mrepl)

;; Custom keybindings
(define-key sly-mode-map (kbd "C-c r") 'sly-eval-region)
(define-key sly-mode-map (kbd "C-c C-r") 'sly-eval-region)

;; Start SLY automatically when opening .lisp files
;; (Commented out - you may not always want this)
;; (add-hook 'sly-mode-hook
;;           (lambda ()
;;             (unless (sly-connected-p)
;;               (save-excursion (sly)))))
```

### SLY Contrib Packages

Enable additional features:

```elisp
(setq sly-contribs 
      '(sly-fancy           ; Collection of useful extensions
        sly-mrepl           ; Multiple REPLs
        sly-scratch         ; Scratch buffers
        sly-trace-dialog    ; Interactive tracing
        sly-stickers))      ; Execution stickers
```

### Custom Snippets with YASnippet

**~/.emacs.d/snippets/lisp-mode/defun:**
```
# -*- mode: snippet -*-
# name: defun
# key: defun
# --
(defun ${1:function-name} (${2:args})
  "${3:docstring}"
  $0)
```

**~/.emacs.d/snippets/lisp-mode/test:**
```
# -*- mode: snippet -*-
# name: test
# key: test
# --
(test ${1:test-name}
  "${2:description}"
  $0)
```

### Project-Specific Settings

**.dir-locals.el in your project root:**
```elisp
((lisp-mode . ((sly-lisp-implementations . 
                ((sbcl ("sbcl" "--dynamic-space-size" "4096"))))
               (indent-tabs-mode . nil)
               (fill-column . 80))))
```

---

## Advanced Patterns and Idioms

### Condition System for Flow Control

Unlike exceptions, conditions don't unwind the stack:

```lisp
(define-condition insufficient-funds (error)
  ((amount :initarg :amount :reader amount)
   (balance :initarg :balance :reader balance)))

(defun withdraw (account amount)
  (when (< (account-balance account) amount)
    (error 'insufficient-funds 
           :amount amount 
           :balance (account-balance account)))
  (decf (account-balance account) amount))

;; Higher level code can handle it:
(defun process-withdrawal (account amount)
  (handler-case 
      (withdraw account amount)
    (insufficient-funds (c)
      (format t "Cannot withdraw ~A, balance is ~A~%" 
              (amount c) (balance c))
      nil)))

;; Or restart it:
(defun process-withdrawal-with-restart (account amount)
  (restart-case 
      (withdraw account amount)
    (reduce-amount (new-amount)
      :report "Withdraw a smaller amount"
      :interactive (lambda () 
                     (list (read-number "New amount: ")))
      (withdraw account new-amount))))
```

**In debugger, user can:**
- Choose "reduce-amount" restart
- Enter new amount
- Continue without unwinding!

### Method Combination

```lisp
(defgeneric calculate-price (item)
  (:method-combination +))

(defmethod calculate-price + ((item book))
  (book-base-price item))

(defmethod calculate-price + ((item book))
  (book-shipping-cost item))

(defmethod calculate-price + ((item book))
  (calculate-tax item))

;; Result is sum of all applicable methods!
(calculate-price my-book)
```

### Macros for DSLs

```lisp
(defmacro with-transaction (db &body body)
  `(let ((transaction (begin-transaction ,db)))
     (handler-case
         (progn
           ,@body
           (commit-transaction transaction))
       (error (e)
         (rollback-transaction transaction)
         (error e)))))

;; Usage:
(with-transaction *db*
  (insert-user user)
  (insert-order order))
```

### Pattern Matching with Trivia

```lisp
(ql:quickload :trivia)

(use-package :trivia)

(defun process-message (msg)
  (match msg
    ((list 'user-login username)
     (handle-login username))
    
    ((list 'user-logout username)
     (handle-logout username))
    
    ((list 'send-message from to text)
     (deliver-message from to text))
    
    (_
     (error "Unknown message: ~A" msg))))
```

---

## Real-World Project Example

Let's tie it all together with a small but complete project.

### Project: Task Tracker

**Structure:**
```
task-tracker/
├── task-tracker.asd
├── package.lisp
├── model.lisp
├── storage.lisp
├── api.lisp
└── tests/
    └── test-suite.lisp
```

**task-tracker.asd:**
```lisp
(defsystem "task-tracker"
  :version "0.1.0"
  :author "Your Name"
  :license "MIT"
  :depends-on (:alexandria :local-time :fiveam)
  :components ((:file "package")
               (:file "model" :depends-on ("package"))
               (:file "storage" :depends-on ("package" "model"))
               (:file "api" :depends-on ("package" "model" "storage")))
  :in-order-to ((test-op (test-op "task-tracker/tests"))))

(defsystem "task-tracker/tests"
  :depends-on (:task-tracker :fiveam)
  :components ((:module "tests"
                :components ((:file "test-suite"))))
  :perform (test-op (o c) 
             (symbol-call :fiveam :run! :task-tracker-tests)))
```

**package.lisp:**
```lisp
(defpackage #:task-tracker
  (:use #:cl)
  (:import-from #:alexandria
                #:when-let
                #:if-let)
  (:export #:make-task
           #:task-p
           #:add-task
           #:complete-task
           #:list-tasks
           #:find-task))

(in-package #:task-tracker)
```

**model.lisp:**
```lisp
(in-package #:task-tracker)

(defstruct task
  id
  title
  description
  (status :pending)
  (created-at (local-time:now))
  completed-at)

(defun task-completed-p (task)
  (eq (task-status task) :completed))
```

**storage.lisp:**
```lisp
(in-package #:task-tracker)

(defvar *tasks* (make-hash-table))
(defvar *next-id* 1)

(defun add-task (title &optional description)
  (let ((task (make-task :id *next-id*
                        :title title
                        :description description)))
    (setf (gethash *next-id* *tasks*) task)
    (incf *next-id*)
    task))

(defun find-task (id)
  (gethash id *tasks*))

(defun list-tasks (&key (status nil))
  (let ((tasks (alexandria:hash-table-values *tasks*)))
    (if status
        (remove-if-not (lambda (task) 
                        (eq (task-status task) status))
                      tasks)
        tasks)))

(defun complete-task (id)
  (when-let ((task (find-task id)))
    (setf (task-status task) :completed
          (task-completed-at task) (local-time:now))
    task))
```

**api.lisp:**
```lisp
(in-package #:task-tracker)

(defun create-task-interactive ()
  (format t "Title: ")
  (let ((title (read-line)))
    (format t "Description (optional): ")
    (let ((desc (read-line)))
      (add-task title (if (string= desc "") nil desc)))))

(defun show-tasks ()
  (let ((tasks (list-tasks)))
    (if tasks
        (dolist (task tasks)
          (format t "~&[~A] ~A - ~A~%"
                  (task-id task)
                  (task-status task)
                  (task-title task)))
        (format t "No tasks.~%"))))

(defun main ()
  (loop
    (format t "~%Commands: (a)dd, (l)ist, (c)omplete, (q)uit~%> ")
    (let ((cmd (read-line)))
      (cond
        ((string= cmd "a") (create-task-interactive))
        ((string= cmd "l") (show-tasks))
        ((string= cmd "c") 
         (format t "Task ID: ")
         (let ((id (parse-integer (read-line))))
           (if (complete-task id)
               (format t "Task completed!~%")
               (format t "Task not found.~%"))))
        ((string= cmd "q") (return))
        (t (format t "Unknown command.~%"))))))
```

**tests/test-suite.lisp:**
```lisp
(defpackage #:task-tracker/tests
  (:use #:cl #:fiveam #:task-tracker))

(in-package #:task-tracker/tests)

(def-suite task-tracker-tests)
(in-suite task-tracker-tests)

(test create-task
  (let ((task (add-task "Test task" "Description")))
    (is (task-p task))
    (is (string= "Test task" (task-title task)))
    (is (eq :pending (task-status task)))))

(test complete-task
  (let* ((task (add-task "Test task"))
         (id (task-id task)))
    (complete-task id)
    (is (eq :completed (task-status (find-task id))))
    (is (not (null (task-completed-at (find-task id)))))))

(test list-tasks-by-status
  (add-task "Task 1")
  (let ((task2 (add-task "Task 2")))
    (complete-task (task-id task2)))
  
  (is (= 1 (length (list-tasks :status :pending))))
  (is (= 1 (length (list-tasks :status :completed)))))
```

### Development Workflow

```lisp
;; 1. Load project
(ql:quickload :task-tracker)
(in-package :task-tracker)

;; 2. Test interactively
(add-task "First task" "A test task")
(list-tasks)

;; 3. Run tests
(ql:quickload :task-tracker/tests)
(fiveam:run! :task-tracker-tests)

;; 4. Try the UI
(main)
```

---

## Workflow Summary

### The Professional Lisp Developer's Day

**Morning: Starting up**
```
1. emacs
2. M-x sly
3. C-x C-f ~/projects/my-app/main.lisp
4. (ql:quickload :my-app)
5. (in-package :my-app)
```

**Development: The flow**
```
1. Write function/modify code
2. C-M-x (compile it)
3. C-c C-z (jump to REPL)
4. Test it manually
5. Discover issue
6. C-c C-z (back to code) or stay in REPL and trace/inspect
7. Fix
8. C-M-x (recompile)
9. Test again - NO RESTART NEEDED
10. Repeat until working
11. Write test
12. Run (fiveam:run! 'my-suite)
13. Commit
```

**Debugging: When things break**
```
1. Error happens
2. Debugger appears
3. Press 'v' to see source
4. Press 'e' to evaluate in context
5. Inspect variables
6. Fix in editor
7. C-M-x
8. Press '0' to restart with new code
9. Continue
```

**End of day**
```
1. Run full test suite
2. Save all buffers (C-x s)
3. Commit changes
4. M-x sly-quit-lisp (or just close Emacs)
```

---

## Quick Reference: Advanced Commands

### SLY Evaluation
| Command | Key | What It Does |
|---------|-----|--------------|
| sly-eval-defun | `C-M-x` | Compile/eval top-level form |
| sly-eval-last-expression | `C-x C-e` | Eval expression before point |
| sly-eval-region | `C-c C-r` | Eval selected region |
| sly-eval-buffer | `C-c C-k` | Compile/load entire file |
| sly-interactive-eval | `C-c C-e` | Eval with output to REPL |
| sly-pprint-eval-last-expression | `C-c C-p` | Eval and pretty-print |

### SLY Navigation
| Command | Key | What It Does |
|---------|-----|--------------|
| sly-edit-definition | `M-.` | Jump to definition |
| sly-pop-find-definition-stack | `M-,` | Jump back |
| sly-next-note | `M-n` | Next compiler note |
| sly-previous-note | `M-p` | Previous compiler note |
| sly-list-compiler-notes | `C-c M-c` | List all compiler notes |

### SLY Documentation
| Command | Key | What It Does |
|---------|-----|--------------|
| sly-describe-symbol | `C-c C-d d` | Describe symbol |
| sly-documentation | `C-c C-d h` | HyperSpec lookup |
| sly-apropos | `C-c C-d a` | Apropos search |
| sly-apropos-package | `C-c C-d p` | Apropos in package |

### SLY Debugging
| Command | Key | What It Does |
|---------|-----|--------------|
| sly-toggle-trace-fdefinition | `C-c C-t` | Toggle tracing |
| sly-untrace-all | `C-c U` | Untrace everything |
| sly-disassemble-symbol | `C-c M-d` | Disassemble function |
| sly-macroexpand-1 | `C-c M-m` | Macroexpand once |
| sly-macroexpand-all | `C-c C-m` | Macroexpand recursively |
| sly-inspect | `C-c I` | Inspect object |

### SLY REPL
| Command | Key | What It Does |
|---------|-----|--------------|
| sly | `M-x sly` | Start SLY |
| sly-mrepl | `C-c C-z` | Switch to REPL |
| sly-quit-lisp | `C-c C-q` | Quit Lisp |
| sly-interrupt | `C-c C-b` | Interrupt Lisp |
| sly-sync-package-and-default-directory | `C-c ~` | Sync package/directory |

### SLY Cross-Reference
| Command | Key | What It Does |
|---------|-----|--------------|
| sly-who-calls | `C-c C-w C-c` | Who calls this |
| sly-who-references | `C-c C-w C-r` | Who references this |
| sly-who-sets | `C-c C-w C-s` | Who sets this |
| sly-who-binds | `C-c C-w C-b` | Who binds this |
| sly-who-macroexpands | `C-c C-w C-m` | Who uses this macro |

---

## Common Pitfalls and Solutions

### 1. Package Not Found
**Problem:**
```lisp
CL-USER> (in-package :my-app)
Error: Package MY-APP does not exist
```

**Solution:**
```lisp
;; Load the system first
(ql:quickload :my-app)
;; Then switch
(in-package :my-app)
```

### 2. Function Not Found After Recompile
**Problem:** Changed function signature but old callers still exist.

**Solution:**
```lisp
;; Recompile all files that use it
C-c M-k  ; In .asd file or
(asdf:load-system :my-app :force t)
```

### 3. Stale Closures
**Problem:** Function captures old variable value.

**Solution:**
```lisp
;; Recompile the function that creates the closure
;; AND recompile/re-run the code that calls it
```

### 4. Reader Errors
**Problem:**
```
Error: Reader error: ...
```

**Solution:**
- Check for unmatched parentheses: `M-x check-parens`
- Look for unquoted symbols in data: Use `'(...)` or `#(...)`
- Check for package markers: `package:symbol` or `package::symbol`

### 5. ASDF Can't Find System
**Problem:**
```
System "my-app" not found
```

**Solution:**
```lisp
;; Option 1: Add to central registry
(push #p"~/projects/my-app/" asdf:*central-registry*)

;; Option 2: Symlink to Quicklisp
;; ln -s ~/projects/my-app ~/quicklisp/local-projects/

;; Then:
(asdf:load-system :my-app)
```

### 6. Type Errors in Tight Loops
**Problem:** Declaration doesn't match reality.

**Solution:**
```lisp
;; Check declarations carefully
(defun process (items)
  (declare (type list items))  ; Is it REALLY always a list?
  ...)

;; Consider removing declarations during development:
(declaim (optimize (speed 0) (safety 3)))
```

---

## Resources for Continued Learning

### Books
- **Practical Common Lisp** by Peter Seibel (free online)
- **Land of Lisp** by Conrad Barski (fun, cartoons!)
- **ANSI Common Lisp** by Paul Graham (reference)
- **On Lisp** by Paul Graham (macros deep-dive, free)
- **Let Over Lambda** by Doug Hoyte (advanced macros)
- **The Art of the Metaobject Protocol** (CLOS deep-dive)

### Online Resources
- **CLiki**: https://www.cliki.net/
- **Common Lisp Cookbook**: https://lispcookbook.github.io/cl-cookbook/
- **Awesome-CL**: https://github.com/CodyReichert/awesome-cl
- **Common Lisp HyperSpec**: http://www.lispworks.com/documentation/HyperSpec/
- **Quickdocs**: http://quickdocs.org/ (library documentation)

### Communities
- **r/Common_Lisp** - Reddit community
- **#commonlisp** on Libera.Chat IRC
- **Common Lisp Discord**
- **Lisp Forum**: https://lisp-lang.org/

### Practice Projects
1. **Web scraper** - Learn HTTP, parsing, data structures
2. **REST API** - Learn web frameworks, JSON, routing
3. **Database app** - Learn Postmodern, SQL, ORMs
4. **CLI tool** - Learn command-line parsing, file I/O
5. **Game** - Learn graphics, state management, loops
6. **DSL** - Learn macros, readers, compilers

---

## Next Steps: Chapter 3 Preview

In Chapter 3, we'll cover:
- **CLOG**: Building modern web UIs with Common Lisp
- **Postmodern**: Serious PostgreSQL work
- **Web frameworks**: Caveman2, Clack, and more
- **Authentication & Security**: Building secure business apps
- **Deployment**: Getting your app to production
- **Monitoring & Logging**: Keeping your app healthy

Stay tuned!

---

## Closing Thoughts

The key to mastering Common Lisp development is **embracing the interactive workflow**. This isn't just about Lisp the language—it's about Lisp the development experience.

Your code is alive. It grows. It evolves. You shape it in real-time, like a sculptor working with clay, not like a mason stacking bricks.

**Key takeaways:**
1. **Never restart** unless you have to
2. **Test immediately** after every change
3. **Use the debugger** as a tool, not an enemy
4. **Build bottom-up** from working pieces
5. **Keep state** in your REPL between compilations
6. **Inspect everything** with the inspector and trace
7. **Write tests** but also experiment interactively
8. **Profile** only after it works, and only when it matters

You're not just writing code—you're having a conversation with your program.

Welcome to the Lisp way.

---

*Happy hacking!*
*February 2026*
