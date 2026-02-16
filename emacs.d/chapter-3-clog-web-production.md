# Chapter 3: CLOG, Web Development, and Production Systems

**Building Real Business Applications with Common Lisp**

Now that you're comfortable with Lisp development and SLY workflows, let's build production-ready web applications. This chapter focuses on CLOG (Common Lisp Omnificent GUI), PostgreSQL integration, authentication, security, and deployment strategies for internal business management systems.

---

## Table of Contents

1. [Introduction to CLOG](#introduction-to-clog)
2. [CLOG Fundamentals](#clog-fundamentals)
3. [Building Your First CLOG App](#building-your-first-clog-app)
4. [PostgreSQL with Postmodern](#postgresql-with-postmodern)
5. [Authentication and Authorization](#authentication-and-authorization)
6. [Security Best Practices](#security-best-practices)
7. [Building a Complete Business App](#building-a-complete-business-app)
8. [CSS Frameworks: Milligram & PureCSS](#css-frameworks-milligram-and-purecss)
9. [Form Validation and Data Integrity](#form-validation-and-data-integrity)
10. [Session Management](#session-management)
11. [Alternative Web Frameworks](#alternative-web-frameworks)
12. [Testing Web Applications](#testing-web-applications)
13. [Deployment Strategies](#deployment-strategies)
14. [Monitoring and Logging](#monitoring-and-logging)
15. [Performance Optimization](#performance-optimization)

---

## Introduction to CLOG

### What is CLOG?

CLOG (Common Lisp Omnificent GUI) is a framework for building web-based GUIs entirely in Common Lisp. Unlike traditional web frameworks where you write HTML/CSS/JavaScript separately, CLOG lets you build interactive UIs using Lisp code that generates and controls the DOM.

**Key advantages:**
- **No JavaScript needed** - Write everything in Lisp
- **Real-time bidirectional communication** - WebSocket-based
- **Event-driven** - Natural callback model
- **Reactive** - UI updates automatically
- **Desktop-like experience** - Feels like traditional GUI programming
- **Perfect for internal business apps** - Focus on functionality over design

### When to Use CLOG

**Ideal for:**
- Internal business management systems
- Admin dashboards
- Data entry applications
- Real-time monitoring tools
- Line-of-business applications
- Tools where developers are the primary users

**Consider alternatives for:**
- Public-facing marketing sites
- Content-heavy websites
- SEO-critical pages
- Sites requiring broad browser compatibility
- Projects with separate frontend teams

### Installation

```lisp
(ql:quickload :clog)
```

That's it! CLOG is batteries-included.

---

## CLOG Fundamentals

### The CLOG Architecture

```
Browser (WebSocket Client)
         ↕
    CLOG Server
         ↕
  Your Lisp Code
         ↕
     Database
```

**Key concepts:**
1. **Body** - The root of your UI (the `<body>` element)
2. **Elements** - DOM elements (divs, buttons, inputs, etc.)
3. **Events** - User interactions (clicks, typing, etc.)
4. **Connections** - Each browser tab is a separate connection
5. **Handlers** - Lisp functions that respond to events

### Hello World in CLOG

```lisp
(defpackage #:clog-hello
  (:use #:cl #:clog))

(in-package #:clog-hello)

(defun on-new-window (body)
  "Called when a browser connects"
  (create-div body :content "Hello, CLOG World!"))

(defun start-app ()
  (initialize 'on-new-window)
  (open-browser))
```

**Run it:**
```lisp
(ql:quickload :clog-hello)
(clog-hello:start-app)
```

A browser opens showing "Hello, CLOG World!"

### Understanding the Flow

1. `initialize` starts the CLOG server
2. Browser connects → WebSocket established
3. `on-new-window` called with `body` object
4. You build UI by creating elements attached to `body`
5. Events trigger your Lisp callbacks
6. Changes propagate to browser automatically

---

## CLOG Fundamentals - Building UIs

### Creating Elements

```lisp
(defun on-new-window (body)
  ;; Create a div
  (let ((container (create-div body :class "container")))
    
    ;; Create a heading
    (create-h1 container :content "Welcome to My App")
    
    ;; Create a paragraph
    (create-p container :content "This is a CLOG application.")
    
    ;; Create a button
    (let ((btn (create-button container :content "Click Me")))
      ;; Add event handler
      (set-on-click btn
        (lambda (obj)
          (declare (ignore obj))
          (alert (window body) "Button clicked!"))))))
```

### Common Elements

```lisp
;; Text elements
(create-h1 parent :content "Heading 1")
(create-h2 parent :content "Heading 2")
(create-p parent :content "Paragraph")
(create-span parent :content "Span text")

;; Containers
(create-div parent :class "my-class")
(create-section parent)

;; Forms
(create-form parent)
(create-form-element parent :input :name "username")
(create-form-element parent :password :name "password")
(create-text-area parent :name "description")
(create-select parent :name "category")

;; Buttons
(create-button parent :content "Submit")

;; Links
(create-a parent :content "Click here" :link "https://example.com")

;; Images
(create-img parent :url-src "/images/logo.png")

;; Tables
(let ((table (create-table parent)))
  (let ((row (create-table-row table)))
    (create-table-heading row :content "Name")
    (create-table-heading row :content "Email")))

;; Lists
(let ((ul (create-unordered-list parent)))
  (create-list-item ul :content "Item 1")
  (create-list-item ul :content "Item 2"))
```

### Setting Properties and Attributes

```lisp
(let ((input (create-form-element body :input)))
  ;; Set attributes
  (set-attribute input "placeholder" "Enter your name")
  (set-attribute input "required" "true")
  
  ;; Set properties
  (setf (property input "value") "Default value")
  
  ;; Get values
  (property input "value")
  
  ;; CSS classes
  (add-class input "form-control")
  (remove-class input "old-class")
  (toggle-class input "active")
  
  ;; Inline styles
  (setf (style input "color") "red")
  (setf (style input "font-size") "16px"))
```

### Event Handling

```lisp
(defun on-new-window (body)
  (let ((input (create-form-element body :input))
        (display (create-div body)))
    
    ;; Click event
    (set-on-click input
      (lambda (obj)
        (setf (text display) "Input clicked!")))
    
    ;; Change event (when value changes)
    (set-on-change input
      (lambda (obj)
        (setf (text display) 
              (format nil "Value: ~A" (value obj)))))
    
    ;; Input event (real-time typing)
    (set-on-input input
      (lambda (obj)
        (setf (text display)
              (format nil "Typing: ~A" (value obj)))))
    
    ;; Focus events
    (set-on-focus input
      (lambda (obj)
        (add-class obj "focused")))
    
    (set-on-blur input
      (lambda (obj)
        (remove-class obj "focused")))))
```

### Working with Values

```lisp
;; Input elements
(let ((input (create-form-element body :input)))
  (setf (value input) "Initial value")
  (print (value input)))

;; Text areas
(let ((textarea (create-text-area body)))
  (setf (text-value textarea) "Multi-line\ntext"))

;; Checkboxes
(let ((checkbox (create-form-element body :checkbox)))
  (setf (checkbox-checked-p checkbox) t)
  (when (checkbox-checked-p checkbox)
    (print "Checked!")))

;; Select elements
(let ((select (create-select body)))
  (create-option select :content "Option 1" :value "1")
  (create-option select :content "Option 2" :value "2")
  (setf (value select) "2"))
```

---

## Building Your First CLOG App

Let's build a simple but complete task manager.

### Project Structure

```
task-manager/
├── task-manager.asd
├── package.lisp
├── model.lisp
├── ui.lisp
├── main.lisp
└── static/
    └── css/
        └── style.css
```

### task-manager.asd

```lisp
(defsystem "task-manager"
  :version "0.1.0"
  :author "Your Name"
  :license "MIT"
  :depends-on (:clog :local-time)
  :components ((:file "package")
               (:file "model" :depends-on ("package"))
               (:file "ui" :depends-on ("package" "model"))
               (:file "main" :depends-on ("package" "model" "ui")))
  :description "A simple task manager built with CLOG")
```

### package.lisp

```lisp
(defpackage #:task-manager
  (:use #:cl #:clog)
  (:export #:start-app
           #:stop-app))

(in-package #:task-manager)
```

### model.lisp

```lisp
(in-package #:task-manager)

(defstruct task
  id
  title
  description
  completed
  (created-at (local-time:now)))

(defvar *tasks* nil)
(defvar *next-id* 1)
(defvar *tasks-lock* (bt:make-lock "tasks-lock"))

(defun add-task (title description)
  "Add a new task"
  (bt:with-lock-held (*tasks-lock*)
    (let ((task (make-task :id *next-id*
                          :title title
                          :description description
                          :completed nil)))
      (push task *tasks*)
      (incf *next-id*)
      task)))

(defun get-all-tasks ()
  "Get all tasks, newest first"
  (bt:with-lock-held (*tasks-lock*)
    (copy-list (reverse *tasks*))))

(defun find-task (id)
  "Find task by ID"
  (bt:with-lock-held (*tasks-lock*)
    (find id *tasks* :key #'task-id)))

(defun toggle-task-completion (id)
  "Toggle task completion status"
  (bt:with-lock-held (*tasks-lock*)
    (when-let ((task (find id *tasks* :key #'task-id)))
      (setf (task-completed task) (not (task-completed task)))
      task)))

(defun delete-task (id)
  "Delete a task"
  (bt:with-lock-held (*tasks-lock*)
    (setf *tasks* (remove id *tasks* :key #'task-id))))
```

### ui.lisp

```lisp
(in-package #:task-manager)

(defun render-task-list (container)
  "Render the list of tasks"
  (setf (inner-html container) "") ; Clear existing content
  
  (let ((tasks (get-all-tasks)))
    (if tasks
        (dolist (task tasks)
          (render-task container task))
        (create-p container :content "No tasks yet. Add one above!"))))

(defun render-task (container task)
  "Render a single task"
  (let* ((task-div (create-div container :class "task-item"))
         (checkbox (create-form-element task-div :checkbox))
         (title (create-span task-div :content (task-title task)))
         (delete-btn (create-button task-div :content "Delete")))
    
    ;; Set initial state
    (setf (checkbox-checked-p checkbox) (task-completed task))
    (when (task-completed task)
      (add-class title "completed"))
    
    ;; Toggle completion
    (set-on-change checkbox
      (lambda (obj)
        (declare (ignore obj))
        (toggle-task-completion (task-id task))
        (toggle-class title "completed")))
    
    ;; Delete task
    (set-on-click delete-btn
      (lambda (obj)
        (declare (ignore obj))
        (delete-task (task-id task))
        (render-task-list container)))))

(defun create-task-form (parent on-submit)
  "Create the task input form"
  (let* ((form (create-div parent :class "task-form"))
         (title-input (create-form-element form :input 
                                          :class "task-input"))
         (submit-btn (create-button form :content "Add Task")))
    
    (set-attribute title-input "placeholder" "What needs to be done?")
    
    ;; Handle submit
    (flet ((handle-submit ()
             (let ((title (string-trim '(#\Space #\Tab) 
                                      (value title-input))))
               (when (> (length title) 0)
                 (funcall on-submit title)
                 (setf (value title-input) "")
                 (focus title-input)))))
      
      (set-on-click submit-btn
        (lambda (obj)
          (declare (ignore obj))
          (handle-submit)))
      
      ;; Submit on Enter key
      (set-on-key-press title-input
        (lambda (obj data)
          (declare (ignore obj))
          (when (string= (getf data :key) "Enter")
            (handle-submit)))))))

(defun on-new-window (body)
  "Main UI entry point"
  (setf (title (html-document body)) "Task Manager")
  
  ;; Load CSS
  (load-css (html-document body) "/css/style.css")
  
  (let* ((container (create-div body :class "container"))
         (header (create-h1 container :content "Task Manager"))
         (task-list (create-div container :class "task-list")))
    
    (declare (ignore header))
    
    ;; Create form
    (create-task-form container
      (lambda (title)
        (add-task title "")
        (render-task-list task-list)))
    
    ;; Initial render
    (render-task-list task-list)))
```

### main.lisp

```lisp
(in-package #:task-manager)

(defvar *app* nil)

(defun start-app (&key (port 8080))
  "Start the task manager application"
  (setf *app* (initialize 'on-new-window
                         :static-root (merge-pathnames "static/"
                                                       (asdf:system-source-directory :task-manager))
                         :port port))
  (format t "Task Manager running on http://localhost:~A~%" port)
  (open-browser))

(defun stop-app ()
  "Stop the application"
  (when *app*
    (shutdown *app*)
    (setf *app* nil)))
```

### static/css/style.css

```css
body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    background-color: #f5f5f5;
    margin: 0;
    padding: 20px;
}

.container {
    max-width: 600px;
    margin: 0 auto;
    background: white;
    padding: 30px;
    border-radius: 8px;
    box-shadow: 0 2px 10px rgba(0,0,0,0.1);
}

h1 {
    color: #333;
    margin-top: 0;
}

.task-form {
    margin-bottom: 30px;
    display: flex;
    gap: 10px;
}

.task-input {
    flex: 1;
    padding: 10px;
    font-size: 16px;
    border: 1px solid #ddd;
    border-radius: 4px;
}

button {
    padding: 10px 20px;
    font-size: 16px;
    background: #007bff;
    color: white;
    border: none;
    border-radius: 4px;
    cursor: pointer;
}

button:hover {
    background: #0056b3;
}

.task-item {
    padding: 15px;
    border-bottom: 1px solid #eee;
    display: flex;
    align-items: center;
    gap: 10px;
}

.task-item:last-child {
    border-bottom: none;
}

.task-item span {
    flex: 1;
}

.task-item span.completed {
    text-decoration: line-through;
    color: #999;
}

.task-item button {
    background: #dc3545;
    padding: 5px 15px;
    font-size: 14px;
}

.task-item button:hover {
    background: #c82333;
}
```

### Running the App

```lisp
(ql:quickload :task-manager)
(task-manager:start-app)
```

**What you get:**
- Task input form
- Real-time task list
- Checkbox to mark complete
- Delete button
- No page refreshes!
- All state managed in Lisp

---

## PostgreSQL with Postmodern

Now let's add database persistence to make our apps production-ready.

### Installation

```lisp
(ql:quickload :postmodern)
```

### Connecting to PostgreSQL

```lisp
(defpackage #:my-app-db
  (:use #:cl #:postmodern))

(in-package #:my-app-db)

;; Connection parameters
(defvar *db-host* "localhost")
(defvar *db-name* "myapp")
(defvar *db-user* "myapp_user")
(defvar *db-password* "secure_password")

;; Connect to database
(defun connect-db ()
  (connect-toplevel *db-name* *db-user* *db-password* *db-host*))

;; Disconnect
(defun disconnect-db ()
  (disconnect-toplevel))

;; Use with-connection for transient connections
(defmacro with-db (&body body)
  `(with-connection (list *db-name* *db-user* *db-password* *db-host*)
     ,@body))
```

### Defining Tables

```lisp
(defclass user ()
  ((id :col-type serial :reader user-id)
   (email :col-type string :accessor user-email
          :col-unique t :col-not-null t)
   (password-hash :col-type string :accessor user-password-hash
                  :col-not-null t)
   (full-name :col-type string :accessor user-full-name)
   (created-at :col-type timestamp :reader user-created-at
               :col-default (:now)))
  (:metaclass dao-class)
  (:keys id))

(defclass task ()
  ((id :col-type serial :reader task-id)
   (user-id :col-type integer :accessor task-user-id
            :col-not-null t
            :col-references ((user id)))
   (title :col-type string :accessor task-title
          :col-not-null t)
   (description :col-type (or text db-null) :accessor task-description)
   (completed :col-type boolean :accessor task-completed
              :col-default nil)
   (created-at :col-type timestamp :reader task-created-at
               :col-default (:now)))
  (:metaclass dao-class)
  (:keys id))

;; Create tables
(defun create-tables ()
  (with-db
    (execute (dao-table-definition 'user))
    (execute (dao-table-definition 'task))))

;; Drop tables (careful!)
(defun drop-tables ()
  (with-db
    (execute "DROP TABLE IF EXISTS task CASCADE")
    (execute "DROP TABLE IF EXISTS user CASCADE")))
```

### CRUD Operations

```lisp
;; CREATE
(defun create-user (email password full-name)
  (let ((user (make-instance 'user
                            :email email
                            :password-hash (hash-password password)
                            :full-name full-name)))
    (insert-dao user)
    user))

;; READ - by ID
(defun find-user-by-id (id)
  (get-dao 'user id))

;; READ - by email
(defun find-user-by-email (email)
  (car (select-dao 'user (:= 'email email))))

;; READ - all users
(defun get-all-users ()
  (select-dao 'user))

;; UPDATE
(defun update-user-name (user new-name)
  (setf (user-full-name user) new-name)
  (update-dao user))

;; DELETE
(defun delete-user (user)
  (delete-dao user))

;; Or delete by ID:
(defun delete-user-by-id (id)
  (when-let ((user (find-user-by-id id)))
    (delete-dao user)))
```

### Queries with S-SQL

S-SQL is a Lisp DSL for generating SQL:

```lisp
;; Simple select
(query (:select 'email 'full-name :from 'user))
;; => (("alice@example.com" "Alice Smith") ("bob@example.com" "Bob Jones"))

;; With where clause
(query (:select '* :from 'task
        :where (:and (:= 'user-id 1)
                     (:= 'completed nil))))

;; Joins
(query (:select 'user.email 'task.title
        :from 'task
        :inner-join 'user :on (:= 'task.user-id 'user.id)
        :where (:= 'task.completed t)))

;; Count
(query (:select (:count '*) :from 'task
        :where (:= 'user-id 1))
       :single)
;; => 5

;; Order by
(query (:select '* :from 'task
        :order-by (:desc 'created-at)))

;; Limit
(query (:select '* :from 'task
        :limit 10))

;; Insert
(execute (:insert-into 'user :set
          'email "new@example.com"
          'password-hash "hashed"
          'full-name "New User"))

;; Update
(execute (:update 'task :set
          'completed t
          :where (:= 'id 5)))

;; Delete
(execute (:delete-from 'task
          :where (:= 'id 5)))
```

### Transactions

```lisp
(defun transfer-task (task-id from-user-id to-user-id)
  "Transfer a task from one user to another"
  (with-db
    (with-transaction ()
      ;; Verify task belongs to from-user
      (let ((task (get-dao 'task task-id)))
        (unless (= (task-user-id task) from-user-id)
          (error "Task does not belong to user"))
        
        ;; Transfer
        (setf (task-user-id task) to-user-id)
        (update-dao task)
        
        ;; Log the transfer (example)
        (execute (:insert-into 'transfer-log :set
                  'task-id task-id
                  'from-user from-user-id
                  'to-user to-user-id
                  'transferred-at (:now)))))))

;; If any error occurs, entire transaction rolls back!
```

### Prepared Statements

For frequently-executed queries:

```lisp
(defprepared get-user-tasks
  (:select '* :from 'task
   :where (:= 'user-id '$1)
   :order-by (:desc 'created-at))
  :rows)

;; Use it:
(get-user-tasks 42)
```

### Connection Pooling

For multi-threaded apps:

```lisp
(defvar *connection-pool* nil)

(defun init-connection-pool (&key (size 10))
  (setf *connection-pool*
        (make-instance 'connection-pool
                      :database *db-name*
                      :user *db-user*
                      :password *db-password*
                      :host *db-host*
                      :max-connections size)))

(defmacro with-pooled-connection (&body body)
  `(with-connection (get-connection *connection-pool*)
     (unwind-protect
         (progn ,@body)
       (release-connection *connection-pool* *database*))))
```

---

## Authentication and Authorization

Security is critical for business applications. Let's implement proper auth.

### Password Hashing with Ironclad

```lisp
(ql:quickload :ironclad)
(ql:quickload :cl-base64)

(defpackage #:my-app-auth
  (:use #:cl))

(in-package #:my-app-auth)

(defun hash-password (password)
  "Hash a password using bcrypt-style PBKDF2"
  (let* ((salt (ironclad:make-random-salt))
         (hash (ironclad:pbkdf2-hash-password
                (babel:string-to-octets password)
                :salt salt
                :digest :sha256
                :iterations 100000)))
    ;; Return salt + hash as base64
    (format nil "~A$~A"
            (cl-base64:usb8-array-to-base64-string salt)
            (cl-base64:usb8-array-to-base64-string hash))))

(defun verify-password (password hash-string)
  "Verify a password against a stored hash"
  (let* ((parts (str:split "$" hash-string))
         (salt (cl-base64:base64-string-to-usb8-array (first parts)))
         (stored-hash (cl-base64:base64-string-to-usb8-array (second parts)))
         (computed-hash (ironclad:pbkdf2-hash-password
                        (babel:string-to-octets password)
                        :salt salt
                        :digest :sha256
                        :iterations 100000)))
    (equalp stored-hash computed-hash)))
```

### Session Management

```lisp
(defpackage #:my-app-session
  (:use #:cl))

(in-package #:my-app-session)

(defstruct session
  id
  user-id
  created-at
  last-accessed
  data)

(defvar *sessions* (make-hash-table :test 'equal))
(defvar *session-lock* (bt:make-lock "session-lock"))
(defvar *session-timeout* (* 60 60 2)) ; 2 hours

(defun generate-session-id ()
  "Generate a cryptographically secure session ID"
  (let ((random-bytes (ironclad:make-random-salt 32)))
    (ironclad:byte-array-to-hex-string random-bytes)))

(defun create-session (user-id)
  "Create a new session for a user"
  (bt:with-lock-held (*session-lock*)
    (let* ((session-id (generate-session-id))
           (now (local-time:now))
           (session (make-session :id session-id
                                 :user-id user-id
                                 :created-at now
                                 :last-accessed now
                                 :data (make-hash-table :test 'equal))))
      (setf (gethash session-id *sessions*) session)
      session-id)))

(defun get-session (session-id)
  "Retrieve and refresh a session"
  (bt:with-lock-held (*session-lock*)
    (when-let ((session (gethash session-id *sessions*)))
      ;; Check if expired
      (let ((age (local-time:timestamp-difference 
                  (local-time:now)
                  (session-last-accessed session))))
        (if (> age *session-timeout*)
            (progn
              (remhash session-id *sessions*)
              nil)
            (progn
              (setf (session-last-accessed session) (local-time:now))
              session))))))

(defun destroy-session (session-id)
  "Destroy a session"
  (bt:with-lock-held (*session-lock*)
    (remhash session-id *sessions*)))

(defun get-session-data (session key)
  "Get data from session"
  (gethash key (session-data session)))

(defun set-session-data (session key value)
  "Set data in session"
  (setf (gethash key (session-data session)) value))

;; Cleanup task (run periodically)
(defun cleanup-expired-sessions ()
  "Remove expired sessions"
  (bt:with-lock-held (*session-lock*)
    (let ((now (local-time:now))
          (to-remove nil))
      (maphash (lambda (id session)
                 (let ((age (local-time:timestamp-difference
                            now
                            (session-last-accessed session))))
                   (when (> age *session-timeout*)
                     (push id to-remove))))
               *sessions*)
      (dolist (id to-remove)
        (remhash id *sessions*)))))
```

### Login System with CLOG

```lisp
(defpackage #:my-app-login
  (:use #:cl #:clog #:my-app-db #:my-app-auth #:my-app-session))

(in-package #:my-app-login)

(defun show-login-page (body)
  "Display login form"
  (let* ((container (create-div body :class "login-container"))
         (form (create-div container :class "login-form"))
         (title (create-h2 form :content "Login"))
         (email-input (create-form-element form :email 
                                          :class "form-input"))
         (password-input (create-form-element form :password
                                             :class "form-input"))
         (error-div (create-div form :class "error-message"))
         (submit-btn (create-button form :content "Login")))
    
    (declare (ignore title))
    (set-attribute email-input "placeholder" "Email")
    (set-attribute password-input "placeholder" "Password")
    
    (flet ((attempt-login ()
             (let ((email (value email-input))
                   (password (value password-input)))
               (with-db
                 (when-let ((user (find-user-by-email email)))
                   (if (verify-password password (user-password-hash user))
                       ;; Success!
                       (let ((session-id (create-session (user-id user))))
                         ;; Store session in connection
                         (set-connection-data body "session-id" session-id)
                         ;; Show main app
                         (show-main-app body user))
                       ;; Wrong password
                       (setf (text error-div) "Invalid email or password")))
                 (unless (find-user-by-email email)
                   (setf (text error-div) "Invalid email or password"))))))
      
      (set-on-click submit-btn
        (lambda (obj)
          (declare (ignore obj))
          (attempt-login)))
      
      (set-on-key-press password-input
        (lambda (obj data)
          (declare (ignore obj))
          (when (string= (getf data :key) "Enter")
            (attempt-login)))))))

(defun require-authentication (body handler)
  "Middleware to require authentication"
  (let ((session-id (connection-data body "session-id")))
    (if (and session-id (get-session session-id))
        (funcall handler body)
        (show-login-page body))))
```

### Role-Based Authorization

```lisp
;; Add to user class:
(defclass user ()
  ((id :col-type serial :reader user-id)
   (email :col-type string :accessor user-email
          :col-unique t :col-not-null t)
   (password-hash :col-type string :accessor user-password-hash
                  :col-not-null t)
   (full-name :col-type string :accessor user-full-name)
   (role :col-type string :accessor user-role
         :col-default "user") ; "user", "admin", "manager"
   (created-at :col-type timestamp :reader user-created-at
               :col-default (:now)))
  (:metaclass dao-class)
  (:keys id))

;; Permission checking
(defun user-has-role-p (user role)
  "Check if user has a specific role"
  (string= (user-role user) role))

(defun user-can-p (user action)
  "Check if user can perform an action"
  (case action
    (:view-tasks t) ; Everyone can view their tasks
    (:delete-any-task (member (user-role user) '("admin" "manager") :test #'string=))
    (:manage-users (string= (user-role user) "admin"))
    (otherwise nil)))

;; Use in CLOG:
(defun delete-task-button (body task)
  (when (user-can-p (get-current-user body) :delete-any-task)
    (let ((btn (create-button body :content "Delete")))
      (set-on-click btn
        (lambda (obj)
          (declare (ignore obj))
          (delete-task-by-id (task-id task))
          (refresh-task-list body))))))
```

---

## Security Best Practices

### Input Validation

**NEVER trust user input!**

```lisp
(defun validate-email (email)
  "Validate email format"
  (and (stringp email)
       (> (length email) 5)
       (< (length email) 255)
       (cl-ppcre:scan "^[^@]+@[^@]+\\.[^@]+$" email)))

(defun validate-password (password)
  "Validate password strength"
  (and (stringp password)
       (>= (length password) 8)
       (<= (length password) 128)
       (cl-ppcre:scan "[A-Z]" password)     ; Uppercase
       (cl-ppcre:scan "[a-z]" password)     ; Lowercase
       (cl-ppcre:scan "[0-9]" password)))   ; Digit

(defun sanitize-string (str &key (max-length 1000))
  "Sanitize user input string"
  (when (stringp str)
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) str)))
      (when (<= (length trimmed) max-length)
        ;; Remove control characters
        (remove-if (lambda (c) (< (char-code c) 32))
                   trimmed)))))

;; Use in handlers:
(defun create-user-handler (body email password name)
  (let ((email (sanitize-string email :max-length 255))
        (name (sanitize-string name :max-length 255)))
    (unless (validate-email email)
      (error "Invalid email format"))
    (unless (validate-password password)
      (error "Password too weak"))
    ;; Proceed with creation...
    ))
```

### SQL Injection Prevention

**Postmodern protects you automatically with parameterized queries:**

```lisp
;; SAFE - uses parameters
(query (:select '* :from 'user :where (:= 'email '$1)) 
       user-email)

;; SAFE - S-SQL is safe
(query (:select '* :from 'user :where (:= 'email email)))

;; DANGEROUS - never do this!
(query (format nil "SELECT * FROM user WHERE email = '~A'" email))
```

### XSS Prevention

CLOG automatically escapes HTML, but be careful with `inner-html`:

```lisp
;; SAFE - automatically escaped
(setf (text div) user-input)

;; DANGEROUS - raw HTML
(setf (inner-html div) user-input) ; Could inject <script>!

;; If you must use HTML, sanitize:
(defun sanitize-html (html)
  "Strip potentially dangerous HTML"
  (let ((allowed-tags '("b" "i" "u" "p" "br")))
    ;; Use a proper HTML sanitizer library or
    ;; just escape everything:
    (escape-string html)))

(defun escape-string (str)
  "Escape HTML special characters"
  (with-output-to-string (out)
    (loop for char across str do
      (case char
        (#\< (write-string "&lt;" out))
        (#\> (write-string "&gt;" out))
        (#\& (write-string "&amp;" out))
        (#\" (write-string "&quot;" out))
        (#\' (write-string "&#39;" out))
        (otherwise (write-char char out))))))
```

### CSRF Protection

```lisp
(defun generate-csrf-token ()
  "Generate CSRF token"
  (let ((random-bytes (ironclad:make-random-salt 32)))
    (ironclad:byte-array-to-hex-string random-bytes)))

(defun create-form-with-csrf (body session)
  "Create form with CSRF protection"
  (let* ((form (create-form body))
         (csrf-token (generate-csrf-token))
         (csrf-input (create-form-element form :hidden 
                                         :name "csrf_token")))
    
    ;; Store token in session
    (set-session-data session "csrf-token" csrf-token)
    (setf (value csrf-input) csrf-token)
    
    form))

(defun verify-csrf-token (session submitted-token)
  "Verify CSRF token"
  (let ((stored-token (get-session-data session "csrf-token")))
    (and stored-token
         submitted-token
         (string= stored-token submitted-token))))
```

### Rate Limiting

```lisp
(defvar *rate-limit-table* (make-hash-table :test 'equal))
(defvar *rate-limit-lock* (bt:make-lock))

(defstruct rate-limit-entry
  count
  window-start)

(defun check-rate-limit (identifier &key (max-requests 10) (window 60))
  "Check if rate limit exceeded. Returns T if allowed, NIL if exceeded."
  (bt:with-lock-held (*rate-limit-lock*)
    (let* ((now (get-universal-time))
           (entry (gethash identifier *rate-limit-table*))
           (window-start (if entry 
                            (rate-limit-entry-window-start entry)
                            now)))
      
      ;; Reset if window expired
      (when (> (- now window-start) window)
        (setf entry nil))
      
      (if (or (not entry) 
              (< (rate-limit-entry-count entry) max-requests))
          (progn
            (if entry
                (incf (rate-limit-entry-count entry))
                (setf (gethash identifier *rate-limit-table*)
                      (make-rate-limit-entry :count 1 
                                            :window-start now)))
            t)
          nil))))

;; Use in login:
(defun attempt-login (email password)
  (unless (check-rate-limit email :max-requests 5 :window 300)
    (error "Too many login attempts. Try again later."))
  ;; ... proceed with login
  )
```

### Audit Logging

```lisp
(defclass audit-log ()
  ((id :col-type serial :reader log-id)
   (user-id :col-type (or integer db-null) :accessor log-user-id)
   (action :col-type string :accessor log-action)
   (resource :col-type string :accessor log-resource)
   (details :col-type (or text db-null) :accessor log-details)
   (ip-address :col-type (or string db-null) :accessor log-ip-address)
   (timestamp :col-type timestamp :reader log-timestamp
              :col-default (:now)))
  (:metaclass dao-class)
  (:keys id))

(defun log-action (user-id action resource &key details ip-address)
  "Log a security-relevant action"
  (with-db
    (insert-dao (make-instance 'audit-log
                              :user-id user-id
                              :action action
                              :resource resource
                              :details details
                              :ip-address ip-address))))

;; Use it:
(defun delete-user-handler (current-user target-user-id)
  (unless (user-can-p current-user :manage-users)
    (log-action (user-id current-user) "UNAUTHORIZED_DELETE_ATTEMPT" 
                (format nil "user/~A" target-user-id))
    (error "Unauthorized"))
  
  (delete-user-by-id target-user-id)
  (log-action (user-id current-user) "DELETE_USER"
              (format nil "user/~A" target-user-id)))
```

---

## Building a Complete Business App

Let's build a complete employee time tracking system.

### Requirements

- Employees log in and track time entries
- Managers approve time entries
- Admins manage users
- Reports by employee, project, date range
- Audit trail

### Database Schema

```lisp
(defclass employee ()
  ((id :col-type serial :reader employee-id)
   (email :col-type string :accessor employee-email
          :col-unique t :col-not-null t)
   (password-hash :col-type string :accessor employee-password-hash)
   (full-name :col-type string :accessor employee-full-name)
   (role :col-type string :accessor employee-role
         :col-default "employee")
   (department :col-type (or string db-null) :accessor employee-department)
   (manager-id :col-type (or integer db-null) :accessor employee-manager-id
               :col-references ((employee id)))
   (active :col-type boolean :accessor employee-active
           :col-default t)
   (created-at :col-type timestamp :reader employee-created-at
               :col-default (:now)))
  (:metaclass dao-class)
  (:keys id))

(defclass project ()
  ((id :col-type serial :reader project-id)
   (name :col-type string :accessor project-name
         :col-not-null t)
   (code :col-type string :accessor project-code
         :col-unique t :col-not-null t)
   (active :col-type boolean :accessor project-active
           :col-default t)
   (created-at :col-type timestamp :reader project-created-at
               :col-default (:now)))
  (:metaclass dao-class)
  (:keys id))

(defclass time-entry ()
  ((id :col-type serial :reader entry-id)
   (employee-id :col-type integer :accessor entry-employee-id
                :col-not-null t
                :col-references ((employee id)))
   (project-id :col-type integer :accessor entry-project-id
               :col-not-null t
               :col-references ((project id)))
   (date :col-type date :accessor entry-date
         :col-not-null t)
   (hours :col-type numeric :accessor entry-hours
          :col-not-null t)
   (description :col-type text :accessor entry-description)
   (status :col-type string :accessor entry-status
           :col-default "pending") ; pending, approved, rejected
   (approved-by :col-type (or integer db-null) :accessor entry-approved-by
                :col-references ((employee id)))
   (approved-at :col-type (or timestamp db-null) :accessor entry-approved-at)
   (created-at :col-type timestamp :reader entry-created-at
               :col-default (:now)))
  (:metaclass dao-class)
  (:keys id))
```

### Main Application Structure

```lisp
(defpackage #:timetracker
  (:use #:cl #:clog #:postmodern))

(in-package #:timetracker)

;; Global state
(defvar *app* nil)
(defvar *db-connection-spec* 
  '("timetracker" "timetracker_user" "password" "localhost"))

;; Main entry point
(defun on-new-window (body)
  "Handle new browser connection"
  (with-connection *db-connection-spec*
    (let ((session-id (connection-data body "session-id")))
      (if (and session-id (get-session session-id))
          ;; Authenticated
          (let* ((session (get-session session-id))
                 (employee (get-dao 'employee (session-user-id session))))
            (show-dashboard body employee))
          ;; Not authenticated
          (show-login-page body)))))

;; Start the application
(defun start-app (&key (port 8080))
  (connect-toplevel (first *db-connection-spec*)
                   (second *db-connection-spec*)
                   (third *db-connection-spec*)
                   (fourth *db-connection-spec*))
  (setf *app* (initialize 'on-new-window
                         :static-root "./static/"
                         :port port))
  (format t "TimeTracker running on http://localhost:~A~%" port))
```

### Dashboard UI

```lisp
(defun show-dashboard (body employee)
  "Main dashboard view"
  (setf (title (html-document body)) "TimeTracker")
  (load-css (html-document body) "/css/milligram.min.css")
  (load-css (html-document body) "/css/custom.css")
  
  (let* ((container (create-div body :class "container"))
         (header (create-header container))
         (nav (create-nav container employee))
         (main-content (create-div container :class "main-content")))
    
    (show-welcome-message header employee)
    (render-navigation nav employee)
    
    ;; Default view: today's entries
    (show-time-entries main-content employee)))

(defun show-welcome-message (header employee)
  (let ((welcome (create-h1 header)))
    (setf (text welcome) 
          (format nil "Welcome, ~A" (employee-full-name employee)))))

(defun render-navigation (nav employee)
  (let ((ul (create-unordered-list nav :class "nav-menu")))
    
    ;; My Time
    (let ((li (create-list-item ul)))
      (let ((link (create-a li :content "My Time")))
        (set-on-click link
          (lambda (obj)
            (declare (ignore obj))
            (show-time-entries (get-parent (get-parent nav)) employee)))))
    
    ;; My Reports  
    (let ((li (create-list-item ul)))
      (let ((link (create-a li :content "Reports")))
        (set-on-click link
          (lambda (obj)
            (declare (ignore obj))
            (show-reports (get-parent (get-parent nav)) employee)))))
    
    ;; Manager views
    (when (member (employee-role employee) '("manager" "admin") :test #'string=)
      (let ((li (create-list-item ul)))
        (let ((link (create-a li :content "Approve Time")))
          (set-on-click link
            (lambda (obj)
              (declare (ignore obj))
              (show-approval-queue (get-parent (get-parent nav)) employee))))))
    
    ;; Admin views
    (when (string= (employee-role employee) "admin")
      (let ((li (create-list-item ul)))
        (let ((link (create-a li :content "Manage Users")))
          (set-on-click link
            (lambda (obj)
              (declare (ignore obj))
              (show-user-management (get-parent (get-parent nav)) employee))))))
    
    ;; Logout
    (let ((li (create-list-item ul)))
      (let ((link (create-a li :content "Logout")))
        (set-on-click link
          (lambda (obj)
            (let ((session-id (connection-data (connection-body obj) "session-id")))
              (destroy-session session-id)
              (set-connection-data (connection-body obj) "session-id" nil)
              (location (window (connection-body obj)) "/"))))))))
```

### Time Entry Form

```lisp
(defun show-time-entries (container employee)
  "Show time entry form and list"
  (setf (inner-html container) "")
  
  (let ((form-section (create-div container :class "time-entry-form"))
        (list-section (create-div container :class "time-entry-list")))
    
    ;; Entry form
    (create-h2 form-section :content "Log Time")
    (create-time-entry-form form-section employee
      (lambda ()
        (render-time-entry-list list-section employee)))
    
    ;; Entry list
    (create-h2 list-section :content "Recent Entries")
    (render-time-entry-list list-section employee)))

(defun create-time-entry-form (parent employee on-submit)
  (let* ((form (create-div parent))
         (date-input (create-form-element form :date :class "form-input"))
         (project-select (create-select form :class "form-input"))
         (hours-input (create-form-element form :number :class "form-input"))
         (desc-input (create-text-area form :class "form-input"))
         (submit-btn (create-button form :content "Log Time" :class "button-primary"))
         (error-div (create-div form :class "error-message")))
    
    ;; Setup form
    (set-attribute date-input "value" 
                  (local-time:format-timestring nil (local-time:today) 
                                               :format '(:year "-" (:month 2) "-" (:day 2))))
    (set-attribute hours-input "min" "0.25")
    (set-attribute hours-input "max" "24")
    (set-attribute hours-input "step" "0.25")
    (set-attribute desc-input "placeholder" "Description of work done")
    
    ;; Load projects
    (with-connection *db-connection-spec*
      (let ((projects (select-dao 'project (:= 'active t))))
        (dolist (project projects)
          (let ((option (create-option project-select 
                                      :content (project-name project)
                                      :value (write-to-string (project-id project)))))
            (declare (ignore option))))))
    
    ;; Handle submit
    (flet ((handle-submit ()
             (handler-case
                 (let ((date (value date-input))
                       (project-id (parse-integer (value project-select)))
                       (hours (parse-number:parse-number (value hours-input)))
                       (description (value desc-input)))
                   
                   ;; Validate
                   (unless (and (> hours 0) (<= hours 24))
                     (error "Hours must be between 0 and 24"))
                   
                   ;; Save
                   (with-connection *db-connection-spec*
                     (insert-dao (make-instance 'time-entry
                                               :employee-id (employee-id employee)
                                               :project-id project-id
                                               :date date
                                               :hours hours
                                               :description description
                                               :status "pending")))
                   
                   ;; Clear form
                   (setf (value hours-input) "")
                   (setf (value desc-input) "")
                   (setf (text error-div) "")
                   
                   ;; Refresh list
                   (funcall on-submit))
               (error (e)
                 (setf (text error-div) (format nil "Error: ~A" e))))))
      
      (set-on-click submit-btn
        (lambda (obj)
          (declare (ignore obj))
          (handle-submit))))))

(defun render-time-entry-list (container employee)
  "Render list of time entries"
  (setf (inner-html container) "")
  
  (with-connection *db-connection-spec*
    (let ((entries (query (:order-by
                          (:select 'time-entry.* 'project.name
                           :from 'time-entry
                           :inner-join 'project 
                           :on (:= 'time-entry.project-id 'project.id)
                           :where (:= 'time-entry.employee-id (employee-id employee)))
                          (:desc 'date))
                         :plists)))
      
      (if entries
          (let ((table (create-table container :class "time-entry-table")))
            ;; Header
            (let ((header-row (create-table-row table)))
              (create-table-heading header-row :content "Date")
              (create-table-heading header-row :content "Project")
              (create-table-heading header-row :content "Hours")
              (create-table-heading header-row :content "Description")
              (create-table-heading header-row :content "Status")
              (create-table-heading header-row :content "Actions"))
            
            ;; Entries
            (dolist (entry entries)
              (let ((row (create-table-row table)))
                (create-table-column row :content (getf entry :date))
                (create-table-column row :content (getf entry :name))
                (create-table-column row :content (format nil "~,2F" (getf entry :hours)))
                (create-table-column row :content (getf entry :description))
                (create-table-column row :content (getf entry :status))
                
                ;; Actions (only for pending entries)
                (let ((actions-col (create-table-column row)))
                  (when (string= (getf entry :status) "pending")
                    (let ((delete-btn (create-button actions-col :content "Delete")))
                      (set-on-click delete-btn
                        (lambda (obj)
                          (declare (ignore obj))
                          (with-connection *db-connection-spec*
                            (execute (:delete-from 'time-entry 
                                     :where (:= 'id (getf entry :id)))))
                          (render-time-entry-list container employee)))))))))
          
          (create-p container :content "No time entries yet.")))))
```

This is a solid foundation. The complete app would also include:
- Approval workflows for managers
- Reporting with date range filters
- User management for admins
- Export to CSV/PDF
- Email notifications

---

## CSS Frameworks: Milligram and PureCSS

For clean, minimal styling without bloat.

### Using Milligram

**Download and include:**
```bash
mkdir -p static/css
cd static/css
wget https://cdnjs.cloudflare.com/ajax/libs/milligram/1.4.1/milligram.min.css
```

**In your CLOG app:**
```lisp
(load-css (html-document body) "/css/milligram.min.css")
```

**Milligram classes:**
```lisp
;; Containers
(create-div body :class "container")
(create-div body :class "row")
(create-div body :class "column column-50") ; 50% width

;; Forms
(create-form-element form :input :class "form-input")
(create-button form :class "button-primary")
(create-button form :class "button-outline")

;; Tables
(create-table container :class "table")

;; Typography - automatic styling
(create-h1 container :content "Heading") ; Already styled!
```

### Using PureCSS

```bash
wget https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/pure-min.css
```

```lisp
(load-css (html-document body) "/css/pure-min.css")
```

**PureCSS classes:**
```lisp
;; Grids
(create-div body :class "pure-g")
(create-div container :class "pure-u-1-3") ; 1/3 width

;; Forms
(create-form body :class "pure-form pure-form-stacked")
(create-button form :class "pure-button pure-button-primary")

;; Tables
(create-table container :class "pure-table pure-table-striped")

;; Menus
(create-div body :class "pure-menu pure-menu-horizontal")
```

### Custom CSS

**static/css/custom.css:**
```css
/* Layout */
.container {
    max-width: 1200px;
    margin: 0 auto;
    padding: 20px;
}

/* Navigation */
.nav-menu {
    display: flex;
    list-style: none;
    gap: 20px;
    padding: 20px 0;
    border-bottom: 1px solid #e1e1e1;
}

.nav-menu a {
    text-decoration: none;
    color: #0066cc;
    cursor: pointer;
}

.nav-menu a:hover {
    text-decoration: underline;
}

/* Forms */
.form-input {
    width: 100%;
    margin-bottom: 15px;
}

.error-message {
    color: #d33;
    margin: 10px 0;
    padding: 10px;
    background: #fee;
    border-radius: 4px;
    display: none;
}

.error-message:not(:empty) {
    display: block;
}

/* Tables */
.time-entry-table {
    width: 100%;
    margin-top: 20px;
}

.time-entry-table td,
.time-entry-table th {
    text-align: left;
    padding: 10px;
}

/* Status badges */
.status-pending {
    color: #856404;
    background-color: #fff3cd;
    padding: 3px 8px;
    border-radius: 3px;
}

.status-approved {
    color: #155724;
    background-color: #d4edda;
    padding: 3px 8px;
    border-radius: 3px;
}

.status-rejected {
    color: #721c24;
    background-color: #f8d7da;
    padding: 3px 8px;
    border-radius: 3px;
}
```

---

## Form Validation and Data Integrity

### Client-Side Validation (CLOG)

```lisp
(defun create-validated-form (parent)
  (let* ((form (create-div parent))
         (email-input (create-form-element form :email))
         (age-input (create-form-element form :number))
         (submit-btn (create-button form :content "Submit"))
         (error-div (create-div form :class "error-message")))
    
    ;; HTML5 validation attributes
    (set-attribute email-input "required" "true")
    (set-attribute email-input "pattern" "^[^@]+@[^@]+\\.[^@]+$")
    
    (set-attribute age-input "min" "18")
    (set-attribute age-input "max" "120")
    (set-attribute age-input "required" "true")
    
    ;; Custom validation
    (set-on-click submit-btn
      (lambda (obj)
        (declare (ignore obj))
        (let ((email (value email-input))
              (age-str (value age-input)))
          
          ;; Validate email
          (unless (validate-email email)
            (setf (text error-div) "Invalid email address")
            (return-from lambda))
          
          ;; Validate age
          (let ((age (parse-integer age-str :junk-allowed t)))
            (unless (and age (>= age 18) (<= age 120))
              (setf (text error-div) "Age must be between 18 and 120")
              (return-from lambda)))
          
          ;; All valid
          (setf (text error-div) "")
          (handle-form-submission email age))))))
```

### Server-Side Validation (Always Required!)

```lisp
(defun validate-time-entry (employee-id project-id date hours description)
  "Validate time entry data"
  (let ((errors nil))
    
    ;; Employee exists
    (with-connection *db-connection-spec*
      (unless (get-dao 'employee employee-id)
        (push "Invalid employee" errors)))
    
    ;; Project exists and is active
    (with-connection *db-connection-spec*
      (let ((project (get-dao 'project project-id)))
        (unless project
          (push "Invalid project" errors))
        (when (and project (not (project-active project)))
          (push "Project is not active" errors))))
    
    ;; Date is valid and not in future
    (handler-case
        (let ((parsed-date (local-time:parse-timestring date)))
          (when (local-time:timestamp> parsed-date (local-time:now))
            (push "Date cannot be in the future" errors)))
      (error ()
        (push "Invalid date format" errors)))
    
    ;; Hours are reasonable
    (unless (and (numberp hours) (> hours 0) (<= hours 24))
      (push "Hours must be between 0 and 24" errors))
    
    ;; Description not empty
    (when (or (not description) 
              (zerop (length (string-trim '(#\Space) description))))
      (push "Description is required" errors))
    
    ;; Return errors or nil
    (when errors
      (format nil "Validation errors: ~{~A~^, ~}" (reverse errors)))))

;; Use it:
(defun create-time-entry-handler (employee-id project-id date hours description)
  (let ((validation-error (validate-time-entry employee-id project-id date hours description)))
    (when validation-error
      (error validation-error))
    
    ;; Proceed with creation
    (with-connection *db-connection-spec*
      (insert-dao (make-instance 'time-entry
                                :employee-id employee-id
                                :project-id project-id
                                :date date
                                :hours hours
                                :description description)))))
```

### Database Constraints

```lisp
;; In your DAO definitions, add constraints:
(defclass time-entry ()
  ((id :col-type serial :reader entry-id)
   (employee-id :col-type integer :accessor entry-employee-id
                :col-not-null t
                :col-references ((employee id)))
   (date :col-type date :accessor entry-date
         :col-not-null t)
   (hours :col-type numeric :accessor entry-hours
          :col-not-null t
          :col-check (:and (:> 'hours 0) (:<= 'hours 24)))
   (status :col-type string :accessor entry-status
           :col-default "pending"
           :col-check (:in 'status (:set "pending" "approved" "rejected"))))
  (:metaclass dao-class)
  (:keys id)
  (:unique-keys (employee-id date project-id))) ; No duplicate entries per day/project
```

---

## Alternative Web Frameworks

While CLOG is great for business apps, here are alternatives:

### Caveman2 - Rails-style Framework

```lisp
(ql:quickload :caveman2)

(defpackage my-app
  (:use :cl :caveman2))
(in-package :my-app)

;; Define app
(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))

;; Routes
(defroute "/" ()
  (render #P"index.html"))

(defroute "/tasks" ()
  (render #P"tasks.html" 
          '(:tasks (get-all-tasks))))

(defroute ("/tasks/:id" :method :GET) (&key id)
  (render #P"task.html" 
          `(:task ,(find-task (parse-integer id)))))

;; Start
(defun start ()
  (lack:builder
   :session
   *web*))
```

### Hunchentoot - Low-Level but Flexible

```lisp
(ql:quickload :hunchentoot)

(defpackage :my-app
  (:use :cl :hunchentoot))
(in-package :my-app)

;; Define handlers
(define-easy-handler (home :uri "/") ()
  (setf (content-type*) "text/html")
  "<h1>Welcome</h1>")

(define-easy-handler (tasks :uri "/tasks") ()
  (setf (content-type*) "application/json")
  (encode-json-to-string (get-all-tasks)))

;; Start server
(defvar *server* 
  (start (make-instance 'easy-acceptor :port 8080)))
```

### Clack - WSGI-style Abstraction

```lisp
(ql:quickload :clack)

(defun app (env)
  (declare (ignore env))
  '(200 (:content-type "text/plain") ("Hello, Clack!")))

;; Start
(clack:clackup #'app :port 8080)
```

---

## Testing Web Applications

### Testing Database Layer

```lisp
(def-suite db-tests)
(in-suite db-tests)

(def-fixture test-db ()
  ;; Setup: use test database
  (let ((*db-connection-spec* '("test_db" "test_user" "test_pass" "localhost")))
    (with-connection *db-connection-spec*
      ;; Create tables
      (execute (dao-table-definition 'employee))
      (execute (dao-table-definition 'time-entry))
      
      ;; Run tests
      (&body)
      
      ;; Teardown: drop tables
      (execute "DROP TABLE time_entry CASCADE")
      (execute "DROP TABLE employee CASCADE"))))

(test create-employee
  (with-fixture test-db ()
    (let ((employee (create-employee "test@example.com" "password" "Test User")))
      (is (not (null (employee-id employee))))
      (is (string= "test@example.com" (employee-email employee))))))
```

### Testing CLOG UIs

Testing CLOG is tricky since it requires a browser. Options:

**1. Test the logic separately:**
```lisp
;; Extract business logic
(defun calculate-weekly-hours (entries)
  (reduce #'+ entries :key #'entry-hours))

;; Test it
(test weekly-hours-calculation
  (let ((entries (list (make-instance 'time-entry :hours 8)
                      (make-instance 'time-entry :hours 7.5)
                      (make-instance 'time-entry :hours 8))))
    (is (= 23.5 (calculate-weekly-hours entries)))))
```

**2. Use headless browser (Selenium):**
```lisp
(ql:quickload :selenium)

(defun test-login-ui ()
  (selenium:with-session (session "http://localhost:8080")
    (selenium:go session "/")
    (selenium:send-keys session (selenium:find-element session :name "email") 
                       "test@example.com")
    (selenium:send-keys session (selenium:find-element session :name "password")
                       "password")
    (selenium:click session (selenium:find-element session :css "button[type=submit]"))
    (selenium:wait-for session 5
      (selenium:find-element session :css ".dashboard"))))
```

---

## Deployment Strategies

### Systemd Service (Linux)

**/etc/systemd/system/timetracker.service:**
```ini
[Unit]
Description=TimeTracker Application
After=network.target postgresql.service

[Service]
Type=simple
User=timetracker
WorkingDirectory=/opt/timetracker
Environment="SBCL_HOME=/usr/lib/sbcl"
ExecStart=/usr/bin/sbcl --load /opt/timetracker/start.lisp
Restart=always
RestartSec=10

[Install]
WantedBy=multi-user.target
```

**/opt/timetracker/start.lisp:**
```lisp
(load "~/.sbclrc") ; Load Quicklisp
(ql:quickload :timetracker)
(timetracker:start-app :port 8080)

;; Keep running
(loop (sleep 60))
```

**Commands:**
```bash
sudo systemctl enable timetracker
sudo systemctl start timetracker
sudo systemctl status timetracker
```

### Docker Deployment

**Dockerfile:**
```dockerfile
FROM clfoundation/sbcl:latest

WORKDIR /app

# Install Quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         --eval '(ql:add-to-init-file)' \
         --quit

# Copy application
COPY . /app/

# Load dependencies
RUN sbcl --eval '(ql:quickload :timetracker)' --quit

EXPOSE 8080

CMD ["sbcl", "--load", "start.lisp"]
```

**docker-compose.yml:**
```yaml
version: '3.8'

services:
  db:
    image: postgres:15
    environment:
      POSTGRES_DB: timetracker
      POSTGRES_USER: timetracker_user
      POSTGRES_PASSWORD: secure_password
    volumes:
      - postgres_data:/var/lib/postgresql/data
    ports:
      - "5432:5432"

  app:
    build: .
    ports:
      - "8080:8080"
    depends_on:
      - db
    environment:
      DB_HOST: db
      DB_NAME: timetracker
      DB_USER: timetracker_user
      DB_PASSWORD: secure_password

volumes:
  postgres_data:
```

### Reverse Proxy with Nginx

**/etc/nginx/sites-available/timetracker:**
```nginx
server {
    listen 80;
    server_name timetracker.company.com;

    location / {
        proxy_pass http://localhost:8080;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        
        # WebSocket support for CLOG
        proxy_read_timeout 86400;
    }
}
```

### HTTPS with Let's Encrypt

```bash
sudo apt install certbot python3-certbot-nginx
sudo certbot --nginx -d timetracker.company.com
```

---

## Monitoring and Logging

### Structured Logging

```lisp
(ql:quickload :log4cl)

(defpackage :my-app-logging
  (:use :cl :log4cl))

(in-package :my-app-logging)

;; Configure logging
(log:config :sane :daily "/var/log/timetracker/app.log")

;; Use it
(log:info "Application started")
(log:warn "Low disk space: ~A MB" (get-free-space))
(log:error "Database connection failed: ~A" error-msg)

;; With context
(defun process-request (user-id action)
  (log:info "Processing request" 
            :user-id user-id 
            :action action)
  (handler-case
      (perform-action action)
    (error (e)
      (log:error "Action failed" 
                 :user-id user-id 
                 :action action 
                 :error e))))
```

### Health Check Endpoint

```lisp
(defun health-check-handler (body)
  "Health check endpoint for monitoring"
  (with-connection *db-connection-spec*
    (handler-case
        (progn
          ;; Test database
          (query "SELECT 1")
          
          ;; Return healthy
          (setf (content-type body) "application/json")
          (setf (text body) "{\"status\":\"healthy\",\"database\":\"ok\"}"))
      (error (e)
        (setf (content-type body) "application/json")
        (setf (text body) 
              (format nil "{\"status\":\"unhealthy\",\"error\":\"~A\"}" e))))))
```

### Application Metrics

```lisp
(defvar *request-count* 0)
(defvar *error-count* 0)
(defvar *active-connections* 0)

(defun metrics-handler (body)
  "Prometheus-style metrics endpoint"
  (setf (content-type body) "text/plain")
  (setf (text body)
        (format nil "# HELP requests_total Total requests~%~
                     # TYPE requests_total counter~%~
                     requests_total ~A~%~
                     # HELP errors_total Total errors~%~
                     # TYPE errors_total counter~%~
                     errors_total ~A~%~
                     # HELP active_connections Active WebSocket connections~%~
                     # TYPE active_connections gauge~%~
                     active_connections ~A~%"
                *request-count*
                *error-count*
                *active-connections*)))
```

---

## Performance Optimization

### Database Connection Pooling

Already covered, but worth emphasizing:

```lisp
(defvar *db-pool* nil)

(defun init-db-pool ()
  (setf *db-pool*
        (postmodern:make-connection-pool 
         :database "timetracker"
         :user "user"
         :password "pass"
         :host "localhost"
         :max-connections 20
         :min-connections 5)))

;; Use pooled connections
(defmacro with-pooled-db (&body body)
  `(postmodern:with-connection-pool (*db-pool*)
     ,@body))
```

### Caching

```lisp
(ql:quickload :cl-cache)

(defvar *cache* (make-instance 'cl-cache:lru-cache :capacity 100))

(defun get-project-cached (project-id)
  "Get project with caching"
  (or (cl-cache:get-cached *cache* project-id)
      (let ((project (with-db (get-dao 'project project-id))))
        (cl-cache:cache-value *cache* project-id project)
        project)))

;; Invalidate on update
(defun update-project (project)
  (with-db
    (update-dao project)
    (cl-cache:remove-cached *cache* (project-id project))))
```

### Lazy Loading

```lisp
(defun render-large-list (container items)
  "Render list with lazy loading"
  (let ((page-size 50)
        (current-page 0))
    
    (labels ((render-page (page)
               (let ((start (* page page-size))
                     (end (min (* (1+ page) page-size) (length items))))
                 (loop for i from start below end
                       do (render-item container (nth i items)))))
             
             (load-more ()
               (incf current-page)
               (render-page current-page)
               (when (< (* (1+ current-page) page-size) (length items))
                 (setf (text load-more-btn) "Load More")
                 (setf (disabled load-more-btn) nil))))
      
      ;; Initial render
      (render-page 0)
      
      ;; Load more button
      (let ((load-more-btn (create-button container :content "Load More")))
        (set-on-click load-more-btn
          (lambda (obj)
            (declare (ignore obj))
            (setf (disabled load-more-btn) t)
            (setf (text load-more-btn) "Loading...")
            (load-more)))))))
```

### Compilation Settings

**For production:**
```lisp
;; In your .asd file or before compiling:
(declaim (optimize (speed 3) (safety 1) (debug 1)))

;; Save core image for faster startup
(sb-ext:save-lisp-and-die "timetracker-core" 
                         :toplevel #'timetracker:main
                         :executable t
                         :compression 9)
```

---

## Summary and Best Practices

### CLOG Development Checklist

- ✅ Use `with-connection` or connection pooling
- ✅ Validate all inputs (client AND server)
- ✅ Hash passwords, never store plaintext
- ✅ Implement proper session management
- ✅ Use CSRF tokens for forms
- ✅ Escape user content (use `text` not `inner-html`)
- ✅ Add audit logging for sensitive operations
- ✅ Implement rate limiting
- ✅ Use HTTPS in production
- ✅ Keep dependencies updated
- ✅ Test database operations in transactions
- ✅ Monitor errors and performance
- ✅ Regular database backups

### Code Organization Tips

```
project/
├── project.asd
├── package.lisp
├── config.lisp          # Configuration
├── db/
│   ├── schema.lisp      # DAO definitions
│   ├── queries.lisp     # Complex queries
│   └── migrations.lisp  # Schema changes
├── models/
│   ├── user.lisp
│   ├── project.lisp
│   └── time-entry.lisp
├── auth/
│   ├── password.lisp
│   └── session.lisp
├── ui/
│   ├── common.lisp      # Shared UI components
│   ├── dashboard.lisp
│   ├── time-entry.lisp
│   └── admin.lisp
├── api/
│   └── handlers.lisp    # Business logic
├── utils/
│   ├── validation.lisp
│   └── formatting.lisp
├── static/
│   ├── css/
│   └── images/
├── tests/
│   ├── db-tests.lisp
│   ├── model-tests.lisp
│   └── api-tests.lisp
└── main.lisp            # Entry point
```

### Common Pitfalls to Avoid

1. **Not using transactions** - Always wrap related DB operations
2. **Trusting user input** - Validate everything
3. **Forgetting thread safety** - Use locks for shared state
4. **No error handling** - Wrap risky operations in `handler-case`
5. **Ignoring resource cleanup** - Use `unwind-protect`
6. **Hardcoded configuration** - Use environment variables
7. **No logging** - You'll regret it when debugging production
8. **Skipping backups** - Automate PostgreSQL backups
9. **Weak passwords** - Enforce password policies
10. **No rate limiting** - Easy target for abuse

---

## What's Next?

You now have the foundation to build production-grade business applications with Common Lisp. 

**Recommended next steps:**
1. Build a small CRUD app with CLOG + PostgreSQL
2. Add authentication and basic authorization
3. Deploy it locally with Docker
4. Add tests for critical paths
5. Monitor and iterate

**Advanced topics to explore:**
- Real-time features with WebSockets
- File uploads and processing
- PDF generation with cl-pdf
- Excel export with cl-xlsx
- Email with cl-smtp
- Background jobs with SBCL threads
- API integrations
- Advanced SQL with window functions
- Database replication and failover
- Multi-tenancy

**Resources:**
- CLOG Documentation: https://github.com/rabbibotton/clog
- Postmodern Manual: https://marijnhaverbeke.nl/postmodern/
- Common Lisp Cookbook: https://lispcookbook.github.io/cl-cookbook/

---

**You're ready to build real applications!**

The combination of CLOG's simplicity, Postmodern's power, and Common Lisp's interactive development makes for an incredibly productive stack for internal business applications. Focus on solving business problems, not fighting frameworks.

Happy building! 🚀

---

*Last updated: February 2026*
