# How To Get Common Lisp (SBCL) and CLOG working on Windows

## Prerequisites:
- Download SQLite (Precompiled Binaries, as you only need the sqlite3.dll)
- Download SBCL (Newest Version)
- Download VSCode or Emacs (for VScode we use ALIVE, for Emacs we get our own config-files)
- Download GIT (https://git-scm.com/download/win) 
  - **Notes from dbotton:** Even if you don't use GIT, it installs the needed ssl files and some basic unix tools like bash. 
  You should choose for line endings, checkout as-is, commit unix-style line endings sbcl does not handle crlf well in certain situations

## Tasks To Do In The Git Bash Terminal

`cd
curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp`
<br> 
`sbcl --load /tmp/ql.lisp`
- Use in REPL: 
`(quicklisp-quickstart:install)
(ql:add-to-init-file)`
- Back in Git Shell: <br>
    `sbcl --eval '(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)' --eval '(ql:update-all-dists)' --quit`

`sbcl --eval '(ql:quickload :clog/tools)' `

## Starten vom CLOG Builder im REPL

`(clog-tools:clog-builder)`

## Default Locations

- **SBCL**: C:\Program Files\Steel Bank Common Lisp
- **Quicklisp**: C:\Users\<user>\quicklisp

## Setup Texteditor zur Entwicklung in Common Lisp

### VSCode mit ALIVE

#### Notwendige Packages:

`sbcl --eval '(ql:quickload "bordeaux-threads")' --eval '(ql:quickload "usocket")'
     --eval '(ql:quickload "cl-json")' --eval '(ql:quickload "flexi-streams")'`

Danach Extension aus dem Marketplace installieren. ALIVE startet sich sobald eine .Lisp-Datei geoeffnet wird

### Emacs mit custom Config
- Lade Emacs > 28 runter
- **Windows** Bei der Installation *C:\Users\<user>* angeben fuer den "Home Folder" 
- Dann Configfiles hinterlegen in *~/.emacs.d/*: https://github.com/flameesy/config 

#### Alternativ ohne Configfiles hier weitermachen (auch essentiell fuer Custom Config!):

- Emacs starten
- (dann C-x-f verwenden und die ~/.emacs.d/early-init.el erstellen)<br>
- danach slime-helper installieren: ` (ql:quickload "quicklisp-slime-helper")` 
- und in die early-init.el diesen Code hinterlegen:
  - `(load (expand-file-name "~/quicklisp/slime-helper.el"))
     (setq inferior-lisp-program "sbcl")`


Danach `M-x package-install  <RET> sly` und alles ist erledigt um erfolgreich zu entwickeln! 

