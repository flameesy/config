;;; org.el --- Org mode setup  -*- lexical-binding: t; -*-
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Core locations
(setq org-directory (expand-file-name "~/Documents/org/"))
(setq org-roam-directory (expand-file-name "~/Documents/org-roam/"))
(setq org-roam-index-file (expand-file-name "index.org" org-roam-directory))

;; Helper: monthly work log file path
(defun knoglerdev/org-work-monthly-file ()
  "Return absolute path of the current monthly work log file."
  (expand-file-name (format-time-string "work-%Y-%m.org") org-directory))

;; Include inbox/work hub + all monthly work logs in agenda/search
(setq org-agenda-files
      (append
       (mapcar (lambda (f) (expand-file-name f org-directory))
               '("inbox.org" "work.org"))
       (directory-files org-directory t "^work-[0-9]\\{4\\}-[0-9]\\{2\\}\\.org$")))

;; Default tags
(setq org-tag-alist
      '((:startgroup)
        ("home" . ?h)
        ("work" . ?w)
        ("school" . ?s)
        (:endgroup)
        (:newline)
        (:startgroup)
        ("one-shot" . ?o)
        ("project"  . ?j)
        ("tiny"     . ?t)
        (:endgroup)
        ("meta")
        ("review")
        ("reading")))

;; Refile targets: current file + headings up to level 3 in agenda files
(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Org mode setup
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun knoglerdev/org--maybe-ticket-line (s)
  "Return an indented ticket line for timelog entries, or empty string."
  (let ((tix (string-trim (or s ""))))
    (if (string-empty-p tix)
        ""
      (concat "  Ticket: " tix "\n"))))

(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . flyspell-mode))
  :bind (:map global-map
              ("C-c c" . org-capture)
              ("C-c a" . org-agenda)
              ("C-c l" . org-store-link))
  :config
  ;; Citation support (built-in in newer org; harmless if available)
  (require 'oc-csl nil t)

  ;; Export tweaks
  (add-to-list 'org-export-backends 'md)
  (setq org-export-with-smart-quotes t)

  ;; Open file links in same window
  (setf (cdr (assoc 'file org-link-frame-setup)) #'find-file)

  ;; TODO workflow
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|"
                    "DONE(d!)" "OBSOLETE(o@)")))

  ;; Keep state-change logs tidy
  (setq org-log-into-drawer t)

  ;; Refile UX
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; Capture templates
  (setq org-capture-templates
        `(
          ;; Inbox
          ("c" "Default Capture" entry
           (file ,(expand-file-name "inbox.org" org-directory))
           "* TODO %?\n%U\n%i")

          ("r" "Capture with Reference" entry
           (file ,(expand-file-name "inbox.org" org-directory))
           "* TODO %?\n%U\n%i\n%a")

          ;; Monthly timelog (your Notepad++-style, but organized)
          ("tl" "Time log (monthly, multiline)" item
           (file+olp+datetree ,(lambda () (knoglerdev/org-work-monthly-file)))
           "- %^{From}-%^{To}: %^{Context}\n  %?\n%(knoglerdev/org--maybe-ticket-line \"%^{Ticket (optional, Jira/GDI/blank)}\")"
           :empty-lines 1)

          ;; Structured work captures (keep using work.org as hub)
          ("w" "Work")
          ("wm" "Work meeting" entry
           (file+headline ,(expand-file-name "work.org" org-directory) "Meetings")
           "** TODO %?\n%U\n%i\n%a")

          ("wr" "Work report" entry
           (file+headline ,(expand-file-name "work.org" org-directory) "Reports")
           "** TODO %?\n%U\n%i\n%a")))

  ;; Custom agenda commands
  (setq org-agenda-custom-commands
        '(("n" "Agenda and All Todos"
           ((agenda)
            (todo)))
          ("w" "Work"
           ((agenda "" ((org-agenda-files (list (expand-file-name "work.org" org-directory))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Org-roam
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam
  :after org
  :custom
  (org-roam-directory org-roam-directory)
  (org-roam-index-file org-roam-index-file)
  :config
  (org-roam-db-autosync-mode 1))
