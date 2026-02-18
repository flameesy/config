;;; org.el --- Org mode setup  -*- lexical-binding: t; -*-

;; Core locations
(setq org-directory (expand-file-name "~/Documents/org/"))
(setq org-roam-directory (expand-file-name "~/Documents/org-roam/"))
(setq org-roam-index-file (expand-file-name "index.org" org-roam-directory))

(defun knoglerdev/org-work-monthly-file ()
  "Return absolute path of the current monthly work log file."
  (expand-file-name (format-time-string "work-%Y-%m.org") org-directory))

(defun knoglerdev/org--maybe-ticket-line (s)
  "Return an indented ticket line for timelog entries, or empty string."
  (let ((tix (string-trim (or s ""))))
    (if (string-empty-p tix)
        ""
      (concat "  Ticket: " tix "\n"))))

(setq org-agenda-files
      (append
       (mapcar (lambda (f) (expand-file-name f org-directory))
               '("inbox.org" "work.org"))
       (when (file-directory-p org-directory)  ; <-- guard
         (directory-files org-directory t "^work-[0-9]\\{4\\}-[0-9]\\{2\\}\\.org$"))))

(setq org-tag-alist
      '((:startgroup)
        ("home" . ?h) ("work" . ?w) ("school" . ?s)
        (:endgroup)
        (:newline)
        (:startgroup)
        ("one-shot" . ?o) ("project" . ?j) ("tiny" . ?t)
        (:endgroup)
        ("meta") ("review") ("reading")))

(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . flyspell-mode))
  :bind (:map global-map
              ("C-c c" . org-capture)
              ("C-c a" . org-agenda)
              ("C-c l" . org-store-link))
  :config
  (require 'oc-csl nil t)
  (add-to-list 'org-export-backends 'md)
  (setq org-export-with-smart-quotes t)
  (setf (cdr (assoc 'file org-link-frame-setup)) #'find-file)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|"
                    "DONE(d!)" "OBSOLETE(o@)")))

  (setq org-log-into-drawer t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-capture-templates
        `(("c" "Default Capture" entry
           (file ,(expand-file-name "inbox.org" org-directory))
           "* TODO %?\n%U\n%i")

          ("r" "Capture with Reference" entry
           (file ,(expand-file-name "inbox.org" org-directory))
           "* TODO %?\n%U\n%i\n%a")

          ("tl" "Time log (monthly, multiline)" item
           (file+olp+datetree ,(knoglerdev/org-work-monthly-file)) ; pre-evaluated
           "- %^{From}-%^{To}: %^{Context}\n  %?\n  Ticket: %^{Ticket (optional)}\n"
           :empty-lines 1)

          ("w" "Work")
          ("wm" "Work meeting" entry
           (file+headline ,(expand-file-name "work.org" org-directory) "Meetings")
           "** TODO %?\n%U\n%i\n%a")

          ("wr" "Work report" entry
           (file+headline ,(expand-file-name "work.org" org-directory) "Reports")
           "** TODO %?\n%U\n%i\n%a")))

  (setq org-agenda-custom-commands
        '(("n" "Agenda and All Todos"
           ((agenda) (todo)))
          ("w" "Work"
           ((agenda "" ((org-agenda-files
                         (list (expand-file-name "work.org" org-directory))))))))))

(use-package org-roam
  :after org
  :config
  (setq org-roam-directory (expand-file-name "~/Documents/org-roam/"))
  ;(org-roam-db-autosync-mode 1)
  )