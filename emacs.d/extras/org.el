;;; org.el --- Org mode setup  -*- lexical-binding: t; -*-

;;; --- Verzeichnisse (früh, keine Org-Abhängigkeit) ---

(setq org-directory      (expand-file-name "~/Documents/org/"))
(setq org-roam-directory (expand-file-name "~/Documents/org-roam/"))
(setq org-roam-index-file
      (expand-file-name "index.org" org-roam-directory))

;;; --- Hilfsfunktionen (kein Org-Load nötig) ---

(defun knoglerdev/org-work-monthly-file ()
  "Return absolute path of the current monthly work log file."
  (expand-file-name (format-time-string "work-%Y-%m.org") org-directory))

(defun knoglerdev/org--maybe-ticket-line (s)
  "Return an indented ticket line for timelog entries, or empty string."
  (let ((tix (string-trim (or s ""))))
    (if (string-empty-p tix)
        ""
      (concat "  Ticket: " tix "\n"))))

;;; --- Org ---

(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . (lambda ()
                       ;; flyspell nur starten wenn ispell/aspell verfügbar ist
                       (when (executable-find "aspell")
                         (flyspell-mode 1)))))
  :bind (:map global-map
			  ("C-c c" . org-capture)
			  ("C-c a" . org-agenda)
			  ("C-c s" . org-store-link))
  :config

  ;; --- Optionaler CSL-Export ---
  (require 'oc-csl nil t)
  (add-to-list 'org-export-backends 'md)
  (setq org-export-with-smart-quotes t)

  ;; --- Link-Setup (mit Guard gegen nil) ---
  (when (assoc 'file org-link-frame-setup)
    (setf (cdr (assoc 'file org-link-frame-setup)) #'find-file))

  ;; --- TODO-Keywords ---
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|"
                    "DONE(d!)" "OBSOLETE(o@)")))
  (setq org-log-into-drawer t)

  ;; --- Tags ---
  (setq org-tag-alist
        '((:startgroup)
          ("home" . ?h) ("work" . ?w) ("school" . ?s)
          (:endgroup)
          (:newline)
          (:startgroup)
          ("one-shot" . ?o) ("project" . ?j) ("tiny" . ?t)
          (:endgroup)
          ("meta") ("review") ("reading")))

  ;; --- Agenda-Files (hier, nach Org-Load) ---
  (setq org-agenda-files
        (append
         (mapcar (lambda (f) (expand-file-name f org-directory))
                 '("inbox.org" "work.org"))
         (when (file-directory-p org-directory)
           (directory-files org-directory t
                            "^work-[0-9]\\{4\\}-[0-9]\\{2\\}\\.org$"))))

  ;; --- Refile ---
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; --- Capture Templates ---
  ;; Hinweis: `knoglerdev/org-work-monthly-file` wird hier als Funktion
  ;; übergeben (nicht vorab ausgewertet), damit sie zur Capture-Zeit
  ;; den korrekten Monat liefert.
  (setq org-capture-templates
        `(("c" "Default Capture" entry
           (file ,(expand-file-name "inbox.org" org-directory))
           "* TODO %?\n%U\n%i")

          ("r" "Capture with Reference" entry
           (file ,(expand-file-name "inbox.org" org-directory))
           "* TODO %?\n%U\n%i\n%a")

          ("tl" "Time log (monthly, multiline)" item
           (file+olp+datetree knoglerdev/org-work-monthly-file) ; Funktion, kein Aufruf!
           "- %^{From}-%^{To}: %^{Context}\n  %?\n  Ticket: %^{Ticket (optional)}\n"
           :empty-lines 1)

          ("w" "Work")

          ("wm" "Work meeting" entry
           (file+headline ,(expand-file-name "work.org" org-directory) "Meetings")
           "** TODO %?\n%U\n%i\n%a")

          ("wr" "Work report" entry
           (file+headline ,(expand-file-name "work.org" org-directory) "Reports")
           "** TODO %?\n%U\n%i\n%a")))

  ;; --- Agenda Custom Commands ---
  (setq org-agenda-custom-commands
        '(("n" "Agenda and All Todos"
           ((agenda) (todo)))
          ("w" "Work"
           ((agenda "" ((org-agenda-files
                         (list (expand-file-name "work.org" org-directory))))))))))

;;; --- Org Roam ---

(use-package org-roam
  :after org
  :config
  ;; org-roam-directory wurde bereits oben gesetzt, hier zur Sicherheit wiederholt
  (setq org-roam-directory (expand-file-name "~/Documents/org-roam/"))
  ;; Autosync nur aktivieren wenn das Verzeichnis existiert
  (when (file-directory-p org-roam-directory)
    (org-roam-db-autosync-mode 1)))

;;; org.el ends here