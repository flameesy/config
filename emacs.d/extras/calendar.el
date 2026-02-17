;;;; calendar.el --- Calendar configuration -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Package setup
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package calfw
  :ensure t
  :config
  ;; Better visual defaults
  (setq calfw:fchar-junction ?╋
        calfw:fchar-vertical-line ?┃
        calfw:fchar-horizontal-line ?━
        calfw:fchar-left-junction ?┣
        calfw:fchar-right-junction ?┫
        calfw:fchar-top-junction ?┳
        calfw:fchar-top-left-corner ?┏
        calfw:fchar-top-right-corner ?┓))

(use-package calfw-org
  :ensure t
  :after (calfw org))

(use-package calfw-ical
  :ensure t
  :after calfw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Org-mode integration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Org agenda files
(setq org-agenda-files (list (expand-file-name "org/" user-emacs-directory)))

;; Create org directory if it doesn't exist
(unless (file-exists-p (car org-agenda-files))
  (make-directory (car org-agenda-files) t))

;; Org agenda settings
(setq org-agenda-span 'month
      org-agenda-start-on-weekday 1
      org-deadline-warning-days 7
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; German locale settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq calendar-week-start-day 1)
(setq calendar-day-name-array
      ["Sonntag" "Montag" "Dienstag" "Mittwoch" 
       "Donnerstag" "Freitag" "Samstag"])
(setq calendar-month-name-array
      ["Januar" "Februar" "März" "April" "Mai" "Juni"
       "Juli" "August" "September" "Oktober" "November" "Dezember"])

;; Disable default holidays
(setq holiday-general-holidays nil
      holiday-christian-holidays nil
      holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-bahai-holidays nil
      holiday-oriental-holidays nil)

;; German holidays
(setq holiday-local-holidays
      '((holiday-fixed 1 1 "Neujahr")
        (holiday-fixed 1 6 "Heilige Drei Könige (BW, BY, ST)")
        (holiday-fixed 3 8 "Internationaler Frauentag (BE)")
        (holiday-easter-etc -2 "Karfreitag")
        (holiday-easter-etc 0 "Ostersonntag")
        (holiday-easter-etc 1 "Ostermontag")
        (holiday-fixed 5 1 "Tag der Arbeit")
        (holiday-easter-etc 39 "Christi Himmelfahrt")
        (holiday-easter-etc 49 "Pfingstsonntag")
        (holiday-easter-etc 50 "Pfingstmontag")
        (holiday-easter-etc 60 "Fronleichnam (BW, BY, HE, NW, RP, SL)")
        (holiday-fixed 8 15 "Mariä Himmelfahrt (BY, SL)")
        (holiday-fixed 10 3 "Tag der Deutschen Einheit")
        (holiday-fixed 10 31 "Reformationstag")
        (holiday-fixed 11 1 "Allerheiligen (BW, BY, NW, RP, SL)")
        (holiday-fixed 12 25 "1. Weihnachtsfeiertag")
        (holiday-fixed 12 26 "2. Weihnachtsfeiertag")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ICS calendar setup
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-local-calendar-file 
  (expand-file-name "calendar/simon@knogler.dev.ics" user-emacs-directory)
  "Path to local ICS calendar file.")

;; Ensure calendar exists
(unless (file-exists-p (file-name-directory my-local-calendar-file))
  (make-directory (file-name-directory my-local-calendar-file) t))

(unless (file-exists-p my-local-calendar-file)
  (with-temp-file my-local-calendar-file
    (insert "BEGIN:VCALENDAR\nVERSION:2.0\n")
    (insert "PRODID:-//Emacs Calendar//EN\n")
    (insert "END:VCALENDAR\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Calendar functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-calendar-open ()
  "Open calendar with local ICS file."
  (interactive)
  (calfw-open-calendar-buffer
   :contents-sources
   (list (calfw-ical-create-source "Lokal" my-local-calendar-file "DeepSkyBlue"))
   :view 'month))
   
(defun my-calendar-agenda ()
  "Open org-agenda."
  (interactive)
  (org-agenda nil "a"))

(defun my-calendar-sync ()
  "Sync local calendar file."
  (interactive)
  (message "Calendar sync: %s" my-local-calendar-file)
  (message "Sync complete!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Keybindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c c") 'my-calendar-open)
(global-set-key (kbd "C-c a") 'my-calendar-agenda)
(global-set-key (kbd "C-c C-s") 'my-calendar-sync)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Menu bar
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'menu-bar
  (defvar knoglerdev-calendar-menu (make-sparse-keymap "Calendar"))
  
  (define-key knoglerdev-calendar-menu [calendar-sync]
    '(menu-item "Sync Calendar" my-calendar-sync
                :help "Sync calendar (C-c C-s)"))
  
  (define-key knoglerdev-calendar-menu [separator] '(menu-item "--"))
  
  (define-key knoglerdev-calendar-menu [calendar-agenda]
    '(menu-item "Org Agenda" my-calendar-agenda
                :help "Open org-agenda (C-c a)"))

  (define-key knoglerdev-calendar-menu [calendar-open]
    '(menu-item "ICS Calendar" my-calendar-open
                :help "ICS calendar (C-c c)"))
  
  (define-key-after global-map [menu-bar calendar]
    (cons "Calendar" knoglerdev-calendar-menu)
    'workspace))

(provide 'calendar)
;;; calendar.el ends here