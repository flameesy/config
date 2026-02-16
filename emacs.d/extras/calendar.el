;;;; calendar.el --- Calendar configuration -*- lexical-binding: t; -*-
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Package setup
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install calfw
(unless (package-installed-p 'calfw)
  (package-install 'calfw))

(unless (package-installed-p 'calfw-ical)
  (package-install 'calfw-ical))
  
(unless (package-installed-p 'calfw-org)
  (package-install 'calfw-org))

;; Load the packages
(require 'calfw)
(require 'calfw-ical)
(require 'calfw-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; German holiday definitions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq holiday-general-holidays nil)
(setq holiday-christian-holidays nil)
(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)
(setq holiday-bahai-holidays nil)
(setq holiday-oriental-holidays nil)

;; Define German holidays
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
        (holiday-fixed 8 8 "Friedensfest (Augsburg)")
        (holiday-fixed 8 15 "Mariä Himmelfahrt (BY, SL)")
        (holiday-fixed 9 20 "Weltkindertag (TH)")
        (holiday-fixed 10 3 "Tag der Deutschen Einheit")
        (holiday-fixed 10 31 "Reformationstag (BB, HB, HH, MV, NI, SN, ST, SH, TH)")
        (holiday-fixed 11 1 "Allerheiligen (BW, BY, NW, RP, SL)")
        (holiday-float 11 3 -1 "Buß- und Bettag (SN)" 23)
        (holiday-fixed 12 25 "1. Weihnachtsfeiertag")
        (holiday-fixed 12 26 "2. Weihnachtsfeiertag")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Calendar settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq calendar-week-start-day 1)  ; Monday
(setq calendar-day-name-array
      ["Sonntag" "Montag" "Dienstag" "Mittwoch" 
       "Donnerstag" "Freitag" "Samstag"])
(setq calendar-month-name-array
      ["Januar" "Februar" "März" "April" "Mai" "Juni"
       "Juli" "August" "September" "Oktober" "November" "Dezember"])

;;;Local calendar file path
(defvar my-local-calendar-file 
  (expand-file-name "calendar/simon@knogler.dev.ics" user-emacs-directory)
  "Path to local ICS calendar file for syncing.")
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Calendar setup
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ensure calendar directory exists
(unless (file-exists-p (file-name-directory my-local-calendar-file))
  (make-directory (file-name-directory my-local-calendar-file) t))

;; Create empty calendar file if it doesn't exist
(unless (file-exists-p my-local-calendar-file)
  (with-temp-file my-local-calendar-file
    (insert "BEGIN:VCALENDAR\n")
    (insert "VERSION:2.0\n")
    (insert "PRODID:-//My Calendar//EN\n")
    (insert "END:VCALENDAR\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions - CORRECT NEW NAMING CONVENTION (calfw- prefix)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-calendar-open ()
  "Open calendar view with local calendar."
  (interactive)
  (calfw-open-calendar-buffer
   :contents-sources
   (list
    (calfw-ical-create-source "Lokal" my-local-calendar-file "Blue"))
   :view 'month))

(defun my-calendar-with-org ()
  "Open calendar with org-mode."
  (interactive)
  (calfw-org-open-calendar))

(defun my-calendar-both ()
  "Open calendar with both ICS and org sources."
  (interactive)
  (calfw-open-calendar-buffer
   :contents-sources
   (list
    (calfw-org-create-source "Green")
    (calfw-ical-create-source "Lokal" my-local-calendar-file "Blue"))
   :view 'month))

(defun my-calendar-sync ()
  "Sync local calendar file with external service."
  (interactive)
  (message "Syncing calendar: %s" my-local-calendar-file)
  (message "Calendar sync complete"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Keybindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c c") 'my-calendar-open)
(global-set-key (kbd "C-c o") 'my-calendar-with-org)
(global-set-key (kbd "C-c b") 'my-calendar-both)
(global-set-key (kbd "C-c C-s") 'my-calendar-sync)

(provide 'calendar)