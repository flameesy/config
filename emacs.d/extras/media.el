;;; media.el --- Media setup -*- lexical-binding: t; -*-
;;;
;;; Prerequisites:
;;;   - mpv or vlc (recommended: mpv)
;;;
;;; Quick start:
;;;   M-x emms-add-directory-tree RET ~/Music RET
;;;   M-x emms-shuffle
;;;   M-x emms-start

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   EMMS: Emacs Multimedia System
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emms
  :ensure t
  :config
  ;; Basic setup
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  
  ;; Set default music directory (can be changed on the fly)
  (setq emms-source-file-default-directory "~/Music/")
  
  ;; Player backend (mpv recommended, falls back to vlc/mplayer)
  (setq emms-player-list '(emms-player-mpv
                           emms-player-vlc
                           emms-player-mplayer))
  
  ;; Show track info in mode line
  (emms-mode-line 1)
  (emms-playing-time 1)
  
  ;; Show album art and track info (optional but nice)
  (setq emms-show-format "♪ %s")
  
  ;; Cache for faster loading
  (setq emms-cache-file (expand-file-name "emms-cache" user-emacs-directory))
  (emms-cache 1)
  
  ;; Playlist settings
  (setq emms-playlist-buffer-name "*Music*")
  (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
  
  ;; Tag editing support (for mp3, ogg, flac)
  (require 'emms-info-libtag)
  (setq emms-info-functions '(emms-info-libtag))
  
  ;; Browser for navigating music by artist/album
  (require 'emms-browser)
  (setq emms-browser-covers 'emms-browser-cache-thumbnail-async)
  
  ;; Volume control
  (setq emms-volume-change-function 'emms-volume-pulse-change)  ; Linux/Pulse
  ;; For Windows: (setq emms-volume-change-function 'emms-volume-mpv-change)
  
  ;; Keybindings
  :bind
  (("C-c e p" . emms-pause)
   ("C-c e s" . emms-stop)
   ("C-c e n" . emms-next)
   ("C-c e b" . emms-previous)
   ("C-c e f" . emms-show)
   ("C-c e l" . emms-playlist-mode-go)
   ("C-c e e" . emms-play-directory-tree)
   ("C-c e a" . emms-add-directory-tree)
   ("C-c e r" . emms-random)
   ("C-c e +" . emms-volume-raise)
   ("C-c e -" . emms-volume-lower)
   ("C-c e b" . emms-browser)))

;; Helper function: Quick play from any directory
(defun emms-play-music-directory (dir)
  "Play all music from DIR."
  (interactive "DMusic directory: ")
  (emms-stop)
  (emms-playlist-current-clear)
  (emms-add-directory-tree dir)
  (emms-shuffle)
  (emms-start))

;; Helper function: Change default music directory
(defun emms-set-music-directory (dir)
  "Set the default music directory to DIR."
  (interactive "DNew music directory: ")
  (setq emms-source-file-default-directory dir)
  (message "Music directory set to: %s" dir))

;; Global keybindings for quick access
(global-set-key (kbd "C-c m p") 'emms-play-music-directory)
(global-set-key (kbd "C-c m d") 'emms-set-music-directory)
(global-set-key (kbd "C-c m m") 'emms)