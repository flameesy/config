;;; init-mu4e.el --- mu4e mail setup for Centaur Emacs -*- lexical-binding: t; -*-

;; ----------------------------------------
;; mu4e (comes with mu, not ELPA)
;; ----------------------------------------

(use-package mu4e
  :ensure nil
  :defer 20
  :config

  ;; ----------------------------------------
  ;; Basic paths
  ;; ----------------------------------------

  ;; Change this to your Maildir
  (setq mu4e-maildir "~/Mail")

  ;; Where sent / drafts / trash go
  (setq mu4e-sent-folder   "/sent"
        mu4e-drafts-folder "/drafts"
        mu4e-trash-folder  "/trash"
        mu4e-refile-folder "/archive")

  ;; ----------------------------------------
  ;; General behaviour
  ;; ----------------------------------------

  (setq mu4e-update-interval 300          ;; update every 5 minutes
        mu4e-get-mail-command "mbsync -a" ;; or "offlineimap"
        mu4e-index-update-in-background t
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-confirm-quit nil)

  ;; Better rendering
  (setq mu4e-html2text-command "pandoc -f html -t plain")

  ;; ----------------------------------------
  ;; Compose mail
  ;; ----------------------------------------

  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "smtp.example.com"
        smtpmail-smtp-server "smtp.example.com"
        smtpmail-smtp-service 587
        user-full-name "Your Name"
        user-mail-address "you@example.com")

  ;; ----------------------------------------
  ;; Contexts (multiple accounts ready)
  ;; ----------------------------------------

  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "default"
            :match-func (lambda (msg)
                          (when msg
                            (string-prefix-p "/default" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "you@example.com")
                    (user-full-name    . "Your Name")
                    (mu4e-sent-folder  . "/sent")
                    (mu4e-drafts-folder . "/drafts")
                    (mu4e-trash-folder . "/trash")
                    (mu4e-refile-folder . "/archive")))))

  (setq mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'ask)

  ;; ----------------------------------------
  ;; Headers view
  ;; ----------------------------------------

  (setq mu4e-headers-fields
        '((:human-date . 12)
          (:flags . 6)
          (:from . 22)
          (:subject)))

  ;; ----------------------------------------
  ;; Shortcuts
  ;; ----------------------------------------

  (setq mu4e-maildir-shortcuts
        '(("/inbox"   . ?i)
          ("/sent"    . ?s)
          ("/archive" . ?a)
          ("/trash"   . ?t)))

  ;; ----------------------------------------
  ;; Hooks
  ;; ----------------------------------------

  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
  (add-hook 'mu4e-compose-mode-hook #'flyspell-mode)

  ;; ----------------------------------------
  ;; Keybindings
  ;; ----------------------------------------

  (define-key mu4e-main-mode-map (kbd "q") #'mu4e-quit)
  (define-key mu4e-headers-mode-map (kbd "D") #'mu4e-mark-delete)
  (define-key mu4e-view-mode-map (kbd "D") #'mu4e-view-mark-delete))

(provide 'init-mu4e)
;;; init-mu4e.el ends here
