;;; .gnus.el --- summary -*- lexical-binding: t -*-

;; Author: Thomas Trabue
;; Maintainer:
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords

;;; Commentary:

;; Configuration file for the Gnus news reader, an Emacs-based application
;; for reading news feeds and interfacing with email.

;;; Code:

;;; Basic information
(setq user-mail-address "tom.trabue@gmail.com"
      user-full-name    "Thomas Jacob Trabue")

;;; Incoming and outgoing server configuration
(setq gnus-select-method
      '(nnimap "gmail"
         ; This value could also be imap.googlemail.com if that's your server.
	       (nnimap-address     "imap.gmail.com")
	       (nnimap-server-port "imaps")
	       (nnimap-stream       ssl)))

;;; SMTP settings
(setq smtpmail-smtp-server    "smtp.gmail.com"
      smtpmail-smtp-service   587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
      send-mail-function      'smtpmail-send-it)

;;; Daemons
;; Basic syntax:
;;   (gnus-demon-add-handler 'handler-func <time> <idle>)
;;
;; Notes from the manual:
;; This time parameter and that idle parameter work together in a strange, but
;; wonderful fashion. Basically, if idle is nil, then the function will be
;; called every time minutes.
;;
;; If idle is t, then the function will be called after time minutes only if
;; Emacs is idle. So if Emacs is never idle, the function will never be called.
;; But once Emacs goes idle, the function will be called every time minutes.
;;
;; If idle is a number and time is a number, the function will be called every
;; time minutes only when Emacs has been idle for idle minutes.
;;
;; If idle is a number and time is nil, the function will be called once every
;; time Emacs has been idle for idle minutes.
;;
;; And if time is a string, it should look like ‘07:31’, and the function will
;; then be called once every day somewhere near that time. Modified by the idle
;; parameter, of course.
;;
;; (When I say “minute” here, I really mean gnus-demon-timestep seconds. This is
;; 60 by default. If you change that variable, all the timings in the handlers
;; will be affected.)
;;
(add-hook 'gnus-startup-hook
  '(lambda ()
     (gnus-demon-init)
     ;; Each timestep is 60 seconds
     (setq gnus-demon-timestep 60)
     ;; Check for new mail every n timesteps (n minutes)
     (gnus-demon-add-handler 'gnus-demon-scan-news 2 t)

     ;; Don't crash gnus if disconnected
     (defadvice gnus-demon-scan-news (around gnus-demon-timeout activate)
       "Timeout for Gnus."
       (with-timeout
       (120 (message "Gnus timed out."))
     ad-do-it))))

;;; Hooks
;; Hooks after new news/mail arrives
(add-hook 'gnus-after-getting-new-news-hook
  #'(lambda ()
      ;; Send a notification over dbus
      (gnus-notifications)))

;;; Key bindings
;; NOTE: evil-mode interferes with many of the default key bindings for gnus.
;;       It is therefore advisable to create new bindings here.
;; Group buffer
; Mark articles in marked groups as read
(define-key gnus-group-mode-map (kbd "C-c c") 'gnus-group-catchup-current)
(define-key gnus-group-mode-map (kbd "C-c C") 'gnus-group-catchup-current-all)
; Check for new mail/news
(define-key gnus-group-mode-map (kbd "C-c g") 'gnus-group-get-new-news)
(define-key gnus-group-mode-map (kbd "C-c m") 'gnus-msg-mail)
;; Summary buffer
(define-key gnus-summary-mode-map (kbd "C-c m") 'gnus-msg-mail)
;;; Other key bindings reference:
;; Summary buffer:
; c : Catchup
; r : Reply without citing article
; R : Reply and cite article
;; Message buffer:
; C-c C-c   : Send message
; C-c C-d   : Save message as draft
; C-c C-k   : Kill message
; C-c C-m f : Attach file

;;; .gnus.el ends here
