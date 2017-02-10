
;; Org-Gcal
(require 'org)
(require 'org-gcal)

(setq org-gcal-client-id "**********"
      org-gcal-client-secret "*********"
      org-gcal-file-alist '(("**********" .  "~/.emacs.d/calendar.org")))

;; Org-Agenda
(setq org-agenda-files (list "~/.emacs.d/agenda.org"))


;; Calendar UI
(require 'calfw)
(require 'calfw-org)
(provide '.org)
;;; .org.el ENDS HERE
