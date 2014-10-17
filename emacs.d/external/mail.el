(add-to-list 'load-path "~/.emacs.d/external/mu4e-multi")

(require 'mu4e)
(require 'mu4e-multi)

(setq mu4e-multi-account-alist
      '(("personal"
         (user-mail-address . "cameronjlopez@gmail..com")
         (mu4e-drafts-folder . "/gmail/[Gmail].Drafts")
         (mu4e-sent-folder . "/gmail/[Gmail].Sent Mail")
         (mu4e-trash-folder . "/gmail/[Gmail].Trash"))
        ("work"
         (user-mail-address . "cameron.lopez@rackspace.com")
         (mu4e-drafts-folder . "/work/Drafts")
         (mu4e-sent-folder . "/work/Sent Items")
         (mu4e-trash-folder . "/work/Deleted Items")))
      )

(mu4e-multi-enable)

(setq mu4e-get-mail-command "offlineimap")
(setq mu4e-update-interval 300)
(setq mu4e-sent-messages-behavior 'delete)
(setq message-kill-buffer-on-exit t)
(setq mu4e-html2text-command "html2text -utf8 -nobs -width 79")
(setq mu4e-view-show-images t)
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))
(setq mu4e-maildir-shortcuts
      '( ("/gmail/INBOX"               . ?i)
         ("/gmail/[Gmail].All Mail"    . ?a)
         ("/work/INBOX"                . ?w)
         ("/work/INBOX.DvCD"           . ?d)
         ("/work/Archive"              . ?s)))

(setq
 user-mail-address "cameronjlopez@gmail.com"
 user-full-name  "Cameron Lopez"
 mu4e-compose-signature (concat "Cameron Lopez\n"))

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(global-set-key (kbd "C-x m") 'mu4e-multi-compose-new)
(global-set-key (kbd "C-c m") 'mu4e)
