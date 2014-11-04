(require 'mu4e)

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587)

;; default
(setq user-mail-address "cameronjlopez@gmail..com"
      mu4e-drafts-folder "/gmail/[Gmail].Drafts"
      mu4e-sent-folder "/gmail/[Gmail].Sent Mail"
      mu4e-trash-folder "/gmail/[Gmail].Trash"
      mu4e-refile-folder "/gmail/[Gmail].All Mail"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; multi account support
(defvar my-mu4e-account-alist
  '(("personal"
     (user-mail-address "cameronjlopez@gmail..com")
     (mu4e-drafts-folder "/gmail/[Gmail].Drafts")
     (mu4e-sent-folder "/gmail/[Gmail].Sent Mail")
     (mu4e-trash-folder "/gmail/[Gmail].Trash")
     (mu4e-refile-folder "/gmail/[Gmail].All Mail")
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-service 587))
    ("work"
     (user-mail-address "cameron.lopez@rackspace.com")
     (mu4e-drafts-folder "/work/Drafts")
     (mu4e-sent-folder "/work/Sent Items")
     (mu4e-trash-folder "/work/Deleted Items")
     (mu4e-refile-folder "/work/Archive")
     (smtpmail-smtp-server "smtpout.rackspace.com")
     (smtpmail-smtp-service 25))))
(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

(setq mu4e-get-mail-command "offlineimap")
(setq mu4e-update-interval 300)
(setq mu4e-sent-messages-behavior 'delete)
(setq message-kill-buffer-on-exit t)
(setq mu4e-html2text-command "html2text -utf8 -nobs -width 79")
(setq mu4e-view-show-images t)
;; (when (fboundp 'imagemagick-register-types)
;;   (imagemagick-register-types))
(setq mu4e-maildir-shortcuts
      '( ("/gmail/INBOX"               . ?i)
         ("/gmail/[Gmail].All Mail"    . ?a)
         ("/work/INBOX"                . ?w)
         ("/work/INBOX.DvCD"           . ?d)
         ("/work/Archive"              . ?s)))

(setq
 user-full-name  "Cameron Lopez"
 mu4e-compose-signature (concat "Cameron Lopez\n"))

(global-set-key (kbd "C-x m") 'mu4e)

;; (add-to-list 'load-path "~/.emacs.d/external/mu4e-multi")

;; (require 'mu4e)
;; (require 'mu4e-multi)

;; (setq mu4e-maildir "~/Maildir")
;; (setq mu4e-multi-account-alist
;;       '(("personal"
;;          (mu4e-maildir "~/Maildir/gmail")
;;          (user-mail-address . "cameronjlopez@gmail..com")
;;          (mu4e-drafts-folder . "/gmail/[Gmail].Drafts")
;;          (mu4e-sent-folder . "/gmail/[Gmail].Sent Mail")
;;          (mu4e-trash-folder . "/gmail/[Gmail].Trash")
;;          (mu4e-refile-folder . "/gmail/[Gmail].All Mail")
;;          (smtpmail-smtp-server "smtp.gmail.com")
;;          (smtpmail-smtp-service 587))
;;         ("work"
;;          (mu4e-maildir "~/Maildir/work")
;;          (user-mail-address . "cameron.lopez@rackspace.com")
;;          (mu4e-drafts-folder . "/work/Drafts")
;;          (mu4e-sent-folder . "/work/Sent Items")
;;          (mu4e-trash-folder . "/work/Deleted Items")
;;          (mu4e-refile-folder . "/work/Archive")
;;          (smtpmail-smtp-server "smtpout.rackspace.com")
;;          (smtpmail-smtp-service 25)))
;;       )

;; (mu4e-multi-enable)

;; (setq mu4e-get-mail-command "offlineimap")
;; (setq mu4e-update-interval 300)
;; (setq mu4e-sent-messages-behavior 'delete)
;; (setq message-kill-buffer-on-exit t)
;; (setq mu4e-html2text-command "html2text -utf8 -nobs -width 79")
;; (setq mu4e-view-show-images t)
;; (when (fboundp 'imagemagick-register-types)
;;   (imagemagick-register-types))
;; (setq mu4e-maildir-shortcuts
;;       '( ("/gmail/INBOX"               . ?i)
;;          ("/gmail/[Gmail].All Mail"    . ?a)
;;          ("/work/INBOX"                . ?w)
;;          ("/work/INBOX.DvCD"           . ?d)
;;          ("/work/Archive"              . ?s)))

;; (setq
;;  user-full-name  "Cameron Lopez"
;;  mu4e-compose-signature (concat "Cameron Lopez\n"))

;; (require 'smtpmail)
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-stream-type 'starttls
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;; )

;; (global-set-key (kbd "C-x m") 'mu4e-multi-compose-new)
;; (global-set-key (kbd "C-c m") 'mu4e)
