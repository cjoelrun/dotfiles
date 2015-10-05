(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(require 'el-get-elpa)

(setq el-get-sources
      '((:name grails-projectile-mode
               :pkgname "yveszoundi/grails-projectile-mode"
               :description "Grails support for Projectile."
               :type github
               :after (progn
                        (require 'grails-projectile-mode)
                        (grails-projectile-global-mode t)
                        (require 'grails-projectile-discover)
                        ;; (grails-projectile-discover-setup-keybindings)
                        ))
        (:name eclim-groovy
               :website "https://github.com/yveszoundi/emacs-eclim/"
               :description "This project brings some of the great eclipse features to emacs developers."
               :type github
               :pkgname "yveszoundi/emacs-eclim"
               :features eclim
               :depends (s discover)
               :post-init (progn
                            (setq eclim-auto-save t)
                            (global-eclim-mode -1))
               :after (require 'eclimd))))
(el-get-bundle deft)
(el-get-bundle magit)
(el-get-bundle yasnippet)
(el-get-bundle git-gutter+)
(el-get-bundle find-file-in-project)
(el-get-bundle elpy)
(el-get-bundle plantuml-mode)
(el-get-bundle cider)

(el-get 'sync)

;; eclim
;; (global-eclim-mode)
;; (custom-set-variables
;;  '(eclim-eclipse-dirs '("/Applications/Eclipse.app/Contents/Eclipse"))
;;  '(eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/eclim"))
;; (setq help-at-pt-display-when-idle t)
;; (setq help-at-pt-timer-delay 0.1)
;; (help-at-pt-set-timer)
;; (require 'company)
;; (require 'company-emacs-eclim)
;; (company-emacs-eclim-setup)
;; (global-company-mode t)
;; (require 'eclimd)


;; typescript
(require 'typescript)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(require 'tss)

;; Key binding
(setq tss-popup-help-key "C-/")
(setq tss-jump-to-definition-key "C->")
(setq tss-implement-definition-key "C-c i")

;; Make config suit for you. About the config item, eval the following sexp.
;; (customize-group "tss")

;; Do setting recommemded configuration
(tss-config-default)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
;; (when (fboundp 'file-notify-add-watch)
;;   (add-hook 'magit-status-mode-hook 'magit-filenotify-mode))
(setq magit-save-some-buffers nil) ;don't ask to save buffers
(setq magit-set-upstream-on-push t) ;ask to set upstream
(setq magit-diff-refine-hunk t) ;show word-based diff for current hunk

;; git-gutter+
(global-git-gutter+-mode t)
(eval-after-load 'git-gutter+
  '(progn
     (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x v r") 'git-gutter+-revert-hunks)
     (define-key git-gutter+-mode-map (kbd "C-x v t") 'git-gutter+-stage-hunks)
     (define-key git-gutter+-mode-map (kbd "C-x v c") 'git-gutter+-commit)
     (define-key git-gutter+-mode-map (kbd "C-x v C") 'git-gutter+-stage-and-commit)))

;; find-file-in-project
(global-set-key (kbd "C-x f") 'find-file-in-project)

;; deft
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title t)
(setq deft-auto-save-interval 10.0)

;; grails
(add-to-list 'auto-mode-alist '("\.gsp$" . web-mode))
