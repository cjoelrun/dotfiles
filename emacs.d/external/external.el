;; magit
(add-to-list 'load-path "~/.emacs.d/external/git-modes")
(add-to-list 'load-path "~/.emacs.d/external/magit")
(eval-after-load 'info
  '(progn (info-initialize)
      (add-to-list 'Info-directory-list "~/.emacs.d/external/magit")))
(require 'magit)
(require 'magit-blame)
(global-set-key (kbd "C-x g") 'magit-status)
;; (when (fboundp 'file-notify-add-watch)
;;   (add-hook 'magit-status-mode-hook 'magit-filenotify-mode))
(setq magit-save-some-buffers nil) ;don't ask to save buffers
(setq magit-set-upstream-on-push t) ;ask to set upstream
(setq magit-diff-refine-hunk t) ;show word-based diff for current hunk

;; magit-filenotify
;; (add-to-list 'load-path "~/.emacs.d/external/magit-filenotify")
;; (require 'magit-filenotify)

;; git-gutter+
(add-to-list 'load-path "~/.emacs.d/external/git-gutter-plus")
(require 'git-gutter+)
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

;; elpy requirements
(add-to-list 'load-path "~/.emacs.d/external/emacs-ctable")
(add-to-list 'load-path "~/.emacs.d/external/emacs-deferred")
(add-to-list 'load-path "~/.emacs.d/external/emacs-epc")
(add-to-list 'load-path "~/.emacs.d/external/popup-el")
(add-to-list 'load-path "~/.emacs.d/external/emacs-python-environment")
(add-to-list 'load-path "~/.emacs.d/external/company-mode")

(require 'company)
(setq company-idle-delay 0.3)
(setq company-tooltip-limit 20)
(setq company-minimum-prefix-length 2)
(setq company-echo-delay 0)
(setq company-auto-complete nil)
(global-company-mode 1)
(add-to-list 'company-backends 'company-dabbrev t)
(add-to-list 'company-backends 'company-ispell t)
(add-to-list 'company-backends 'company-files t)
(setq company-backends (remove 'company-ropemacs company-backends))
;; This enables company completion for org-mode built-in commands and tags.
(defun my-pcomplete-capf ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
(add-hook 'org-mode-hook #'my-pcomplete-capf)

(add-to-list 'load-path "~/.emacs.d/external/emacs-jedi")
(add-to-list 'load-path "~/.emacs.d/external/yasnippet")
(add-to-list 'load-path "~/.emacs.d/external/Highlight-Indentation-for-Emacs")
(add-to-list 'load-path "~/.emacs.d/external/find-file-in-project")

(require 'find-file-in-project)
(global-set-key (kbd "C-x f") 'find-file-in-project)

(add-to-list 'load-path "~/.emacs.d/external/idomenu")
(autoload 'idomenu "idomenu" nil t)

(add-to-list 'load-path "~/.emacs.d/external/iedit")
(require 'iedit)
(add-to-list 'load-path "~/.emacs.d/external/nosemacs")
(add-to-list 'load-path "~/.emacs.d/external/pyvenv")

;; elpy
(add-to-list 'load-path "~/.emacs.d/external/elpy")
(require 'elpy)
(elpy-enable)
(elpy-use-ipython)
(setq elpy-rpc-python-command "python2.7")

;; deft
(add-to-list 'load-path "~/.emacs.d/external/deft")
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title t)
(setq deft-auto-save-interval 10.0)
(require 'deft)

;; deft
(add-to-list 'load-path "~/.emacs.d/external/xclip")
(require 'xclip)

;; mail
(when (eq system-type 'gnu/linux)
  (load-library "mail"))

;; yaml
(add-to-list 'load-path "~/.emacs.d/external/yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; eww lnum
(add-to-list 'load-path "~/.emacs.d/external/eww-lnum")
(eval-after-load "eww"
  '(progn (define-key eww-mode-map "f" 'eww-lnum-follow)
          (define-key eww-mode-map "F" 'eww-lnum-universal)))

;; restclient
(add-to-list 'load-path "~/.emacs.d/external/json-reformat")
(add-to-list 'load-path "~/.emacs.d/external/restclient")
(require 'restclient)
