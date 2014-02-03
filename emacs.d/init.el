;; Auto byte compile .emacs file
(defun autocompile nil
  "compile itself if ~/.emacs.d/init.el"
  (interactive)
  (require 'bytecomp)
  (let ((dotemacs (expand-file-name "~/.emacs.d/init.el")))
    (if (string= (buffer-file-name) (file-chase-links dotemacs))
                                        (byte-compile-file dotemacs))))
(add-hook 'after-save-hook 'autocompile)

;; ansi-term zsh support
(setq system-uses-terminfo nil)

;; start emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; set online bit
(setq my-onlinep nil)
(unless
    (condition-case nil
        (delete-process
         (make-network-process
          :name "my-check-internet"
          :host "elpa.gnu.org"
          :service 80))
      (error t))
  (setq my-onlinep t))

;; on to the visual settings
(setq inhibit-splash-screen t); no splash screen
(column-number-mode 1)        ; column numbers in the mode line
(setq fill-column 79)
(unless (string-match "apple-darwin" system-configuration)
  ;; on mac, there's always a menu bar drown, don't have it empty
  (menu-bar-mode -1))
(defalias 'yes-or-no-p 'y-or-n-p)

;; choose your own fonts, in a system dependant way
(if (string-match "apple-darwin" system-configuration)
    (set-frame-font
     "-apple-Inconsolata-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1"))
(add-to-list 'default-frame-alist
             '(font . "-apple-Inconsolata-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1"))

(global-hl-line-mode); highlight current line

;; avoid compiz manager rendering bugs
(add-to-list 'default-frame-alist '(alpha . 100))

(display-battery-mode 1)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; bell
(setq ring-bell-function 'ignore)

;; Delete selection on input
(delete-selection-mode 1)

;; For tramp with sudo.
(setq tramp-default-method "ssh")

;; clear command in eshell
(defun eshell/clear ()
  "Clear eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; Don't pop-up new windows
(setq ns-pop-up-frames 'nil)

;; Transmit compatability
(setq backup-by-copying t)

;; aspell
(setq-default ispell-program-name "/usr/local/bin/aspell")

;; Auctex
(setq ispell-program-name "aspell")
(setq ispell-dictionary "english")
(setq TeX-PDF-mode t)
(setq TeX-parse-self t)
(setq TeX-auto-save t)
(setq TeX-save-query nil)

(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(add-hook 'TeX-mode-hook
          '(lambda ()
             (define-key TeX-mode-map (kbd "<C-f8>")
               (lambda ()
                 (interactive)
                 (TeX-command-menu "LaTeX")))
             ))

;; el-get
(require 'package)
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(setq el-get-install-skip-emacswiki-recipes t)
(unless (require 'el-get nil 'noerror)
  (if my-onlinep
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp))
    (error "El-Get is not installed and we are unable to download it without an internet connection: cannot continue")))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.
(setq el-get-sources
      '(
        (:name eval-sexp-fu
               :type http
               :url "http://www.emacswiki.org/emacs/download/eval-sexp-fu.el")
        (:name helm-google
               :type git
               :url "https://github.com/steckerhalter/helm-google")
        (:name magit-filenotify
               :type git
               :url "https://github.com/magit/magit-filenotify")
        ))

(setq my-el-get-packages
      (append
       '()
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-el-get-packages)

;; package repos
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(setq my-packages
      '(ac-nrepl
        ac-slime
        ace-jump-mode
        ag
        apache-mode
        auto-complete
        cl-lib
        color-theme
        company
        csv-mode
        ctable
        dash
        deferred
        deft
        direx
        discover
        elpy
        epc
        epl
        erc-hl-nicks
        expand-region
        f
        find-file-in-project
        flx-ido
        flycheck
        flymake-easy
        flymake-ruby
        flymake-shell
        fuzzy
        gh
        gist
        grizzl
        haskell-mode
        helm
        helm-c-yasnippet
        helm-descbinds
        helm-git
        helm-gtags
        helm-projectile
        helm-swoop
        hide-comnt
        highlight-indentation
        htmlize
        idomenu
        iedit
        jedi
        jinja2-mode
        js2-mode
        json
        json-mode
        logito
        magit
        magit-gh-pulls
        markdown-mode+
        multi-term
        nose
        nrepl-eval-sexp-fu
        org
        pcache
        pkg-info
        popup
        request
        rhtml-mode
        rinari
        robe
        s
        scss-mode
        skewer-mode
        smart-mode-line
        tabulated-list
        virtualenv
        web-mode
        xclip
        yaml-mode
        yasnippet))

(package-initialize)
(when my-onlinep
  (package-refresh-contents)
  (cl-loop for p in my-packages
           unless (package-installed-p p)
           do (package-install p)))

;; load color
(color-theme-initialize)
(color-theme-midnight)

;; encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; general settings
(setq
 inhibit-startup-message t
 backup-directory-alist `((".*" . ,temporary-file-directory)) ;don't clutter my fs and put backups into tmp
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 require-final-newline t          ;auto add newline at the end of file
 column-number-mode t             ;show the column number
 history-length 250        ;default is 30
 locale-coding-system 'utf-8          ;utf-8 is default
 tab-always-indent 'complete          ;try to complete before identing
 confirm-nonexistent-file-or-buffer nil ;don't ask to create a buffer
 vc-follow-symlinks t                   ;follow symlinks automatically
 send-mail-function 'sendmail-send-it
 kill-ring-max 5000                     ;truncate kill ring after 5000 entries
 mark-ring-max 5000                     ;truncate mark ring after 5000 entries
 mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))) ;make mouse scrolling smooth
 indicate-buffer-boundaries 'left             ;fringe markers
 split-height-threshold 110                   ;more readily split horziontally
 enable-recursive-minibuffers t
 )
(setq-default
 tab-width 4
 indent-tabs-mode nil                   ;use spaces instead of tabs
 c-basic-offset 4                       ;"tab" with in c-related modes
 c-hungry-delete-key t                  ;delete more than one space
 )

(global-auto-revert-mode 1)  ;auto revert buffers when changed on disk
(show-paren-mode t)          ;visualize()
(iswitchb-mode t)            ;use advanced tab switching
(blink-cursor-mode -1)       ;no cursor blinking
(tool-bar-mode -1)           ;disable the awful toolbar
(scroll-bar-mode -1)         ;disable the sroll bar
(global-visual-line-mode 1)

;; System specific settings
;; linux
(when (eq system-type 'gnu/linux)
  (setq browse-url-generic-program "/usr/bin/conkeror")
)
;; under mac, have Command as Meta and keep Option for localized input
(when (string-match "apple-darwin" system-configuration)
  (setq browse-url-generic-program "open")
  (setq mac-allow-anti-aliasing t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta))

;; osx path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; company mode
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
(defun my-pcomplete-capf ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
(add-hook 'org-mode-hook #'my-pcomplete-capf)

;; Google Search
(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Google: "))))))
(global-set-key (kbd "C-c g") 'google)

;; ;; ERC
(add-hook 'erc-mode-hook (lambda ()
                           (erc-truncate-mode t)
                           (erc-fill-disable)
                           (set (make-local-variable 'scroll-conservatively) 1000)
                           (visual-line-mode)
                           )
          )
(setq erc-timestamp-format "%H:%M "
      erc-fill-prefix "      "
      erc-insert-timestamp-function 'erc-insert-timestamp-left)
(setq erc-interpret-mirc-color t)
(setq erc-kill-buffer-on-part t)
(setq erc-kill-queries-on-quit t)
(setq erc-kill-server-buffer-on-quit t)
(setq erc-server-send-ping-interval 45)
(setq erc-server-send-ping-timeout 180)
(setq erc-server-reconnect-timeout 60)
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

;; ------ template for .user.el
;; (setq erc-prompt-for-nickserv-password nil)
;; (setq erc-server "hostname"
;;       erc-port 7000
;;       erc-nick "user"
;;       erc-user-full-name "user"
;;       erc-email-userid "user"
;;       erc-password "user:pw"
;;       )

;; (setq erc-autojoin-channels-alist
;;       '(("freenode.net" "#rcbops" "#pcqa" "#cloudcafe")
;; (erc :server "irc.freenode.net" :port 6667 :nick "cjoelrun" :password easy-pass)
;; (erc-tls :server "pretentious.me" :port 6697 :nick "cjoelrun" :password hard-pass)
;; (erc :server "pretentious.me"
;;      :port 8888
;;      :nick "cjoelrun"
;;      :password (format "cameron:%s" hard-pass))
;; (and
;;  (require 'erc-highlight-nicknames)
;;  (add-to-list 'erc-modules 'highlight-nicknames)
;;  (erc-update-modules))

(add-hook 'erc-join-hook 'bitlbee-identify)
(defun bitlbee-identify ()
  "If we're on the bitlbee server, send the identify command to the
 &bitlbee channel."
  (when (and (string= "pretentious.me" erc-session-server)
                  (string= "&bitlbee" (buffer-name)))
    (erc-message "PRIVMSG" (format "%s identify %s"
                                      (erc-default-target)
                                         hard-pass))))

;; Bottle
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . html-mode))

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
(global-set-key (kbd "C-M-<return>") 'toggle-fullscreen)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-generic))
 '(indent-tabs-mode nil)
 '(org-agenda-files (quote ("~/Dropbox/Notes/todo.org")))
 '(org-file-apps (quote ((auto-mode . emacs) ("\\.mm\\'" . default) ("\\.x?html?\\'" . default) ("\\.pdf\\'" . emacs))))
 '(virtualenv-root "~/.venvs/"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'flymake-shell)
(add-hook 'sh-mode-hook 'flymake-shell-load)

;; deft
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title t)
(add-hook 'deft-mode-hook (lambda  () (setq word-wrap nil)))

;; w3m
(when (require 'w3m nil t)
  (setq
   w3m-use-favicon nil
   w3m-default-display-inline-images t
   w3m-search-word-at-point nil
   w3m-use-cookies t
   w3m-home-page "http://en.wikipedia.org/"
   w3m-cookie-accept-bad-cookies t
   w3m-session-crash-recovery nil)
  (add-hook 'w3m-mode-hook
            (function (lambda ()
                        (set-face-foreground 'w3m-anchor-face "LightSalmon")
                        (set-face-foreground 'w3m-arrived-anchor-face "LightGoldenrod")
                        ;;(set-face-background 'w3m-image-anchor "black")
                        (load "w3m-lnum")
                        (defun w3m-go-to-linknum ()
                          "Turn on link numbers and ask for one to go to."
                          (interactive)
                          (let ((active w3m-lnum-mode))
                            (when (not active) (w3m-lnum-mode))
                            (unwind-protect
                                (w3m-move-numbered-anchor (read-number "Anchor number: "))
                              (when (not active) (w3m-lnum-mode))))
                          (w3m-view-this-url)
                          )
                        (define-key w3m-mode-map "f" 'w3m-go-to-linknum)
                        (define-key w3m-mode-map "L" 'w3m-lnum-mode)
                        (define-key w3m-mode-map "o" 'w3m-previous-anchor)
                        (define-key w3m-mode-map "i" 'w3m-next-anchor)
                        (define-key w3m-mode-map "w" 'w3m-search-new-session)
                        (define-key w3m-mode-map "p" 'w3m-previous-buffer)
                        (define-key w3m-mode-map "n" 'w3m-next-buffer)
                        (define-key w3m-mode-map "z" 'w3m-delete-buffer)
                        (define-key w3m-mode-map "O" 'w3m-goto-new-session-url)
                        )))
  )

;; haskell-mode
(require 'haskell-mode)
(setq haskell-indent-thenelse 3)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; rbenv
(setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

;; ace jump
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; kill whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)

;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))

;; plantuml
(setq org-plantuml-jar-path
      (expand-file-name "~/plantuml.jar"))

;; User path
(setenv "PATH" (concat (getenv "HOME") "/bin:" (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/bin:") exec-path))

;; fiplr
(global-set-key (kbd "C-x f") 'fiplr-find-file)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(when (fboundp 'file-notify-add-watch)
  (add-hook 'magit-status-mode-hook 'magit-filenotify-mode))
(setq magit-save-some-buffers nil) ;don't ask to save buffers
(setq magit-set-upstream-on-push t) ;ask to set upstream
(setq magit-diff-refine-hunk t) ;show word-based diff for current hunk

;; git gutter
(global-git-gutter+-mode t)
(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)
(eval-after-load 'git-gutter+
  '(progn
     ;;; Jump between hunks
     (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
     ;;; Act on hunks
     (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x v r") 'git-gutter+-revert-hunks)
     ;; Stage hunk at point.
     ;; If region is active, stage all hunk lines within the region.
     (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
     (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
     (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)))

;; smart mode line
(setq sml/vc-mode-show-backend t)
(sml/setup)
;; (sml/apply-theme 'respectful)
;; (set-face-attribute 'sml/prefix nil :foreground "#dcf692")
;; (set-face-attribute 'sml/folder nil :foreground "#f09fff")
;; (set-face-attribute 'sml/filename nil :foreground "#f6df92")
;; (set-face-attribute 'sml/vc-edited nil :foreground "#ff5f87")

;; clipboard
(turn-on-xclip)

;; python
(elpy-enable)
(elpy-use-ipython)
(elpy-clean-modeline)
(setq elpy-rpc-backend "jedi")
(setq elpy-rpc-python-command "python2.7")
(delq 'flymake-mode elpy-default-minor-modes)

(autoload 'pylint "pylint")
(add-hook 'python-mode-hook 'pylint-add-menu-items)
(add-hook 'python-mode-hook 'pylint-add-key-bindings)

(require 'direx)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)
(eval-after-load "python"
  '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))

;; flycheck
(add-hook 'php-mode-hook 'flycheck-mode)
(add-hook 'sh-mode-hook 'flycheck-mode)
(add-hook 'json-mode-hook 'flycheck-mode)
(add-hook 'nxml-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(add-hook 'lisp-interaction-mode-hook 'flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)) ;disable the annoying doc checker
(setq flycheck-indication-mode 'left-fringe)

;; projectile
(require 'projectile nil t)
(setq projectile-completion-system 'grizzl)

;; grizzl
(setq *grizzl-read-max-results* 30)

;; helm
(require 'helm-config)
(setq helm-mode-handle-completion-in-region nil) ; don't use helm for `completion-at-point'
(helm-mode 1)
(helm-gtags-mode 1)
(helm-descbinds-mode)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(setq helm-buffer-max-length 50)
(setq helm-M-x-always-save-history t)
(setq helm-buffer-details-flag nil)
(add-to-list 'helm-completing-read-handlers-alist '(org-refile)) ; helm-mode does not do org-refile well
(add-to-list 'helm-completing-read-handlers-alist '(org-agenda-refile)) ; same goes for org-agenda-refile
(require 'helm-git)
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

;; save recently opened files
(setq recentf-save-file (expand-file-name "~/.recentf"))
(recentf-mode 1)

;; navigation
(setq ido-enable-flex-matching t
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-everywhere t
      ido-default-buffer-method 'selected-window
      ido-max-prospects 32
      ido-use-filename-at-point 'guess
      )
(ido-mode 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)

;; iedit
(require 'iedit)
(setq iedit-unmatched-lines-invisible-default t)

;; json
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

;; ruby
(add-hook 'ruby-mode-hook
          (lambda ()
            (robe-mode 1)
            (push 'company-robe company-backends)))

;; skewer mode
(skewer-setup)

;; Unique buffer names dependent on file name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-min-dir-content 2)

;; org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(require 'org)
(require 'ox-org)
(require 'ox-md)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-startup-folded t)
(setq org-startup-indented nil)
(setq org-startup-with-inline-images t)
(setq org-startup-truncated t)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-edit-src-content-indentation 0)
(setq org-confirm-babel-evaluate nil)
(setq org-use-speed-commands t)
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

;; create the file for the agendas if it doesn't exist
(let ((agendas "~/.agenda_files"))
  (unless (file-readable-p agendas)
    (with-temp-file agendas nil))
  (setq org-agenda-files agendas))

;; display the agenda first
(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODO's"
        ((alltodo "")
         (agenda "")))))

(defun my-initial-buffer-choice ()
  (org-agenda nil "n")
  (delete-other-windows)
  (current-buffer))
(setq initial-buffer-choice #'my-initial-buffer-choice)

(setq org-agenda-start-with-log-mode t)
(setq org-agenda-todo-ignore-scheduled 'future) ; don't show future scheduled
(setq org-agenda-todo-ignore-deadlines 'far)    ; show only near deadlines

(setq
 appt-message-warning-time 30
 appt-display-interval 15
 appt-display-mode-line t      ; show in the modeline
 appt-display-format 'window)
(appt-activate 1)              ; activate appt (appointment notification)

(org-agenda-to-appt)           ; add appointments on startup

;; add new appointments when saving the org buffer, use 'refresh argument to do it properly
(defun my-org-agenda-to-appt-refresh () (org-agenda-to-appt 'refresh))
(defun my-org-mode-hook ()
  (add-hook 'after-save-hook 'my-org-agenda-to-appt-refresh nil 'make-it-local))
(add-hook 'org-mode-hook 'my-org-mode-hook)

(require 'notifications)
(defun my-appt-disp-window-function (min-to-app new-time msg)
  (notifications-notify :title (format "Appointment in %s min" min-to-app) :body msg))
(setq appt-disp-window-function 'my-appt-disp-window-function)
(setq appt-delete-window-function (lambda (&rest args)))

;; add state to the sorting strategy of todo
(setcdr (assq 'todo org-agenda-sorting-strategy) '(todo-state-up priority-down category-keep))

(setq org-capture-templates
      '(
        ("t" "Task" entry (file "") "* TODO %?\n %a")
        ("s" "Simple Task" entry (file "") "* TODO %?\n")
        ))

(add-to-list 'org-structure-template-alist '("E" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC\n"))
(add-to-list 'org-structure-template-alist '("S" "#+BEGIN_SRC shell-script\n?\n#+END_SRC\n"))

(setq org-todo-keywords
      '((sequence
         "TODO(t)"
         "WAITING(w)"
         "SCHEDULED(s)"
         "FUTURE(f)"
         "|"
         "DONE(d)"
         )))
(setq org-todo-keyword-faces
      '(
        ("SCHEDULED" . warning)
        ("WAITING" . font-lock-doc-face)
        ("FUTURE" . "white")
        ))
(setq org-log-into-drawer t) ; don't clutter files with state logs

(setq org-clock-idle-time 15)
(setq org-clock-in-resume t)
(setq org-clock-persist t)
(org-clock-persistence-insinuate)
(setq org-clock-frame-title-format (append '((t org-mode-line-string)) '(" ") frame-title-format))
(setq org-clock-clocked-in-display 'both)

(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
