;; Auto byte compile .emacs file
(defun autocompile nil
  "compile itself if ~/.emacs.d/init.el"
  (interactive)
  (require 'bytecomp)
  (let ((dotemacs (expand-file-name "~/.emacs.d/init.el")))
    (if (string= (buffer-file-name) (file-chase-links dotemacs))
        (byte-compile-file dotemacs))))
(add-hook 'after-save-hook 'autocompile)

;; start emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; general settings
(setq
 inhibit-splash-screen t                     ; no splash screen
 fill-column 79
 ring-bell-function 'ignore                  ;bell
 tramp-default-method "ssh"                  ;ssh for tramp
 ns-pop-up-frames 'nil                       ;Don't pop-up new windows
 backup-by-copying t                         ;Transmit compatability
 system-uses-terminfo nil                    ;ansi-term zsh support
 inhibit-startup-message t
 backup-directory-alist `((".*" . ,temporary-file-directory));don't clutter my fs and put backups into tmp
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 require-final-newline t                     ;auto add newline at the end of file
 column-number-mode t                        ;show the column number
 history-length 250                      ;default is 30
 locale-coding-system 'utf-8                     ;utf-8 is default
 tab-always-indent 'complete                     ;try to complete before identing
 confirm-nonexistent-file-or-buffer nil              ;don't ask to create a buffer
 vc-follow-symlinks t                        ;follow symlinks automatically
 send-mail-function 'sendmail-send-it
 kill-ring-max 5000                      ;truncate kill ring after 5000 entries
 mark-ring-max 5000                      ;truncate mark ring after 5000 entries
 mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control)))    ;make mouse scrolling smooth
 indicate-buffer-boundaries 'left                ;fringe markers
 split-height-threshold 110                  ;more readily split horziontally
 enable-recursive-minibuffers t
 recentf-save-file (expand-file-name "~/.recentf")
 )
(setq-default
 tab-width 4
 indent-tabs-mode nil ;use spaces instead of tabs
 c-basic-offset 4     ;"tab" with in c-related modes
 c-hungry-delete-key t;delete more than one space
 ispell-program-name "/usr/local/bin/aspell"
 )

(recentf-mode 1)
(global-auto-revert-mode 1);auto revert buffers when changed on disk
(show-paren-mode t)        ;visualize()
(blink-cursor-mode -1)     ;no cursor blinking
(tool-bar-mode -1)         ;disable the awful toolbar
(column-number-mode 1)     ; column numbers in the mode line
(menu-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(when (display-graphic-p)
  (scroll-bar-mode -1))
(global-hl-line-mode)      ; highlight current line

;; avoid compiz manager rendering bugs
(add-to-list 'default-frame-alist '(alpha . 100))

;; kill whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)

;; (display-battery-mode 1)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; Delete selection on input
(delete-selection-mode 1)

;; clear command in eshell
(defun eshell/clear ()
  "Clear eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

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

;; ido
(setq ido-enable-flex-matching t
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-everywhere t
      ido-default-buffer-method 'selected-window
      ido-max-prospects 32
      ido-use-filename-at-point 'guess
      )
(ido-mode 1)
(setq ido-use-faces nil)

;; encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(add-to-list 'load-path "~/.emacs.d/external/")
(load-library "external")

(add-to-list 'load-path "~/.emacs.d/lisp")
(when (string-match "apple-darwin" system-configuration)
  (load-library "osx"))
(when (eq system-type 'gnu/linux)
  (load-library "linux"))
