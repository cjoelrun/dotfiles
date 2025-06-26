;; straight.el setup - do this FIRST
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(setq straight-use-package-by-default t
      use-package-expand-minimally t)

;; Only use package.el for packages that require it
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(use-package auto-package-update
  :ensure t  ;; keep this one with package.el for auto-update functionality
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package counsel
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (ivy-mode 1))

(use-package transient)

(use-package magit
  :bind
  ("C-x g" . magit-status)
  :config
  ;; Remove M-p binding to avoid conflict with projectile
  (define-key magit-mode-map (kbd "M-p") nil))

(use-package forge
  :ensure t
  :after magit
  :config
    (setq auth-sources '("~/.authinfo"))
  )

(use-package git-gutter
  :ensure t
  :bind
  ("C-x v r" . git-gutter:revert-hunk)
  ("C-x v n" . git-gutter:next-hunk)
  ("C-x v p" . git-gutter:previous-hunk))

(use-package avy
  :ensure t
  :bind
  ("M-J" . avy-goto-char-2)
  ("M-L" . avy-goto-line))

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi t))

(use-package ag
    :ensure t)

(use-package projectile
  :ensure t
  :config
  (setq projectile-use-git-grep t)
  (setq projectile-project-search-path '("~/work/"))
  (projectile-discover-projects-in-search-path)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  :bind
  ("M-p" . projectile-command-map)
  )

(use-package flycheck)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp)

(use-package lsp-ui
  :hook ((lsp-mode . lsp-ui-mode))
  :bind (:map lsp-ui-mode-map
              ("C-c r" . lsp-ui-peek-find-references)
              ("M-." . lsp-ui-peek-find-definitions)
              ("C-c i" . lsp-ui-peek-find-implementation)
              ("C-c d" . lsp-ui-doc-glance)
              ("C-c m" . lsp-ui-imenu)))

;; add company mode for lsp mode
(use-package company
  :ensure t)

(use-package which-key
  :ensure t)

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :commands dap-debug
  :hook ((python-mode . dap-ui-mode) (python-mode . dap-mode) (rust-mode . dap-ui-mode) (rust-mode . dap-mode))
  :config
  ;; python
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (defun dap-python--pyenv-executable-find (command)
    (with-venv (executable-find "python")))
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  ;; rust doesn't work
  ;; https://marketplace.visualstudio.com/items?itemName=vadimcn.vscode-lldb
  ;; (require 'dap-codelldb)
  ;; (require 'dap-gdb-lldb)
  ;; (require 'dap-cpptools)
  ;; (dap-gdb-lldb-setup)
  ;; (dap-register-debug-template "Rust::LLDB Run Configuration"
  ;;                              (list :type "lldb"
  ;;                                  :request "launch"
  ;;                                  :name "LLDB::Run"
  ;;                          :gdbpath "rust-lldb"
  ;;                                  :target nil
  ;;                                  :cwd nil))
  )


;; python
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;; (use-package quelpa-use-package)
;; (use-package copilot
;;   :diminish
;;   :quelpa (copilot :fetcher github :repo "zerolfx/copilot.el"
;;                    :files ("*.el" "dist"))
;;   :init
;;   (defun my-copilot-complete ()
;;     (interactive)
;;     (or (copilot-accept-completion)
;;         (move-end-of-line nil)))
;;   :hook (prog-mode . copilot-mode)
;;   :bind (:map copilot-mode-map
;;               ("C-e" . 'my-copilot-complete)))

(use-package markdown-mode
  :config
  (setq markdown-command "pandoc -f markdown -t html -s --mathjax --highlight-style pygments")
  ;; fix keybindings for projectile
  (define-key markdown-mode-map (kbd "M-p") nil))

(use-package deft
  :ensure
  :config
  (setq deft-directory "/Users/cameronlopez/Library/Mobile Documents/iCloud~md~obsidian/Documents/cameron")
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-auto-save-interval 10.0)
  (setq deft-recursive t)
  (setq deft-default-extension "org")
  (setq deft-recursive-ignore-dir-regexp "\\(?:\\.\\|\\.\\.\\|history\\)\\'")
  )

(use-package web-mode
  :hook
  (web-mode . lsp-deferred)
  :mode (("\\.tsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.jinja\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.scss\\'" . web-mode)
         ("\\.json\\'" . web-mode)
         ("\\.graphql\\'" . web-mode)
         ("\\.gql\\'" . web-mode)
         ("\\.mdx\\'" . web-mode)
         ("\\.yaml\\'" . web-mode)
         ("\\.yml\\'" . web-mode)
         ("\\.xml\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.twig\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  ;; indent ts and js files with 2 spaces)
  :config (setq web-mode-code-indent-offset 2))

(use-package rust-mode
  :ensure t
  :hook
  (rust-mode . lsp-deferred)
  :config
  (setq rust-format-on-save t)
  (setenv "RUST_LOG_SPAN_EVENTS" "full")
  (setenv "RUST_BACKTRACE" "1"))

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable "/Library/Developer/CommandLineTools/usr/bin/sourcekit-lsp"))

(use-package swift-mode
  :hook (swift-mode . (lambda () (lsp))))

;; LSP for Go
(use-package go-mode
  :hook
  (go-mode . lsp-deferred))

;; LSP for dockerfile
(use-package dockerfile-mode
  :hook
  (dockerfile-mode . lsp-deferred))

;; LSP for tailwind
(use-package lsp-tailwindcss)

;; gptel
(use-package gptel
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model "gpt-4o"))

;; Color cargo output
(use-package ansi-color
  :ensure t
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package protobuf-mode
  :ensure t)

;; straight.el managed packages




(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq swift-config
      (make-treesit-auto-recipe
       :lang 'swift
       :ts-mode 'swift-ts-mode
       :remap '(swift-mode)
       :url "https://github.com/alex-pinkus/tree-sitter-swift"
       :revision "master"
       :ext "\\.swift\\'"))
  (add-to-list 'treesit-auto-recipe-list swift-config)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
