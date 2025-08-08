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

;; Ensure seq is available (required by transient)
(require 'seq)

;; Load transient before magit to ensure compatibility
(use-package transient
  :demand t  ;; Force immediate loading
  :init
  (setq transient-display-buffer-action '(display-buffer-below-selected))
  :config
  ;; Add the lisp subdirectory to load-path if needed
  (let ((transient-lisp-dir (expand-file-name "straight/repos/transient/lisp" user-emacs-directory)))
    (when (file-directory-p transient-lisp-dir)
      (add-to-list 'load-path transient-lisp-dir)))
  (require 'transient)
  ;; Ensure transient is fully loaded
  (unless (fboundp 'transient-prefix-object)
    (error "transient-prefix-object is not defined - transient may not be properly installed")))  ;; Explicitly require to ensure all functions are available

(use-package magit
  :after transient  ;; Ensure transient is loaded first
  :bind
  ("C-x g" . magit-status)
  :config
  ;; Remove M-p binding to avoid conflict with projectile
  (define-key magit-mode-map (kbd "M-p") nil))

(use-package forge
  :after magit
  :config
  (setq auth-sources '("~/.authinfo"))
  )

(use-package git-gutter
  :bind
  ("C-x v r" . git-gutter:revert-hunk)
  ("C-x v n" . git-gutter:next-hunk)
  ("C-x v p" . git-gutter:previous-hunk))

(use-package avy
  :bind
  ("M-J" . avy-goto-char-2)
  ("M-L" . avy-goto-line))

(use-package modus-themes
  :config
  (load-theme 'modus-vivendi t))

(use-package ag)

(use-package projectile
  :config
  (setq projectile-enable-caching nil)  ;; Disable caching to always find new files
  (setq projectile-use-git-grep t)
  (setq projectile-project-search-path '("~/work/" "~/work/worktrees/"))
  (projectile-discover-projects-in-search-path)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (projectile-mode +1)
  :bind-keymap
  ("M-p" . projectile-command-map))

(use-package flycheck)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-log-io nil) ; Disable verbose logging
  (setq lsp-file-watch-threshold 1000) ; Reduce file watching limit
  :commands lsp
  :hook
  (python-mode . lsp-deferred)
  (python-ts-mode . lsp-deferred)
  (rust-mode . lsp-deferred)
  (typescript-mode . lsp-deferred)
  (typescript-ts-mode . lsp-deferred)
  (tsx-ts-mode . lsp-deferred)
  (typescript-ts-mode . jest-test-mode)
  )

(use-package lsp-ui
  :hook ((lsp-mode . lsp-ui-mode))
  :bind (:map lsp-ui-mode-map
              ("C-c r" . lsp-ui-peek-find-references)
              ("M-." . lsp-ui-peek-find-definitions)
              ("C-c i" . lsp-ui-peek-find-implementation)
              ("C-c d" . lsp-ui-doc-glance)
              ("C-c m" . lsp-ui-imenu)))

;; add company mode for lsp mode
(use-package company)

(use-package which-key)

(use-package dap-mode
  :after lsp-mode
  :commands dap-debug
  :hook ((python-mode . dap-ui-mode) (python-mode . dap-mode) 
         (python-ts-mode . dap-ui-mode) (python-ts-mode . dap-mode)
         (rust-mode . dap-ui-mode) (rust-mode . dap-mode))
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


;; python - Manually register pyright to work around installation issues
(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection 
                     (lambda () 
                       (list "/Users/cameronlopez/Dotfiles/emacs.d/.cache/lsp/npm/pyright/bin/pyright-langserver-lsp")))
    :activation-fn (lsp-activate-on "python")
    :server-id 'pyright-manual
    :priority 10
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration 
                         (lsp-configuration-section "python"))))
    :notification-handlers (ht ("pyright/beginProgress" #'ignore)
                               ("pyright/reportProgress" #'ignore)
                               ("pyright/endProgress" #'ignore)))))

;; Ensure LSP starts with both servers
(add-hook 'python-mode-hook 
          (lambda ()
            (require 'lsp-pyright)
            (lsp-deferred)))
(add-hook 'python-ts-mode-hook 
          (lambda ()
            (require 'lsp-pyright)
            (lsp-deferred)))

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
  ;; Always use pandoc with mermaid support
  (let ((filter-path (expand-file-name "mermaid-filter.lua" user-emacs-directory))
        (header-path (expand-file-name "mermaid-header.html" user-emacs-directory)))
    (setq markdown-command 
          (format "pandoc -f markdown -t html -s --mathjax --highlight-style pygments --metadata title=Document --lua-filter=%s --include-in-header=%s"
                  filter-path header-path)))
  ;; fix keybindings for projectile
  (define-key markdown-mode-map (kbd "M-p") nil))

;; Mermaid mode for editing .mmd and .mermaid files
(use-package mermaid-mode
  :mode (("\\.mmd\\'" . mermaid-mode)
         ("\\.mermaid\\'" . mermaid-mode)))

(use-package deft
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
  :config 
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-content-types-alist '(("tsx" . "\\.tsx\\'"))))

(use-package rust-mode
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

;; Ensure TypeScript LSP is configured and force it to start
(with-eval-after-load 'lsp-mode
  (require 'lsp-javascript)
  (setq lsp-disabled-clients '())  ; Ensure no clients are disabled
  ;; Remove restrictive client list to allow all language servers
  ;; (setq lsp-enabled-clients '(ts-ls tailwindcss))  ; Explicitly enable both servers
  ;; Force TypeScript LSP for tsx-ts-mode
  (add-to-list 'lsp-language-id-configuration '(tsx-ts-mode . "typescriptreact"))
  ;; Add python-ts-mode support
  (add-to-list 'lsp-language-id-configuration '(python-ts-mode . "python"))
  ;; Ensure lsp-pyright supports python-ts-mode
  (with-eval-after-load 'lsp-pyright
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection (lambda ()
                                                               (cons (lsp-package-path 'pyright)
                                                                     lsp-pyright-extra-args)))
                      :activation-fn (lsp-activate-on "python" "python-ts")
                      :priority 3
                      :server-id 'pyright
                      :completion-in-comments? t
                      :initialized-fn (lambda (workspace)
                                        (with-lsp-workspace workspace
                                          (lsp--set-configuration (lsp-configuration-section "pyright"))))
                      :download-server-fn (lambda (_client callback error-callback _update?)
                                            (lsp-package-ensure 'pyright callback error-callback))
                      :notification-handlers (lsp-ht ("pyright/beginProgress" 'lsp-pyright--begin-progress-callback)
                                                     ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
                                                     ("pyright/endProgress" 'lsp-pyright--end-progress-callback)))))
  ;; Help LSP find project roots with tsconfig.json
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\cdk\\.out\\'")
  (setq lsp-clients-typescript-prefer-use-project-ts-server t)
  )

;; gptel
(use-package gptel
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model "gpt-4o"))

;; Color cargo output
(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package protobuf-mode)

;; straight.el managed packages

;; Terminal emulator required for claude-code-ide
(use-package vterm
  :config
  (setq vterm-max-scrollback 10000))

;; Claude Code IDE integration
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind (("C-c C-'" . claude-code-ide-menu)  ; Main transient menu
         ("C-c c c" . claude-code-ide)
         ("C-c c r" . claude-code-ide-resume)
         ("C-c c s" . claude-code-ide-stop)
         ("C-c c p" . claude-code-ide-send-prompt)
         ("C-c c t" . claude-code-ide-toggle)
         ("C-c c l" . claude-code-ide-list-sessions)
         ("C-c c e" . claude-code-ide-send-escape))
  :config
  ;; Customize window placement (right side with custom width)
  (setq claude-code-ide-window-side 'right
        claude-code-ide-window-width 100
        claude-code-ide-use-side-window t)
  
  ;; Terminal backend (vterm is already installed)
  (setq claude-code-ide-terminal-backend 'vterm)
  
  ;; Focus behavior
  (setq claude-code-ide-focus-on-open t
        claude-code-ide-focus-claude-after-ediff nil)
  
  ;; Optional: Enable debug logging
  (setq claude-code-ide-debug nil))

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
