(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq use-package-always-ensure t
      use-package-expand-minimally t)

(unless (package-installed-p 'use-package)
  ;; (package-refresh-contents)
  (package-install 'use-package))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package counsel
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (ivy-mode 1))

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))

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
  (setq projectile-project-search-path '("~/headspace/" "~/work/" "~/work/womoji" ))
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
  :hook ((python-mode . dap-ui-mode) (python-mode . dap-mode))
  :config
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (defun dap-python--pyenv-executable-find (command)
    (with-venv (executable-find "python")))

  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))

;; python
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package quelpa-use-package)
(use-package copilot
  :diminish
  :quelpa (copilot :fetcher github :repo "zerolfx/copilot.el"
                   :files ("*.el" "dist"))
  :init
  (defun my-copilot-complete ()
    (interactive)
    (or (copilot-accept-completion)
        (move-end-of-line nil)))
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-mode-map
              ("C-e" . 'my-copilot-complete)))

(use-package markdown-mode
  :config
  (setq markdown-command "pandoc -f markdown -t html -s --mathjax --highlight-style pygments")
  ;; fix keybindings for projectile
  (define-key markdown-mode-map (kbd "M-p") nil))

(use-package deft
  :ensure
  :config
  (setq deft-directory "/Users/cameronlopez/Library/Mobile Documents/iCloud~md~obsidian/Documents/cameron")
  (setq deft-text-mode 'markdown-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-auto-save-interval 10.0)
  (setq deft-recursive t)
  (setq deft-default-extension "md")
  ;; ignore history directory in headspace dir
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
  :hook
  (rust-mode . lsp-deferred)
  :config
  (setq rust-format-on-save t))

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
  (setq gptel-default-mode 'org-mode))
