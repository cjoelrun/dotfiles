(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package projectile
  :config
  (setq projectile-project-search-path '("~/headspace/" "~/work/" ))
  (projectile-discover-projects-in-search-path)
  :bind
  ("C-c pp" . projectile-switch-project))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp)
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ("C-c r" . lsp-ui-peek-find-references)
              ("M-." . lsp-ui-peek-find-definitions)
              ("C-c i" . lsp-ui-peek-find-implementation)
              ("C-c d" . lsp-ui-doc-glance)
              ("C-c m" . lsp-ui-imenu)))

;; python
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))  ; or lsp-deferred

(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package git-gutter)

(use-package quelpa-use-package)
(use-package copilot
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
  (setq markdown-command "pandoc -f markdown -t html -s --mathjax --highlight-style pygments"))

(use-package deft
  :config
  (setq deft-directory "/Users/cameronlopez/Library/Mobile Documents/iCloud~md~obsidian/Documents/cameron")
  (setq deft-extension "md")
  (setq deft-text-mode 'markdown-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-auto-save-interval 10.0)
  (setq deft-recursive t)
  ;; ignore history directory in headspace dir
  (setq deft-recursive-ignore-dir-regexp "\\(?:\\.\\|\\.\\.\\|history\\)\\'")
)

(use-package web-mode
  ;; include tsx ts
  :hook
  (web-mode . lsp-deferred)
  :mode (("\\.tsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.scss\\'" . web-mode)
         ("\\.json\\'" . web-mode)
         ("\\.graphql\\'" . web-mode)
         ("\\.gql\\'" . web-mode)
         ("\\.md\\'" . web-mode)
         ("\\.mdx\\'" . web-mode)
         ("\\.yaml\\'" . web-mode)
         ("\\.yml\\'" . web-mode)
         ("\\.xml\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.twig\\'" . web-mode)
         ("\\.vue\\'" . web-mode)))

(use-package avy
  :bind
  ("M-J" . avy-goto-char-2))

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-operandi t))
