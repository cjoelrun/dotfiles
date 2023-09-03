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
  :hook (
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-ui
  :commands lsp-ui-mode)

;; python
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package git-gutter)

(use-package quelpa-use-package)
(use-package copilot
  :quelpa (copilot :fetcher github :repo "zerolfx/copilot.el"
                   :files ("*.el" "dist"))
  :init
  ;; accept completion from copilot and fallback to company
  (defun my-tab ()
    (interactive)
    (or (copilot-accept-completion)
        (corfu-complete)))

  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-mode-map
              ("<tab>" . 'copilot-accept-completion)
              ))

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
