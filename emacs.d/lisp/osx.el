(add-to-list 'default-frame-alist
             '(font . "-apple-Inconsolata-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1"))
(setq browse-url-generic-program "open")
(setq mac-allow-anti-aliasing t)
(setq mac-option-modifier 'meta)

;; pbcopy - copy to clipboard
(use-package pbcopy
  :ensure t
  :config
  (turn-on-pbcopy))

;; ;; exec-path-from-shell)
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "JAVA_HOME"))
