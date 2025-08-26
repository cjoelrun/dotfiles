(add-to-list 'default-frame-alist
             '(font . "-apple-Inconsolata-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1"))
(setq browse-url-generic-program "open")
(setq mac-allow-anti-aliasing t)
(setq mac-option-modifier 'meta)

;; macOS clipboard integration
;; Use the built-in macOS clipboard support
(setq select-enable-clipboard t)
(setq select-enable-primary t)

;; Alternative: pbcopy package (using straight.el)
(use-package pbcopy
  :straight t
  :config
  (turn-on-pbcopy))

;; ;; exec-path-from-shell)
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "JAVA_HOME"))
