(add-to-list 'default-frame-alist
             '(font . "-apple-Inconsolata-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1"))
(setq browse-url-generic-program "open")
(setq mac-allow-anti-aliasing t)
(setq mac-option-modifier 'meta)

;; pbcopy - copy to clipboard
(add-to-list 'load-path "~/.emacs.d/external/pbcopy")
(require 'pbcopy)
(turn-on-pbcopy)

;; exec-path-from-shell)
(add-to-list 'load-path "~/.emacs.d/external/exec-path-from-shell")
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
