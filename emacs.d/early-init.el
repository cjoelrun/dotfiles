;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Increase gc threshold during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Reset gc threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1024 1024)
                  gc-cons-percentage 0.1)))

;;; early-init.el ends here