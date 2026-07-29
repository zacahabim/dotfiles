;;; init.el --- Main entry point -*- lexical-binding: t; -*-

;; Load pre-tangled config.el directly, avoiding (require 'org) at startup.
;; Only retangle if config.org is newer than config.el.
(let* ((config-org (expand-file-name "config.org" user-emacs-directory))
       (config-el  (expand-file-name "config.el"  user-emacs-directory)))
  (when (or (not (file-exists-p config-el))
            (file-newer-than-file-p config-org config-el))
    ;; Need to tangle — this only happens when config.org changes.
    (require 'org)
    (org-babel-tangle-file config-org config-el "emacs-lisp"))
  (load config-el nil 'nomessage))
