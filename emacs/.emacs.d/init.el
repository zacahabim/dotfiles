;; Bootstrap: tangle and load config.org
(require 'org)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
