;; Set up clipboard
;; (defun copy-from-osx ()
;; (shell-command-to-string "pbpaste"))
;; 
;; (defun paste-to-osx (text &optional push)
;; (let ((process-connection-type nil))
;; (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
;; (process-send-string proc text)
;; (process-send-eof proc))))
;; 
;; (setq interprogram-cut-function 'paste-to-osx)
;; (setq interprogram-paste-function 'copy-from-osx)

;; Load theme
(load-theme 'tango-dark t)

;; custom *.el scripts
;; add your modules path
(add-to-list 'load-path "~/.emacs.d/custom/")

;; load module
(require 'redo+)
;; normal redo
(define-key global-map (kbd "C-/") 'undo)
(define-key global-map (kbd "C-x /") 'redo)

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Set this to save the clipboard content before killing
;; Thus, enable C-y M-y to copy things from clipboard after killing something
(setq save-interprogram-paste-before-kill t)

;; Set fill column guide
(global-display-fill-column-indicator-mode 1)

;; Package sources
(require 'package)                   ; Bring in to the environment all package management functions
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://mirrors.163.com/elpa/gnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))

(setq use-package-always-ensure t)

;; Benchmark startup
(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(add-hook 'after-init-hook
          (lambda () (message "loaded in %s" (emacs-init-time))))


;; Increase the garbage collection threshold
(setq gc-cons-threshold 10000000)

;; Restore after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 1000000)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

;; ================================================================================
;; Basic configurations
;; ================================================================================
;; Add function to edit this file
(defun find-config ()
  "Edit config.org"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-c I") 'find-config)

;; Move around a bit faster
(global-set-key (kbd "M-n") (kbd "C-u 10 C-n"))
(global-set-key (kbd "M-p") (kbd "C-u 10 C-p"))

;; line wrap
(global-visual-line-mode 1)

;; Disable window decoration
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Disable bell ringing
(setq ring-bell-function 'ignore)

;; prompt
(defalias 'yes-or-no-p 'y-or-n-p)

;; confirm before exit
(setq confirm-kill-emacs 'y-or-n-p)

;; default window move
;; shift+arrow-keys to move between windows
(windmove-default-keybindings 'shift)

;; Custom settings go to its own file
;; This does not work properly when trying to save font from the emacs menu
;;(setq custom-file (make-temp-file "emacs-custom"))

;; Load custom lisp files
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Centralize backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; match parens
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key "%" 'match-paren)

;; No tab indentation. If I just want one tab then use C-q (quoted-insert) to insert as a literal.
(setq-default indent-tabs-mode nil)

;; set mark ring
;; Popping the mark ring with C-u C-SPC C-SPC ...
(setq set-mark-command-repeat-pop t)
(setq mark-ring-max 48)

;; show matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; ================================================================================
;; Custom packages
;; ================================================================================
;; fuzzy search
(use-package fzf
  :bind (("C-c g f" . fzf-git)
         ("C-c f f" . fzf)))

;; rg & projectile to search project
(use-package deadgrep
  :bind (("C-c g g" . deadgrep)))

;; Record key frequency
;; Check with keyfreq-show
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; visualize undo tree
  (use-package undo-tree
    :defer 5
    :config
    (global-undo-tree-mode 1))

;; Git
(use-package magit
  :bind ("C-x g" . magit-status))

;; Ivy
(use-package ivy
    :config
    (ivy-mode t))

;; no need to type ^ before search
(setq ivy-initial-inputs-alist nil)

;; counsel: collection of ivy enhancement
(use-package counsel
  :bind (("M-x" . counsel-M-x)))

;; sort and filter candidate for ivy/counsel
(use-package prescient)
(use-package ivy-prescient
  :config
  (ivy-prescient-mode t))

;; present menus for ivy
(use-package ivy-hydra)

;; suggest next keys
(use-package which-key
  :config
  (add-hook 'after-init-hook 'which-key-mode)
  :bind
  ;; Disable C-h C-h to do help-for-help
  ;; help-for-help can be done with C-h ?
  ;; This also enable paging in C-h menu
  ("C-h C-h" . nil))

;; highlight parens
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; ================================================================================

;; No help screen
(defun copy-from-osx ()
(shell-command-to-string "pbpaste"))


;; Go to last change
(use-package goto-last-change
  :bind (("C-;" . goto-last-change)))


;; expand the selection of the region based on mode
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; highlight strings with colors
(use-package rainbow-mode
  :config
  (setq rainbow-x-colors nil)
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; jump to source with simple search
  (use-package dumb-jump
    :bind (("C-M-g" . dumb-jump-go)
           ("C-M-p" . dumb-jump-back)
           ("C-M-q" . dumb-jump-quick-look)))

;; Display line changes
(use-package git-gutter
  :config
  (global-git-gutter-mode 't))

;; auto-complete
  (use-package eglot
    :commands eglot
    :config
    (add-to-list 'eglot-server-programs '(elm-mode . ("elm-language-server" "--stdio"))))

;; snippets
(use-package yasnippet
    :config
    (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
    (yas-global-mode 1))

;; pre-made snippets
(use-package yasnippet-snippets)

;; Extra
;; (use-package writegood-mode
;;   :bind ("C-c g" . writegood-mode)
;;   :config
;;   (add-to-list 'writegood-weasel-words "actionable"))

;; Custome stuffs from the environment
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet-snippets writegood-mode which-key use-package undo-tree smartparens rainbow-mode rainbow-delimiters magit keyfreq ivy-prescient ivy-hydra goto-last-change git-gutter fzf expand-region eglot dumb-jump deadgrep counsel benchmark-init))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 120 :width normal)))))
