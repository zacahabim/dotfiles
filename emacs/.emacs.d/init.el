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

;; Set the color scheme for the terminal. Zenburn
(custom-set-faces
 '(term-color-black ((t (:foreground "#3F3F3F" :background "#2B2B2B"))))
 '(term-color-red ((t (:foreground "#AC7373" :background "#8C5353"))))
 '(term-color-green ((t (:foreground "#7F9F7F" :background "#9FC59F"))))
 '(term-color-yellow ((t (:foreground "#DFAF8F" :background "#9FC59F"))))
 '(term-color-blue ((t (:foreground "#7CB8BB" :background "#4C7073"))))
 '(term-color-magenta ((t (:foreground "#DC8CC3" :background "#CC9393"))))
 '(term-color-cyan ((t (:foreground "#93E0E3" :background "#8CD0D3"))))
 '(term-color-white ((t (:foreground "#DCDCCC" :background "#656555"))))
 '(term-default-fg-color ((t (:inherit term-color-white))))
 '(term-default-bg-color ((t (:inherit term-color-black))))
 )

;; custom *.el scripts
;; add your modules path
(add-to-list 'load-path "~/.emacs.d/custom/")

;; load module
(require 'redo+)
;; normal redo
(define-key global-map (kbd "C-/") 'undo)
(define-key global-map (kbd "C-_") 'undo)
(define-key global-map (kbd "C-x C-/") 'redo)
;; In terminal, C-x C-/ is interpreted as C-x C-_ Dont' know why yet.
(define-key global-map (kbd "C-x C-_") 'redo)

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Set this to save the clipboard content before killing
;; Thus, enable C-y M-y to copy things from clipboard after killing something
(setq save-interprogram-paste-before-kill t)

;; Set fill column guide to programming mode
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; Set initial scratch text empty
(setq initial-scratch-message "")

;; disable splash screen
(setq inhibit-splash-screen t)

;; enable transient mode
(transient-mark-mode 1)

;; delete trailing whitespace
;; (add-hook 'before-save-hook 'my-prog-nuke-trailing-whitespace)

;; (defun my-prog-nuke-trailing-whitespace ()
;;   (when (derived-mode-p 'prog-mode)
;;     (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)


(defun sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

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

;; personal org
;; TODO: Add argument to this function
(defun find-note ()
  "Edit personal note"
  (interactive)
  (find-file "~/prog/org/notes.org"))

(global-set-key (kbd "C-c N") 'find-note)


;; Move around a bit faster
(global-set-key (kbd "M-n") (kbd "C-u 10 C-n"))
(global-set-key (kbd "M-p") (kbd "C-u 10 C-p"))

;; line wrap
(global-visual-line-mode 1)

;; Disable window decoration
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; enable winner mode (undo, redo window configuration)
;; this enable zooming with C-x 1 then back with C-c left
(winner-mode 1)

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

;; Often used commands
(global-set-key (kbd "C-c b r") 'revert-buffer)

;; No tab indentation. If I just want one tab then use C-q (quoted-insert) to insert as a literal.
(setq-default indent-tabs-mode nil)

;; set mark ring
;; Popping the mark ring with C-u C-SPC C-SPC ...
(setq set-mark-command-repeat-pop t)
(setq mark-ring-max 32)

;; show matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; ================================================================================
;; Custom packages
;; ================================================================================
;; fuzzy search
;; (use-package fzf
;;   :bind
;;   ("C-c g f" . fzf-git)
;;   ("C-c f f" . fzf)
;; )

;; rg & projectile to search project
(use-package deadgrep
  :config
  (defun pp/deadgrep-view-file ()
  "View result under cursor in other window."
  (interactive)
  (deadgrep-visit-result-other-window)
  (other-window 1))
  :bind
  ("C-c d g" . deadgrep)
  (:map deadgrep-mode-map
              ("C-o" . pp/deadgrep-view-file))
)

;; Record key frequency
;; Check with keyfreq-show
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package xclip
  :config
  (xclip-mode 1))

;; visualize undo tree
  (use-package undo-tree
    :defer 5
    :config
    (global-undo-tree-mode 1))

;; Git
(use-package magit)

;; Ivy
(use-package ivy
    :config
    (ivy-mode t))

;; no need to type ^ before search
(setq ivy-initial-inputs-alist nil)

;; counsel: collection of ivy enhancement
(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("C-c f f" . counsel-locate)
  ("C-c g f" . counsel-git)
  ("C-c g g" . vc-git-grep)
)

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

;; Change frame composition
(use-package transpose-frame)

;; Autocomplete
(use-package auto-complete
  :config
  (ac-config-default)
)

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

;; ;; snippets
;; (use-package yasnippet
;;     :config
;;     (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
;;     (yas-global-mode 1))

;; pre-made snippets
;; (use-package yasnippet-snippets)
;;
;; Extra
;; (use-package writegood-mode
;;   :bind ("C-c g" . writegood-mode)
;;   :config
;;   (add-to-list 'writegood-weasel-words "actionable"))

;; ================================================================================
;; Language support

(use-package groovy-mode)
(use-package yaml-mode)
(use-package markdown-mode)

(require 'ttcn3)
(add-to-list 'auto-mode-alist '("\\.ttcn\\'" . ttcn-3-mode))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;; auto-complete
  (use-package eglot
    :commands eglot
    :config
    (add-to-list 'eglot-server-programs '(elm-mode . ("elm-language-server" "--stdio"))))
;; ================================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(elpy xclip yasnippet-snippets yaml-mode writegood-mode which-key use-package undo-tree transpose-frame smartparens rainbow-mode rainbow-delimiters markdown-mode magit keyfreq ivy-prescient ivy-hydra groovy-mode goto-last-change git-gutter fzf expand-region eglot dumb-jump deadgrep counsel benchmark-init auto-complete))
 '(safe-local-variable-values
   '((eval let
           ((pwd
             (file-truename default-directory)))
           (while
               (not
                (file-exists-p
                 (concat pwd dir-locals-file)))
             (setq pwd
                   (file-name-directory
                    (directory-file-name pwd))))
           (let
               ((tags-file
                 (concat pwd "out/TAGS."
                         (car
                          (split-string
                           (symbol-name major-mode)
                           "-")))))
             (when
                 (file-exists-p tags-file)
               (set
                (make-local-variable 'tags-file-name)
                tags-file))))))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 100 :family "Go Mono")))))
