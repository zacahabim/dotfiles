;;; early-init.el --- Pre-initialization for fast startup -*- lexical-binding: t; -*-

;; Set Garbage Collection threshold extremely high (1 GB) during startup.
;; Inspired by https://github.com/stapelberg/configfiles
;; This prevents GC pauses while loading packages.
(setq gc-cons-threshold (* 1024 1024 1024))  ; 1 GB

;; Prevent package.el from initializing before use-package takes over.
;; We call (package-initialize) explicitly in config.
(setq package-enable-at-startup nil)

;; Use package quickstart to avoid scanning load-path at startup.
(setq package-quickstart t)

;; Suppress UI elements before the frame is drawn (avoids flicker).
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)

;; Don't resize frame at startup (avoids expensive redraws).
(setq frame-inhibit-implied-resize t)

;; Disable file-name-handler-alist during startup (speeds up file loads).
(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore file-name-handler-alist after init.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my--file-name-handler-alist)))
