;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Tuning
(setq gc-cons-threshold (* 128 1024 1024))
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

;;; Bootstrap:
;; load .el from modules
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-elpa)

(defun nyeong/enable-line-numbers ()
  (interactive)
  (display-line-numbers-mode)
  (setq display-line-numbers 'relative))

;; Set up for GUI
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)
(use-package emacs
  :init
  (setq initial-scratch-message nil)
  (defun display-startup-echo-area-message () (message ""))
  (defalias 'yes-or-no-p 'y-or-n-p)
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
	coding-system-for-read 'utf-8
	coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'super
	  mac-option-modifier 'meta
	  mac-control-modifier 'control))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-apperance . light))
  (setq ns-use-proxy-icon nil
        frame-title-format nil
        backup-directory-alist `(("." . "~/.saves")))
  (add-hook 'prog-mode-hook #'nyeong/enable-line-numbers)
  )

;; completion
(require 'init-company) ;; as a code completion
(require 'init-vertico) ;; as a completion

;; ui
(use-package rainbow-delimiters)
(require 'init-font) ;; set fonts
;; (require 'init-nano) ;; as a theme
(require 'init-doom-themes) ;; as a theme
(require 'init-treemacs) ;; as a side bar
(require 'init-doom-modeline) ;; as a mode line

;; edit
(require 'init-general) ;; as a key binding definer
(require 'init-meow) ;; as a modal edit system
(require 'init-yasnippet) ;; as a snippets
;; editorconfig

;; tools
;; lsp
(require 'init-eglot) ;; as a lsp client
;; tree-sitter
;; magit
;; lookup
;; eval overlay

;; lang
(require 'init-javascript) ;; and typescript
;; elixir
;; emacs-lisp
;; json
;; org-mode with roam

(provide 'init)
;;; init.el ends here

