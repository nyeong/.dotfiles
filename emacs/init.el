;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Speed Tuning
(setq gc-cons-threshold (* 128 1024 1024) ;; defer gc further back
      read-process-output-max (* 4 1024 1024)
      process-adaptive-read-buffering nil)

;;; Bootstrap:
;; load .el from modules
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'init-util)
(require 'init-elpa)
(require 'init-base)
(require 'init-edit)

(defun nyeong/enable-line-numbers ()
  (interactive)
  (display-line-numbers-mode)
  (setq display-line-numbers 'relative))

;; Set up for GUI
(use-package emacs
  :init
  (electric-pair-mode 1)
  (defun display-startup-echo-area-message () (message ""))
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq meta-prefix-char nil)
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'super
	  mac-option-modifier 'meta
	  mac-control-modifier 'control))
  (global-display-line-numbers-mode t)
  (setq display-line-numbers 'relative)
  (setq display-time-format "[%Y-%m-%d %H:%M]")
  (display-time-mode t))

(require 'init-ui)
(require 'init-completion)
(use-package rainbow-delimiters)
;; (require 'init-nano) ;; as a theme
(require 'init-doom-themes) ;; as a theme
(require 'init-treemacs) ;; as a side bar
(require 'init-dashboard)

;; edit
(require 'init-autoinsert) ;; template manager
(require 'init-korean)
(require 'init-general) ;; as a key binding definer
(require 'init-meow) ;; as a modal edit system
;; (require 'init-yasnippet) ;; as a snippets
;; editorconfig

;; tools
;; lsp
(require 'init-lsp)
(require 'init-org-mode) ;; as a note taking tool
(require 'init-tree-sitter)
;; magit
;; lookup
;; eval overlay

;; lang
(require 'init-javascript) ;; and typescript
(require 'init-elixir)
;; emacs-lisp
;; json
;; org-mode with roam

(provide 'init)
;;; init.el ends here

