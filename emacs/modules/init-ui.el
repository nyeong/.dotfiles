;;; init-ui.el --- visual! -*- lexical-binding: t -*-

;;; Commentary:
;; - set fonts
;; - doom-modeline
;; - nerd-icons

;;; Code:

(set-face-attribute 'default nil :family "Monoplex KR Wide Nerd")

(set-face-attribute 'default nil :height 128)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init (setq doom-modeline-minor-modes t))

(use-package nerd-icons
  :config (when (and (display-graphic-p)
                     (not (font-installed-p nerd-icons-font-family)))
            (nerd-icons-install-fonts t)))

;; GUI 끄기
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t
      inhibit-splash-screen t
      inhibit-default-init t
      initial-scratch-message nil
      use-file-dialog nil
      use-dialog-box nil)

;; macOS
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-apperance . light))
  (setq ns-use-proxy-icon nil
        frame-title-format nil
        backup-directory-alist `(("." . "~/.saves")))

(provide 'init-ui)
;;; init-ui.el ends here
