;;; init-nano.el --- Init looks and feel of Emacs -*- lexical-binding: t -*-
;;; Commentary:
;; - rougier/nano-emacs
;;; Code:

(use-package nano-theme
  :straight (nano :type git :host github :repo "rougier/nano-theme")
  :init
  (setq nano-font-family-monospaced "Monoplex KR Wide Nerd")
  (setq nano-font-family-proportional nil)
  :config
  (nano-dark))

(provide 'init-nano)
;;; init-nano.el ends here
