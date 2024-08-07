;;; init-javascript.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'eglot)

(use-package js2-mode
  :mode "\\.js\\'"
  :init
  :hook (js2-mode . lsp-deferred))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . corfu-mode)
        (typescript-mode . lsp-deferred)
        (typescript-mode . rainbow-delimiters-mode)
  :config
  (setq typescript-indent-level 2))

(provide 'init-javascript)
;;; init-javascript.el ends here
