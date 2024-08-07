;;; init-elixir.el --- setup for elixir -*- lexical-binding: t -*-

;;; Commentary: 
;; 

;;; Code:
(require 'eglot)
(use-package elixir-mode
  :hook
  (elixir-mode . eglot-ensure)
  (before-save . eglot-format)
  :init
  (add-to-list 'eglot-server-programs '(elixir-mode "~/.local/share/elixir-ls/release/language_server.sh")))

(use-package exunit
  :hook
  (elixir-mode-hook . exuni-mode))

(use-package inf-elixir)


(provide 'init-elixir)
;;; init-elixir.el ends here
