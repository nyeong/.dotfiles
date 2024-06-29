;;; init-lsp.el --- LSP configuration

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(provide 'init-lsp)
;;; init-lsp.el ends here
