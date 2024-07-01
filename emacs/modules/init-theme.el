(use-package doom-themes
  :ensure t
  :hook (doom-load-theme . doom-themes-org-config)
  :init
  (setq doom-theme 'doom-one)
  (load-theme 'doom-one t))
(provide 'init-theme)
