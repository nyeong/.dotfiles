;;; init-doom-themes.el --- Themes from doom emacs -*- lexical-binding: t -*-
;;; Comentary:
;;; Code:

(use-package doom-themes
  :demand
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))
(provide 'init-doom-themes)
