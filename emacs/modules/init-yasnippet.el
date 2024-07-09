;;; init-yasnippet.el -*- lexical-binding: t; -*-
(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs
	'("~/.config/emacs/snippets")))

(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git :host github :repo "doomemacs/snippets" :files ("*.el" "*")))

(provide 'init-yasnippet)
