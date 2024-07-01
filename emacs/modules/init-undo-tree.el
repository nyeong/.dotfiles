(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo-tree-hist"))))
  (global-undo-tree-mode 1))

(provide 'init-undo-tree)
