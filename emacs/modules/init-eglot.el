;;; init-eglot.el --- for lsp client
;;; Commentary:
;;; Code:

(use-package eglot
  :config
  (setq eglot-autoshudown t
        eglot-extend-to-xref t))

(setq completion-category-defaults nil)
(setq completion-category-overrides '((eglot (styles orderless)))) 

(provide 'init-eglot)
;;; init-eglot.el ends here
