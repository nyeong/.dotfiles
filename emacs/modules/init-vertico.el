(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic orderless partial-completion))))
    (orderless-matching-styles '(orderless-flex orderless-literal orderless-regexp)))

(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)))

(use-package marginalia
  :init
  (marginalia-mode))

(provide 'init-vertico)
