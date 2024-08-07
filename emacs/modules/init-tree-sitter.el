(use-package tree-sitter
  :hook
  (typescript-mode . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode)
  (add-to-list 'tree-sitter-major-mode-language-alist '(elixir-mode . elixir)))

(use-package tree-sitter-langs
  :after tree-sitter
  :config
  (tree-sitter-require 'typescript))


(defun my/org-src-block-tree-sitter ()
  (when (org-src-get-lang-mode (org-element-property :language (org-element-at-point)))
    (tree-sitter-mode 1)))

(add-hook 'org-src-mode-hook #'my/org-src-block-tree-sitter)


;; (add-to-list 'tree-sitter-major-mode-language-alist '(js-mode . javascript))

(provide 'init-tree-sitter)
