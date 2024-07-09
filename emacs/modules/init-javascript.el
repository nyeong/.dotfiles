;;; init-javascript.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package js2-mode)
(use-package typescript-mode
  :hook (typescript-mode . rainbow-delimiters-mode)
  :hook (typescript-tsx-mode . rainbow-delimiters-mode))
(provide 'init-javascript)
;;; init-javascript.el ends here
