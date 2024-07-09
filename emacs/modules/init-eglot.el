;;; init-eglot.el --- for lsp client
;;; Commentary:
;;; Code:

(use-package consult-eglot)
(use-package consult-eglot-embark)

(with-eval-after-load 'embark
  (with-eval-after-load 'consult-eglot
    (require 'consult-eglot-embark)
    (consult-eglot-embark-mode)))

(provide 'init-eglot)
;;; init-eglot.el ends here
