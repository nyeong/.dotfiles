;;; init-vertico.el --- for Completion
;;; Commentary:
;; - vertico
;; - consult, for useful search and navigation
;; - marginalia, for rich annotations in the minibuffer
;; - embark, for minibuffer actions and context menu
;; - orderless, for advanced completion style
;;; Code:
(use-package vertico
  :init
  (vertico-mode 1))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
(use-package consult)
(use-package consult-eglot
  :after (consult))
(use-package embark)
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
(use-package consult-eglot-embark
  :after (consult eglot embark))
(use-package marginalia
  :config
  (marginalia-mode))

(provide 'init-vertico)
;;; init-vertico.el ends here
