;;; init-company.el
;;; Commentary:
;;; Code:

;; (use-package company
;;   :hook (after-init . global-company-mode)
;;   :custom
;;   (company-idle-delay 0)
;;   (company-show-numbers t)
;;   (company-minimum-prefix-length 1)
;;   (company-dabbrev-ignore-case t)
;;   (completion-ignore-case t)
;;   :config
;;   (setq company-backends '((company-capf
;;                             company-dabbrev-code
;;                             company-keywords
;;                             company-files
;;                             company-dabbrev))))
(use-package company
  :hook (after-init . global-company))


(provide 'init-company)
;;; init-company.el ends here
