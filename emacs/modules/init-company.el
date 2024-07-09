;;; init-company.el
;;; Commentary:
;;; Code:
(use-package company-mode
  :init
  (global-company-mode)
  :config
  (setq company-idel-delay 0
        company-show-numbers "on"))

(provide 'init-company)
;;; init-company.el ends here
