;;; init-dashboard.el --- init dashboard -*- lexical-binding: t -*-

;;; Commentary: 
;;

;;; Code:

(use-package dashboard
  :demand
  :init
  (setq dashboard-items '((recents   . 5)
                        (bookmarks . 5)
                        (projects  . 5)
                        (agenda    . 5)
                        (registers . 5))
        dashboard-center-content t
        dashboard-vertically-center-content t)
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))



(provide 'init-dashboard)
;;; init-dashboard.el ends here
