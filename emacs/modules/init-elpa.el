;;; init-elpa.el --- Initialize ELPA package manager -*- lexical-binding: t -*-
;;; Commentary:
;; - straight.el
;; - use-package
;;; Code:

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use use-package
(straight-use-package 'use-package)

(setq straight-use-package-by-default t ;; use use-package with straight
      use-package-always-defer t) ;; lazy loading

(provide 'init-elpa)
;;; init-elpa.el ends here
