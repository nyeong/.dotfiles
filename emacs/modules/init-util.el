;;; init-util.el --- util funs -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

;; Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(provide 'init-util)
;;; init-util.el ends here
