;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; disable package.el
;; I use straight.el
(setq package-enable-at-startup nil)

;; Remove GUI elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(provide 'early-init)
;;; early-init.el ends here
