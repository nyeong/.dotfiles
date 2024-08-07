;;; init-base.el --- base configs -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:
(defalias 'yes-or-no-p 'y-or-n-p)
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8
     	coding-system-for-read 'utf-8
      default-process-coding-system '(utf-8-unix . utf-8-unix)
	    coding-system-for-write 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


(provide 'init-base)
;;; init-base.el ends here
