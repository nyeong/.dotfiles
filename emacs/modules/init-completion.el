;;; init-completion.el --- completion! -*- lexical-binding: t -*-

;;; Commentary:
;; - orderless : 유연한 완성
;; - vertico : 미니버퍼 자동완성
;; - corfu : 버퍼 자동완성
;; - marginalia : 미니버퍼 자동완성 추가 후보

;;; Code:

(setq completion-cycle-threshold 3
      tab-always-indent 'complete)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package vertico
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package nerd-icons-completion
  :after (vertico)
  :hook (vertico-mode . nerd-icons-completion-mode))

(use-package nerd-icons-corfu
  :after (corfu)
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-preview-current nil)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind ("M-/" . completion-at-point)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)))

(unless (display-graphic-p)
  (use-package corfu-terminal
    :hook (global-corfu-mode . corfu-terminal-mode)))

(provide 'init-completion)
;;; init-completion.el ends here
