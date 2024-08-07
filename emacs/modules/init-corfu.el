;;; init-corfu.el --- code completion -*- lexical-binding: t -*-

;;; Commentary: 
;; 

;;; Code:

(use-package corfu
      :ensure t
      ;; Optional customizations
      :custom
      (corfu-cycle t)                 ; Allows cycling through candidates
      (corfu-auto t)                  ; Enable auto completion
      (corfu-auto-prefix 2)           ; Minimum length of prefix for completion
      (corfu-auto-delay 0)            ; No delay for completion
      (corfu-popupinfo-delay '(0.5 . 0.2))  ; Automatically update info popup after that numver of seconds
      (corfu-preview-current 'insert) ; insert previewed candidate
      (corfu-preselect 'prompt)
      (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
      ;; Optionally use TAB for cycling, default is `corfu-complete'.
      :bind (:map corfu-map
                  ("M-SPC"      . corfu-insert-separator)
                  ("TAB"        . corfu-next)
                  ([tab]        . corfu-next)
                  ("S-TAB"      . corfu-previous)
                  ([backtab]    . corfu-previous)
                  ("S-<return>" . corfu-insert)
                  ("RET"        . corfu-insert))

      :init
      (global-corfu-mode)
      (corfu-history-mode)
      (corfu-popupinfo-mode) ; Popup completion info
      :config
      (add-hook 'eshell-mode-hook
                (lambda () (setq-local corfu-quit-at-boundary t
                                       corfu-quit-no-match t
                                       corfu-auto nil)
                  (corfu-mode))
                nil
                t))

(use-package emacs
  :custom
  (tab-always-indent 'compelete)
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package cape
  :straight t
  :init
  ;; 기본 capfs를 추가합니다
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; 프로그래밍 모드에 대해 주석과 문자열 완성을 허용
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-keyword)
              (add-to-list 'completion-at-point-functions #'cape-abbrev))))

(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; 기본 corfu 아이콘을 corfu-default face로 설정
  :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(provide 'init-corfu)
;;; init-corfu.el ends here
