;;; init-copilot.el --- Copilot configuration

;;; Commentary:
;; This file configures GitHub Copilot for Emacs

;;; Code:

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (setq copilot-idle-delay 0.5)  ; 기본값은 0이지만, 성능을 위해 약간의 지연을 추가할 수 있습니다.
  
  ;; copilot-login이 자동으로 실행되지 않도록 합니다.
  ;; 사용자가 필요할 때 수동으로 M-x copilot-login을 실행하도록 합니다.
  (defun my-copilot-tab ()
    "Try to do copilot-accept-completion, if not, fallback to default behavior."
    (interactive)
    (or (copilot-accept-completion)
        (indent-for-tab-command)))

  ;; 전역 키 바인딩 설정 (선택사항)
  (global-set-key (kbd "C-c C-p") 'copilot-complete) ; Copilot 완성 수동 트리거
  (global-set-key (kbd "C-c C-n") 'copilot-next-completion) ; 다음 제안으로 이동
  (global-set-key (kbd "C-c C-b") 'copilot-previous-completion)) ; 이전 제안으로 이동

;; Copilot 사용 시 주의사항
(message "Remember to run M-x copilot-login to authenticate with GitHub Copilot")
(message "Copilot requires a valid subscription and Node.js installed on your system")

(provide 'init-copilot)

;;; init-copilot.el ends here
