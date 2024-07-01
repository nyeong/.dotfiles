;; set-edit.el -- 편집과 관련된 기본적인 설정들
;; UTF8 쓰기
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; yes/no 대신 y/n 쓰기
(fset 'yes-or-no-p 'y-or-n-p)

;; show all messages
(setq inhibit-message nil)

;; 버그 있을 수 있대서 끔
(setq enable-recursive-minibuffers nil)

;; 심볼릭 링크 따라가기 끄기. 버그 있을 수도 있대서 끔.
(setq find-file-visit-truename nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(provide 'set-edit)
