;;; init-projectile.el --- Projectile configuration

;;; Commentary:
;; This file configures Projectile for project management in Emacs

;;; Code:

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-completion-system 'vertico)  ; 'default를 사용중인 completion system으로 변경 가능 (예: 'vertico)
  (setq projectile-indexing-method 'alien)
  (setq projectile-sort-order 'recentf)
  (setq projectile-enable-caching t)
  
  ;; 프로젝트 타입별 무시할 파일/디렉토리 설정
  (setq projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS"))
  (setq projectile-globally-ignored-directories '(".idea" ".vscode" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work"))

  ;; 프로젝트 검색 경로 설정 (선택사항)
  ;; (setq projectile-project-search-path '("~/Projects/" "~/Work/"))

  ;; known projects에 자동으로 추가 (선택사항)
  (setq projectile-track-known-projects-automatically nil)

  ;; 프로젝트 별 compile command 설정 (선택사항)
  ;; (setq projectile-project-compilation-cmd '((js-mode . "npm run build")
  ;;                                            (web-mode . "npm run build")
  ;;                                            (python-mode . "python setup.py build")))
  )

;; Projectile와 관련된 추가 패키지 설정 (선택사항)

;; counsel-projectile (if using Ivy)
;; (use-package counsel-projectile
;;   :ensure t
;;   :config
;;   (counsel-projectile-mode))

;; projectile-rails (if working with Ruby on Rails projects)
;; (use-package projectile-rails
;;   :ensure t
;;   :config
;;   (projectile-rails-global-mode))

(provide 'init-projectile)

;;; init-projectile.el ends here
