;;; init-projectile.el --- Projectile configuration

;;; Commentary:
;; This file configures Projectile for project management in Emacs

;;; Code:

(use-package projectile
  :ensure t
  :after vertico
  :init
  (projectile-mode +1)
  (when (fboundp 'meow-leader-define-key)
    (meow-leader-define-key
      '("p" . projectile-command-map)))
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (when (fboundp 'vertico-mode)
  (setq projectile-completion-system 'vertico))
  (setq projectile-indexing-method 'alien)
  (setq projectile-sort-order 'recentf)
  (setq projectile-enable-caching t)
  
  ;; 프로젝트 타입별 무시할 파일/디렉토리 설정
  (setq projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS"))
  (setq projectile-globally-ignored-directories '(".idea" ".vscode" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work"))
  (setq projectile-track-known-projects-automatically nil)
  )

(provide 'init-projectile)

;;; init-projectile.el ends here
