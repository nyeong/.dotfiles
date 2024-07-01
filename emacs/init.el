;; * References
;;   - https://github.com/purcell/emacs.d
;;   - https://github.com/SystemCrafters/crafted-emacs/tree/master
;;   - https://git.sr.ht/~ashton314/emacs-bedrock
;;   - https://github.com/leotaku/literate-emacs/tree/master
;;   - https://github.com/bbatsov/prelude?tab=readme-ov-file
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq create-lockfiles nil)

(require 'helpers)

;; package
(require 'init-packages)

;; edit
(require 'set-edit)  ;; 편집기의 기본적인 설정들
(require 'init-meow) ;; meow 키맵 켜기
(require 'init-undo-tree)

;; ui
(require 'set-ui)
(require 'init-which-key)
(require 'init-dashboard)
(require 'init-treemacs)
(require 'init-minimap)
(require 'init-modeline)
(require 'init-theme)

;; completion
(require 'init-vertico)
(require 'init-company)

;; tools
(require 'init-lsp)
(require 'init-projectile)
(require 'init-copilot)
(require 'init-magit)

;; lang
(require 'lang-typescript)
