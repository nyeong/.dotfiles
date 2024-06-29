;; * References
;;   - https://github.com/purcell/emacs.d
;;   - https://github.com/SystemCrafters/crafted-emacs/tree/master
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; package
(require 'init-packages)

;; ui
(require 'init-dashboard)

;; tools
(require 'init-lsp)
(require 'init-projectile)
(require 'init-copilot)

;; edit
(require 'init-vertico)
(require 'init-meow)

;; lang


;; magit
;; tree-sitter
;; lsp
;; lookup
