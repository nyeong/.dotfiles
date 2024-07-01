;;; init-copilot.el --- Copilot configuration

;;; Commentary:
;; This file configures GitHub Copilot for Emacs

;;; Code:

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook
  (prog-mode . copilot-mode)
  (copilot-mode . (lambda () (setq-local copilot--indent-warning-printed-p t)))
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-accept-completion-by-word)
              ("C-p" . 'copilot-accept-completion-by-word))
  :config
  (setq copilot-idle-delay 0.5)
  (add-to-list 'copilot-indentation-alist
               '(prog-mode . 2)
               '(emacs-lisp-mode . 2)))

(provide 'init-copilot)

;;; init-copilot.el ends here
