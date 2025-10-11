;;; ../../.dotfiles/packages/emacs/config/doom/custom.el -*- lexical-binding: t; -*-


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval let*
      ((project-root
        (locate-dominating-file (or load-file-name buffer-file-name)
                                ".dir-locals.el"))
       (lisp-dir (expand-file-name "lisp/" project-root)))
      (when (and project-root (file-directory-p lisp-dir))
       (mapc #'load-file (directory-files lisp-dir t "\\.el$"))))
     (eval let*
      ((dir-locals-dir
        (file-name-directory (or load-file-name buffer-file-name)))
       (lisp-dir (expand-file-name "lisp/" dir-locals-dir)))
      (when (file-directory-p lisp-dir)
        (mapc #'load-file (directory-files lisp-dir t "\\.el$"))))
     (eval progn
      (add-to-list 'load-path
                   (expand-file-name "lisp"
                                     (locate-dominating-file default-directory
                                                             ".dir-locals.el"))))
     (eval add-hook 'org-mode-hook (lambda nil (org-babel-execute-buffer)) nil t)
     (eval add-hook 'after-save-hook (lambda nil (org-babel-execute-buffer)) nil
      t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
