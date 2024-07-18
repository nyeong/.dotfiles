(use-package which-key
  :demand
  :init
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode))

(use-package general
  :demand
  :config
  (setq global-leader-map
        (let ((keymap (make-sparse-keymap)))
          (define-key keymap (kbd "SPC") 'meow-keypad)
          keymap))
  (general-create-definer nyeong/leader-keys
    :keymaps 'global-leader-map)
  
  (nyeong/leader-keys
    "<escape>" '(keyboard-escape-quit :which-key t)
    "c" '(:ignore t :which-key "code")
    "c <escape>" '(keyboard-escape-quit :which-key t)
    "c r" '(eglot-rename :which-key "rename")
 
    "p" '(:ignore t :which-key "project")
    "p <escape>" '(keyboard-escape-quit :which-key t)
    "p p" '(project-switch-project :which-key "switch project")
    "p f" '(project-find-file :which-key "find file")
    "p b" '(project-list-buffers :which-key "list buffers")))

(provide 'init-general)
