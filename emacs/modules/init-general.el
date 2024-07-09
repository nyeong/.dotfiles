(use-package which-key
  :demand
  :init
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode))

(use-package general
  :demand
  :config
  (defconst nyeong/leader "SPC")
  (general-create-definer nyeong/leader-keys
    :keymaps 'override
    :prefix nyeong/leader)
  
  (nyeong/leader-keys
    "<escape>" '(keyboard-escape-quit :which-key t)
    "c" '(:ignore t :which-key "code")
    "c <escape>" '(keyboard-escape-quit :which-key t)
    "c r" '(eglot-rename :which-key "rename")
 
    "p" '(:ignore t :which-key "project")
    "p <escape>" '(keyboard-escape-quit :which-key t)
    "p f" '(project-find-file :which-key "find file")
    "p b" '(project-list-buffers :which-key "list buffers")))

(provide 'init-general)
