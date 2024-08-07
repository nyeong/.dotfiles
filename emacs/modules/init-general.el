(use-package which-key
  :demand
  :init
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode))

(use-package general
  :demand
  :init
  (defvar local-leader-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<escape>") 'keyboard-escape-quit)
      map)
    "모드마다 쓸 로컬 키맵")
  
  (defvar global-leader-map
        (let ((keymap (make-sparse-keymap)))
          (define-key keymap (kbd "SPC") 'meow-keypad)
          keymap)
        "전역으로 쓸 커스텀 키맵")
  
  :config
  (general-create-definer nyeong/leader-keys
    :keymaps 'global-leader-map)

  (general-create-definer nyeong/local-keys
    :keymaps 'local-leader-map)
  
  (nyeong/leader-keys
    "<escape>" '(keyboard-escape-quit :which-key t)

    "m" '(:keymap local-leader-map :which-key "mode specific")
    
    "c" '(:ignore t :which-key "code")
    "c <escape>" '(keyboard-escape-quit :which-key t)
    "c r" '(eglot-rename :which-key "rename")
 
    "p" '(:ignore t :which-key "project")
    "p <escape>" '(keyboard-escape-quit :which-key t)
    "p p" '(project-switch-project :which-key "switch project")
    "p f" '(project-find-file :which-key "find file")
    "p b" '(project-list-buffers :which-key "list buffers")))

(provide 'init-general)
