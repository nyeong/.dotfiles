(use-package autoinsert
  :init
  (setq auto-insert-query nil)
  :config
  (auto-insert-mode t))

;; org template
(define-auto-insert
  '("\\.org\\'" . "Org")
  '(""
    "#+TITLE: " str \n
    "#+AUTHOR: An Nyeong" \n
    "#+DATE: " '(format-time-string "%Y-%m-%d") \n
    ))

(define-auto-insert
  '("\\.el\\'" . "Elisp")
  '(nil
    ";;; " (file-name-nondirectory buffer-file-name) " --- "
    (read-string "Short description: ") " -*- lexical-binding: t -*-" \n
    \n
    ";;; Commentary: " \n
    ";; " _ \n
    \n
    ";;; Code:" \n
    \n
    \n
    "(provide '" (file-name-base buffer-file-name) ")" \n
    ";;; " (file-name-nondirectory buffer-file-name) " ends here" \n))

(provide 'init-autoinsert)
