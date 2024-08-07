;;; init-org-mode.el

(defun nyeong/modify-org-inline-markup-rules ()
  "인라인 마크업 뒤에 조사를 붙일 수 있도록 허용"
  (setcar org-emphasis-regexp-components " \t('\"{[:multibyte:]")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "[:multibyte:]- \t.,:!?;'\")}\\")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))

(with-eval-after-load 'org
  (setq org-directory "~/Nextcloud/notes"
      org-default-notes-file (concat org-directory "/index.org")
      org-startup-indented t ;; 항목에 따른 들여쓰기
      org-src-fontify-natively t
      org-startup-with-inline-images t
      org-log-done 'time)
  (nyeong/modify-org-inline-markup-rules)
  (nyeong/leader-keys :major-modes 'org-mode
    "<return>" '(org-return :which-key "org return"))
  (nyeong/local-keys :major-modes 'org-mode
    "t" '(org-todo :which-key "cycle todo")
    "o" '(org-open-at-point :which-key "open link")
    "l" '(org-insert-link :which-key "insert link")))

(use-package org-cliplink
  :bind (("C-x p i" . org-cliplink)))

(use-package company-org-block
  :after 'company
  :custom
  (company-org-block-edit-style 'auto)
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))


(provide 'init-org-mode)

;;; init-org-mode.el ends here
