;;; config.el -*- lexical-binding: t; -*-

(setq user-full-name "An Nyeong"
      user-mail-address "me@annyeong.me")

;; secrets.el 파일 로드 (API 키 등 민감한 정보)
(let ((secret-file (expand-file-name "secrets.el" doom-private-dir)))
  (when (file-exists-p secret-file)
    (load secret-file)))

(setq-default
 tap-width 2
 evil-shift-width 2
 indent-tabs-mode nil
 standard-indent 2)

(setq doom-theme 'doom-one-light)
(setq display-line-numbers-type 'visual)

(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(if (fboundp 'w32-set-system-coding-system)
    (w32-set-system-coding-system 'utf-8))
;; 날짜 표시를 영어로한다. org mode에서 time stamp 날짜에 영향을 준다.
(setq system-time-locale "C")
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

(setq doom-font (font-spec :family "Monoplex KR Wide Nerd" :size 14 :weight 'semi-light))
;;       doom-variable-pitch-font (font-spec :family "Liga SFMono Nerd Font" :size 14 :weight 'semi-light)
;;       doom-serif-font (font-spec :family "Liga SFMono Nerd Font" :size 14 :weight 'semi-light))

(set-face-attribute 'default nil :family "Monoplex KR Wide Nerd")
;; (set-face-attribute 'default nil :family "Liga SFMono Nerd Font")
;; (set-fontset-font "fontset-default" '(#x1100 . #xffdc) (font-spec :family "Monoplex KR Nerd"))
;; (set-fontset-font "fontset-default" '(#xe0bc . #xf66e) (font-spec :family "Monoplex KR Nerd"))
;; (set-fontset-font "fontset-default" 'hangul (font-spec :family "Monoplex KR Nerd"))
(set-face-attribute 'default nil :height 140)
(set-default 'line-spacing 3)

;; (setq face-font-rescale-alist
;;       '((".*hiragino.*" . 1.2)
;;         ("Monoplex KR Wide Nerd" . 1.0)))

(setq shell-file-name "zsh")
(setq vterm-shell "zsh -l")

(setq org-directory "~/hanassig/")
(after! org-roam
  (setq org-roam-directory (file-truename "~/hanassig")
        epa-pinentry-mode 'loopback
        org-roam-node-template "#+TITLE: ${title}\n"))

(after! org
  (setq org-latex-compiler "xelatex")
  (setq org-latex-pdf-process
      '("latexmk -xelatex -interaction=nonstopmode -output-directory=%o %f")))
(after! ox-latex
  (add-to-list 'org-latex-default-packages-alist '("cjk" "kotex" nil)))

(use-package! org-test)

(use-package! org-ql
  :after org)

;; 픽셀 단위 스크롤
(pixel-scroll-precision-mode t)

;; 기본 템플릿
(setq org-roam-capture-templates
  ;; "d" :: 키
  ;; "default" :: 이름
  `(("d" "default" plain
    ;; 노트에 삽입될 내용
    "%?"
    ;; 새 파일일 경우
    :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        ,(concat
                          ":PROPERTIES:\n"
                          ":CREATED: <%<%Y-%m-%d %a>>\n"
                          ":MODIFIED: <%<%Y-%m-%d %a>>\n"
                          ":END:\n"
                          "#+TITLE: ${title}\n\n"))
    ;; ensures that the full file will be displayed when captured.
    :unnarrowed t)))

;; 프로젝트 템플릿
(defvar nyeong/project-template
  `("p" "Project" plain
    "%?"
    :target (file+head "projects/${slug}.org"
                       ,(concat
                         ":PROPERTIES:\n"
                         ":CREATED: <%<%Y-%m-%d %a>>\n"
                         ":MODIFIED: <%<%Y-%m-%d %a>>\n"
                         ":END:\n\n"
                         "#+title: ${title}\n"
                         "#+DESCRIPTION: \n"))
    :unnarrowed t)
  "프로젝트를 위한 org-roam 캡처 템플릿.
- ${title}: 문서 제목")

;; 영역 템플릿
(defvar nyeong/area-template
  `("a" "Area" plain
    "%?"
    :target (file+head "areas/${slug}.org"
                       ,(concat
                         ":PROPERTIES:\n"
                         ":CREATED: <%<%Y-%m-%d %a>>\n"
                         ":MODIFIED: <%<%Y-%m-%d %a>>\n"
                         ":END:\n"
                         "#+TITLE: ${title}\n"
                         "#+DESCRIPTION: \n"))
    :unnarrowed t)
  "영역(Area)을 위한 org-roam 캡처 템플릿.
- ${title}: 문서 제목")

;; 노트 템플릿
(defvar nyeong/note-template
  `("n" "Note" plain
    "%?"
    :target (file+head "notes/${slug}.org"
                       ,(concat
                         ":PROPERTIES:\n"
                         ":CREATED: <%<%Y-%m-%d %a>>\n"
                         ":MODIFIED: <%<%Y-%m-%d %a>>\n"
                         ":END:\n\n"
                         "#+TITLE: ${title}\n"
                         "#+DESCRIPTION: \n"))
    :unnarrowed t)
  "개념 정리를 위한 org-roam 캡처 템플릿.
- ${title}: 문서 제목")

;; inbox 캡처 템플릿
(defvar nyeong/inbox-template
  `("i" "Inbox" plain
    "* %U\n\n%?"
    :target (file+head "inbox/capture.org"
                       ,(concat
                         ":PROPERTIES:\n"
                         ":CREATED: <%<%Y-%m-%d %a>>\n"
                         ":MODIFIED: <%<%Y-%m-%d %a>>\n"
                         ":END:\n"
                         "#+TITLE: 캡처 박스\n"))
    :unnarrowed t)
  "캡처")

;; 레퍼런스 템플릿
(defvar nyeong/reference-template
  `("r" "Reference" plain "%?"
    :target (file+head "references/${citekey}.org"
                       ,(concat
                         ":PROPERTIES:\n"
                         ":ROAM_REFS: [cite:@${citekey}] ${url}\n"
                         ":CREATED: [%<%Y-%m-%d %a>]\n"
                         ":MODIFIED: [%<%Y-%m-%d %a>]\n"
                         ":END:\n"
                         "#+TITLE: ${title}\n"))
    :immediate-finish t
    :unnarrowed t)
  "참고 문헌을 위한 org-roam 캡처 템플릿.
- ${citekey} : 인용키
- ${title}: 문서 제목
- ${url}: 웹 주소 (있는 경우)")

(after! org-roam
  (add-to-list 'org-roam-capture-templates nyeong/project-template)
  (add-to-list 'org-roam-capture-templates nyeong/area-template)
  (add-to-list 'org-roam-capture-templates nyeong/note-template)
  (add-to-list 'org-roam-capture-templates nyeong/reference-template)
  (add-to-list 'org-roam-capture-templates nyeong/inbox-template))

(defun nyeong/update-last-modified ()
  "Update the LAST_MODIFIED property in the current buffer."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward ":MODIFIED:" nil t)
        (org-set-property "MODIFIED"
                         (format-time-string "<%Y-%m-%d %a>"))))))

(defun nyeong/upcase-org-title ()
  "Convert #+title: to #+TITLE: in org files."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\+title:" nil t)
        (replace-match "#+TITLE:" t)))))

(add-hook 'before-save-hook #'nyeong/update-last-modified)
(add-hook 'before-save-hook #'nyeong/upcase-org-title)

(after! org
  (setq org-agenda-files '("~/hanassig/journals/weekly"
                          "~/hanassig/projects"
                          "~/hanassig/areas")
        org-cite-export-processors '((t csl))
        org-cite-csl-styles-dir "~/hanassig/csl-styles"
        org-cite-csl-default-style "ieee"
        org-cite-global-bibliography '("~/hanassig/references.bib")))
(setq! citar-bibliography '("~/hanassig/references.bib"))

(defun nyeong/format-authors (author-string)
  "저자 문자열을 '성1 & 성2' 또는 '성1 et al.' 형식으로 변환합니다."
  (if (not author-string)
      "Unknown"
    (let ((authors (split-string author-string " and ")))
      (cond
       ;; 단일 저자
       ((= (length authors) 1)
        (if (string-match "\\(.*\\), \\(.*\\)" (car authors))
            (match-string 1 (car authors))  ;; 성만 사용
          (car authors)))

       ;; 두 명의 저자
       ((= (length authors) 2)
        (concat (if (string-match "\\(.*\\), \\(.*\\)" (car authors))
                    (match-string 1 (car authors))
                  (car authors))
                " & "
                (if (string-match "\\(.*\\), \\(.*\\)" (cadr authors))
                    (match-string 1 (cadr authors))
                  (cadr authors))))

       ;; 세 명 이상의 저자
       (t
        (concat (if (string-match "\\(.*\\), \\(.*\\)" (car authors))
                    (match-string 1 (car authors))
                  (car authors))
                " et al."))))))

(defun nyeong/get-entry-value (entry key &optional default)
  "BibTeX entry에서 key에 해당하는 값을 가져오고 없으면 default 반환"
  (let ((value (citar-format--entry (format "${%s}" key) entry)))
    (if (or (null value) (string-empty-p value))
        default
      value)))

(defun nyeong/create-reference-from-cite (citekey)
  (interactive (list (citar-select-ref)))
  (let* ((entry (citar-get-entry citekey))
         (raw-title (nyeong/get-entry-value entry "title" "Untitled"))
         (author (nyeong/format-authors (nyeong/get-entry-value entry "author" nil)))
         (year (nyeong/get-entry-value entry "year" "n.d."))
         (title (format "%s (%s, %s)" raw-title author year))
         (url (or (nyeong/get-entry-value entry "url" nil)
                  (nyeong/get-entry-value entry "howpublished" nil)
                  "")))
    (org-roam-capture- :templates (list nyeong/reference-template)
                       :info (list :citekey citekey
                                   :title title
                                   :url url)
                       :node (org-roam-node-create :title title)
                       :props (list :finalize 'find-file))))

(defun nyeong/org-babel-tangle-ensure-directory (file)
  "Ensure the directory for FILE exists."
  (let ((dir (file-name-directory file)))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(add-hook 'org-babel-pre-tangle-hook
          (lambda ()
            (let ((tangle-file (org-entry-get nil "TANGLE")))
              (when (and tangle-file (string-match-p "/" tangle-file))
                (nyeong/org-babel-tangle-ensure-directory tangle-file)))))

(after! (:and org ob-d2)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((d2 . t))))

(use-package! org-ps
 :load-path "/Users/nyeong/Repos/org-ps.el"
 :config
 (defun org-ps-cpp--compile-cmd (source-file output-file)
   (list "nix" "shell" "nixpkgs#clang" "nixpkgs#catch2_3" "--command"
       "bash" "-c"
       (format
        "catch2_path=$(nix eval --raw nixpkgs#catch2_3.outPath) && \\\n          clang++ %s -o %s \\\n          -I\"$catch2_path/include\" \\\n          -L\"$catch2_path/lib\" \\\n          -lCatch2Main -lCatch2 \\\n          -std=c++20 -O2 -Wall -Werror -Wpedantic -Wextra"
        (shell-quote-argument source-file)
        (shell-quote-argument output-file))))

 (defun org-ps-cpp--execute-cmd (executable-file)
   (list executable-file "--durations" "yes"))

 (defun org-ps-cpp--template-fn (solution-code test-code)
   "Generate single C++ code by concatenating SOLUTION-CODE and TEST-CODE."
   (concat
    "#include <catch2/catch_test_macros.hpp>\n"
    "#include <vector>\n"
    "#include <string>\n"
    "#include <iostream>\n"
    "#include <algorithm>\n\n"
    "using namespace std;\n\n"
    solution-code "\n\n"
    test-code))

 (defun org-ps-cpp--edit-prep-fn (babel-info)
   (let* ((header-params (nth 2 babel-info))
        (tangle-file (alist-get :tangle header-params))
        (project-root (expand-file-name "~/hanassig/"))
        (file-path (if (and tangle-file (not (string= tangle-file "no")))
                  (expand-file-name tangle-file project-root)
                 (expand-file-name "temp.cpp" project-root)))
        (compile-commands (expand-file-name "compile_commands.json" project-root)))
     (setq-local buffer-file-name file-path)
     (setq-local default-directory project-root)

     (when (and (fboundp 'lsp!) (not lsp-mode)) (lsp!))))

 (org-ps--register-backend "cpp"
                   (list :compile-cmd #'org-ps-cpp--compile-cmd
                       :execute-cmd #'org-ps-cpp--execute-cmd
                       :template-fn #'org-ps-cpp--template-fn
                       :edit-prep-fn #'org-ps-cpp--edit-prep-fn
                       :file-ext "cpp"))

   (defun org-ps-rust--compile-cmd (source-file output-file)
    (list "nix" "shell" "nixpkgs#rustc" "--command"
          "bash" "-c"
          (format
           "rustc %s -o %s --edition 2021 -C opt-level=2 -C debuginfo=0 --test"
           (shell-quote-argument source-file)
           (shell-quote-argument output-file))))

  (defun org-ps-rust--execute-cmd (executable-file)
    (list executable-file "--show-output"))

  (defun org-ps-rust--template-fn (solution-code test-code)
    "Generate single Rust test file by concatenating SOLUTION-CODE and TEST-CODE."
    (concat
     solution-code "\n\n"
     "#[cfg(test)]\n"
     "mod tests {\n"
     "    use super::*;\n\n"
     test-code "\n"
     "}\n"))

   (defun org-ps-rust--edit-prep-fn (babel-info)
  (let* ((header-params (nth 2 babel-info))
         (tangle-file (alist-get :tangle header-params))
         ;; 임시 버퍼의 디렉토리
         (temp-root (file-name-directory (buffer-file-name)))
         (cargo-toml (expand-file-name "Cargo.toml" temp-root)))

    ;; 더미 Cargo.toml 생성
    (unless (file-exists-p cargo-toml)
      (with-temp-file cargo-toml
        (insert "[package]\n")
        (insert "name = \"hanassig\"\n")
        (insert "version = \"0.1.0\"\n")
        (insert "edition = \"2021\"\n")))

    (setq-local default-directory temp-root)
    (setq-local lsp-rust-analyzer-diagnostics-enable nil)

    (when (and (fboundp 'lsp!) (not lsp-mode)) (lsp!))))


  (org-ps--register-backend "rust"
                            (list :compile-cmd #'org-ps-rust--compile-cmd
                                  :execute-cmd #'org-ps-rust--execute-cmd
                                  :template-fn #'org-ps-rust--template-fn
                                  :edit-prep-fn #'org-ps-rust--edit-prep-fn
                                  :file-ext "rs"))

 )

(after! lsp-clangd
  (setq lsp-clients-clangd-args
        '("-j=3"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=never"
          "--header-insertion-decorators=0"
          "--pch-storage=memory"
          "--query-driver=/usr/bin/clang++,/Library/Developer/CommandLineTools/usr/bin/clang++"))
  (set-lsp-priority! 'clangd 2))

(after! ccls
  (setq ccls-initialization-options
        '(:index (:comments 2)
          :completion (:detailedLabel t)
          :clang (:extraArgs ["-std=c++20"]
                  :resourceDir "")))
  (set-lsp-priority! 'ccls 1))

(after! nix-mode
  (setq lsp-nix-nil-formatter ["nixfmt"]))
