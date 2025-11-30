;;; lisp/init-org.el --- org-mode configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

;;; Enable TOC
(use-package! toc-org
  :init
  (add-hook 'org-mode-hook #'toc-org-mode)
  :config
  (setq toc-org-max-depth 3))

;;; Insert Template at find file
(after! org
  (defun my/org-insert-header-template ()
    "Insert default Org header into new, empty .org files."
    (when (and (buffer-file-name)
               (string-match-p "\\.org\\'" (buffer-file-name))
               (= (point-min) (point-max))) ;; buffer is empty
      (insert "#+title: \n"
              "#+DESCRIPTION:\n"
              "#+AUTHOR: H. Kwak\n"
              "#+DATE: " (format-time-string "<%Y-%m-%d %a>") "\n"
              "#+OPTIONS: toc:t num:t\n\n")))

  (add-hook 'find-file-hook #'my/org-insert-header-template))

;;; Set org level colors
(after! org
  (setq org-n-level-faces 9)

  (custom-set-faces!
    ;; all vivid, no black, chosen to pop on a warm/yellow bg
    '(org-level-1  :foreground "#0033FF" :weight bold)  ; strong blue
    '(org-level-2  :foreground "#FF00AF" :weight bold)  ; hot magenta
    '(org-level-3  :foreground "#00AFAF" :weight bold)  ; teal
    '(org-level-4  :foreground "#8700FF" :weight bold)  ; bright violet
    '(org-level-5  :foreground "#D70000" :weight bold)  ; vivid red
    '(org-level-6  :foreground "#008080" :weight bold)  ; dark cyan
    '(org-level-7  :foreground "#008700" :weight bold)  ; bright green
    '(org-level-8  :foreground "#4B0082" :weight bold)  ; deep indigo
    '(org-level-9 :foreground "#00A000" :weight bold))  ; emerald
)

(after! org
  ;; If you use `org' and don't want your org files in the default location below,
  ;; change `org-directory'. It must be set before org loads!
  (setq org-directory "~/org/")

  ;; enable font-lock for bold/italic/underline/etc.
  (setq org-fontify-emphasized-text t)
  (setq org-hide-emphasis-markers t)

  ;; (optional) also prettify symbols like ->, <=, etc.
  (setq org-pretty-entities t)

  ;; Use dvipng (compatible with every Emacs build)
  (setq org-preview-latex-default-process 'dvipng)

  ;; Make math formulas big and readable
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 0.1)) ; tweak 2.0–2.6 if you want

  ;; High-quality transparent PNGs
  (setq org-preview-latex-process-alist
        '((dvipng :programs ("latex" "dvipng")
                  :description "dvi > png"
                  :message "Install: texlive texlive-latex-extra dvipng"
                  :image-input-type "dvi"
                  :image-output-type "png"
                  :image-size-adjust (1.0 . 1.0)
                  :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
                  :image-converter ("dvipng -D 200 -T tight -bg Transparent -o %O %f"))))

  ;; Automatically preview math on open (optional)
  (setq org-startup-with-latex-preview t))

;; Optional: if your screen is HiDPI
(setq image-scaling-factor 1.0)

(defun my/org-wrap-region-as-src (lang)
  "Wrap the active region in an Org src block for LANG.
Expands to whole lines. Indents #+begin_src / #+end_src by two spaces."
  (interactive "sLanguage: ")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (unless (use-region-p)
    (user-error "Select the code you want to wrap"))
  (when (org-in-src-block-p)
    (user-error "Already inside a src block"))
  (let* ((rb (region-beginning))
         (re (region-end))
         (lbeg (save-excursion (goto-char rb) (line-beginning-position)))
         (lend (save-excursion (goto-char re)
                               (if (bolp) (line-beginning-position) (line-end-position))))
         (content (buffer-substring-no-properties lbeg lend))
         (indent "  "))                            ; two spaces
    (goto-char lbeg)
    (delete-region lbeg lend)
    (insert (format "%s#+begin_src %s\n%s%s%s#+end_src\n"
                    indent lang
                    content
                    (if (string-suffix-p "\n" content) "" "\n")
                    indent))
    (forward-line 1)))

;; Convenience commands
(defun my/org-wrap-region-as-python () (interactive) (my/org-wrap-region-as-src "python"))
(defun my/org-wrap-region-as-c      () (interactive) (my/org-wrap-region-as-src "C"))


(provide 'init-org)
;;; init-org.el ends here
