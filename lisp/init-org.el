;;; lisp/init-org.el --- org-mode configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package! toc-org
  :hook (org-mode . toc-org-mode)
  :config
  (setq toc-org-max-depth 3))

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
        (plist-put org-format-latex-options :scale 2.0)) ; tweak 2.0–2.6 if you want

  ;; High-quality transparent PNGs
  (setq org-preview-latex-process-alist
        '((dvipng :programs ("latex" "dvipng")
                  :description "dvi > png"
                  :message "Install: texlive texlive-latex-extra dvipng"
                  :image-input-type "dvi"
                  :image-output-type "png"
                  :image-size-adjust (1.0 . 1.0)
                  :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
                  :image-converter ("dvipng -D 300 -T tight -bg Transparent -o %O %f"))))

  ;; Automatically preview math on open (optional)
  (setq org-startup-with-latex-preview t))

;; Optional: if your screen is HiDPI
(setq image-scaling-factor 1.2)

(provide 'init-org)
;;; init-org.el ends here
