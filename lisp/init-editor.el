;;; init-editor.el --- Editor behavior configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Editor settings: all setq statements for editor configuration,
;;; performance settings, cursor behavior, indentation settings, etc.
;;; Code:

;;; Performance
(setq native-comp-jit-compilation t)
(setq native-comp-async-jobs-number 2)

(defun my-indent-setup ()
  "Set up the TAB key to indent with a single press."
  (local-set-key (kbd "<tab>") 'c-indent-line-or-region))

(add-hook 'prog-mode-hook 'my-indent-setup)
(add-hook 'text-mode-hook 'my-indent-setup)

;;; Tab & Indentation
(setq tab-width 4)

(after! cc-mode
  (c-set-offset 'enum-intro 4)
  (c-set-offset ' brace-list-intro 4)
  (c-set-offset 'brace-list-close 0)
  (c-set-offset 'brace-list-entry 0)
  (add-hook 'c-mode-common-hook
            (lambda ()
              ;; deactivate auto fill
              (auto-fill-mode -1)
              (c-toggle-auto-newline -1)

              ;; set fill-column extreme big
              (setq fill-column 999999))))

;;; Imenu
(setq imenu-sort-function nil)

;;; Evil Cursor Behavior
;; Don't jump one char left when leaving Insert
(setq evil-move-cursor-back nil)

;; Let point sit after the last character (Emacs style)
(setq evil-move-beyond-eol t)

(provide 'init-editor)
;;; init-editor.el ends here
