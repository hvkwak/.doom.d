;;; init-editor.el --- Editor behavior configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

;;; Performance

(setq native-comp-jit-compilation t)

;;; Tab & Indentation

(setq-default tab-width 2)

(defun my-indent-setup ()
  "Set up the TAB key to indent with a single press."
  (local-set-key (kbd "<tab>") 'indent-for-tab-command))

(add-hook 'prog-mode-hook 'my-indent-setup)
(add-hook 'text-mode-hook 'my-indent-setup)

;;; Evil Cursor Behavior

;; Don't jump one char left when leaving Insert
(setq evil-move-cursor-back nil)

;; Let point sit after the last character (Emacs style)
(setq evil-move-beyond-eol t)

;;; Flycheck

(after! flycheck
  (global-flycheck-mode -1)
  (setq flycheck-global-modes nil))


(provide 'init-editor)
;;; init-editor.el ends here
