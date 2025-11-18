;;; lisp/init-keybinds-md.el --- Markdown keybindings -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Markdown-mode specific keybindings. Common keybindings are in init-keybinds-common.el
;;; Code:

;; Enable common keys in markdown-mode
(add-hook 'markdown-mode-hook #'my-enable-common-keys)

(after! evil-markdown
  (evil-define-key '(normal insert visual) evil-markdown-mode-map
    (kbd "M-<down>") #'markdown-move-down
    (kbd "M-<up>") #'markdown-move-up
    (kbd "M-r") nil ;; cancels browse-url-of-file
    (kbd "M-i") nil
    (kbd "M-k") nil
    (kbd "M-j") nil
    (kbd "M-l") nil
    (kbd "M-I") nil
    (kbd "M-K") nil
    (kbd "M-J") nil
    (kbd "M-L") nil)
  )

(provide 'init-keybinds-md)

;; MARKDOWN-MODE SPECIFIC KEYBINDINGS Example
;; Add markdown-specific bindings here (e.g., markdown-preview, markdown-insert-link, etc.)
;; Example:
;; (map! :map evil-markdown-mode-map
;;       :localleader
;;       :desc "Markdown preview" "p" #'markdown-preview
;;       :desc "Insert link" "l" #'markdown-insert-link)
