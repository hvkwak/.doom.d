;;; lisp/init-keybinds-md.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Markdown-mode specific keybindings. Common keybindings are in init-keybinds-common.el
;;; Code:

(require 'init-keybinds-common)



(after! evil-markdown
  (evil-define-key '(normal insert visual) evil-markdown-mode-map
    (kbd "M-r") nil ;; cancels browse-url-of-file
    (kbd "M-i") nil
    (kbd "M-k") nil
    (kbd "M-j") nil
    (kbd "M-l") nil
    (kbd "M-I") nil
    (kbd "M-K") nil
    (kbd "M-J") nil
    (kbd "M-L") nil)

  ;; Apply common writing mode setup
  (my-setup-writing-mode evil-markdown-mode-map 'evil-markdown-mode-hook)

  ;; MARKDOWN-MODE SPECIFIC KEYBINDINGS
  ;; Add markdown-specific bindings here (e.g., markdown-preview, markdown-insert-link, etc.)
  ;; Example:
  ;; (map! :map evil-markdown-mode-map
  ;;       :localleader
  ;;       :desc "Markdown preview" "p" #'markdown-preview
  ;;       :desc "Insert link" "l" #'markdown-insert-link)
  )

(provide 'init-keybinds-md)
