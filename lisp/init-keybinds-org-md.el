;;; lisp/init-keybinds-org.el -*- lexical-binding: t; -*-

;; use define-key when needed
(after! evil-org
  (define-key evil-org-mode-map (kbd "<normal-state> M-i") #'previous-line)
  (define-key evil-org-mode-map (kbd "<insert-state> M-i") #'previous-line)
  (define-key evil-org-mode-map (kbd "<visual-state> M-i") #'previous-line)
  (define-key evil-org-mode-map (kbd "<normal-state> M-k") #'next-line)
  (define-key evil-org-mode-map (kbd "<insert-state> M-k") #'next-line)
  (define-key evil-org-mode-map (kbd "<visual-state> M-k") #'next-line)
  (define-key evil-org-mode-map (kbd "<normal-state> M-j") #'backward-char)
  (define-key evil-org-mode-map (kbd "<insert-state> M-j") #'backward-char)
  (define-key evil-org-mode-map (kbd "<visual-state> M-j") #'backward-char)
  (define-key evil-org-mode-map (kbd "<normal-state> M-l") #'forward-char)
  (define-key evil-org-mode-map (kbd "<insert-state> M-l") #'forward-char)
  (define-key evil-org-mode-map (kbd "<visual-state> M-l") #'forward-char)
  (define-key evil-org-mode-map (kbd "<normal-state> M-K") nil)
  (define-key evil-org-mode-map (kbd "<insert-state> M-K") nil)
  (define-key evil-org-mode-map (kbd "<visual-state> M-K") nil)
)

(after! evil-markdown

  ;; Line Navigation
  (define-key evil-markdown-mode-map (kbd "<normal-state> i") #'previous-line)
  (define-key evil-markdown-mode-map (kbd "<insert-state> M-i") #'previous-line)
  (define-key evil-markdown-mode-map (kbd "<visual-state> M-i") #'previous-line)
  (define-key evil-markdown-mode-map (kbd "<normal-state> k") #'next-line)
  (define-key evil-markdown-mode-map (kbd "<insert-state> M-k") #'next-line)
  (define-key evil-markdown-mode-map (kbd "<visual-state> M-k") #'next-line)
  (define-key evil-markdown-mode-map (kbd "<normal-state> j") #'backward-char)
  (define-key evil-markdown-mode-map (kbd "<insert-state> M-j") #'backward-char)
  (define-key evil-markdown-mode-map (kbd "<visual-state> M-j") #'backward-char)
  (define-key evil-markdown-mode-map (kbd "<normal-state> l") #'forward-char)
  (define-key evil-markdown-mode-map (kbd "<insert-state> M-l") #'forward-char)
  (define-key evil-markdown-mode-map (kbd "<visual-state> M-l") #'forward-char)

  ;; evil insert in normal state
  (define-key evil-markdown-mode-map (kbd "<normal-state> M-i") #'evil-insert)

  ;; set to nil to select multiple lines
  (define-key evil-markdown-mode-map (kbd "<normal-state> M-K") nil)
  (define-key evil-markdown-mode-map (kbd "<insert-state> M-K") nil)
  (define-key evil-markdown-mode-map (kbd "<visual-state> M-K") nil)
  (define-key evil-markdown-mode-map (kbd "<normal-state> M-J") nil)
  (define-key evil-markdown-mode-map (kbd "<insert-state> M-J") nil)
  (define-key evil-markdown-mode-map (kbd "<visual-state> M-J") nil)
  (define-key evil-markdown-mode-map (kbd "<normal-state> M-L") nil)
  (define-key evil-markdown-mode-map (kbd "<insert-state> M-L") nil)
  (define-key evil-markdown-mode-map (kbd "<visual-state> M-L") nil)
  (define-key evil-markdown-mode-map (kbd "<normal-state> M-i") nil)
  (define-key evil-markdown-mode-map (kbd "<insert-state> M-i") nil)
  (define-key evil-markdown-mode-map (kbd "<visual-state> M-i") nil)
  )

;; (after! evil-org
;;   (evil-define-key '(normal insert visual) evil-org-mode-map
;;     (kbd "M-i") #'previous-line
;;     (kbd "M-k") #'next-line
;;     (kbd "M-j") #'backward-char
;;     (kbd "M-l") #'forward-char
;;     (kbd "M-K") nil))


(provide 'init-keybinds-org-md)
