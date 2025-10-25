;;; lisp/init-keybinds-org.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Org-mode specific keybindings. Common keybindings are in init-keybinds-common.el
;;; Code:

(require 'init-keybinds-common)

(after! evil-org

  ;; ORG-MODE SPECIFIC KEYBINDINGS
  ;; Add org-specific bindings here (e.g., org-agenda, org-capture, etc.)
  (evil-define-key '(normal insert visual) evil-org-mode-map
    (kbd "C-h") nil ;; frees C-h for C-h help command
    (kbd "u")   nil
    (kbd "O")   nil
    (kbd "g")   nil
    (kbd "o")   nil
    (kbd "M-o") nil
    (kbd "M-h") nil
    (kbd "M-r") nil
    (kbd "M-i") nil
    (kbd "M-k") nil
    (kbd "M-j") nil
    (kbd "M-l") nil
    (kbd "M-I") nil ;; Unbind org-mode's Meta-Shift keys that conflict with our layout
    (kbd "M-K") nil
    (kbd "M-J") nil
    (kbd "M-L") nil)

  (evil-define-key '(normal) evil-org-mode-map
    (kbd "src") #'my/org-wrap-region-as-c)

  ;; Apply common writing mode setup
  (my-setup-writing-mode evil-org-mode-map 'evil-org-mode-hook)
  )

(provide 'init-keybinds-org)

  ;; ;; Unbind ALL single-letter keys in normal state that conflict
  ;; ;; This prevents evil-org/evil-markdown from interfering with our custom layout
  ;; (dolist (state '(normal visual))
  ;;   (dolist (key '("i" "j" "k" "l" "u" "o" "h" "g" "w" "y" "z"
  ;;                  "q" "e" "r" "t" "a" "s" "f" "d" "p"
  ;;                  "x" "c" "v" "b" "n" "m"
  ;;                  "H" "G"))
  ;;     (evil-define-key state mode-map (kbd key) nil)))

  ;; ;; Also unbind in insert state if needed
  ;; ;; Note: These need to be set after the mode is loaded, so we add to the hook
  ;; (add-hook mode-hook
  ;;           (lambda ()
  ;;             (dolist (key '("M-i" "M-j" "M-k" "M-l" "M-y" "M-q" "M-I" "M-K"))
  ;;               (evil-define-key 'insert mode-map (kbd key) nil))))
