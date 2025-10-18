;;; lisp/init-keybinds-org.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Org-mode specific keybindings. Common keybindings are in init-keybinds-common.el
;;; Code:

(require 'init-keybinds-common)

(after! evil-org

  ;; Note: Still there are some keybindings that prevail. Unbind them manually.

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

  (evil-define-key '(normal insert visual) evil-org-mode-map

    (kbd "M-I") nil ;; Unbind org-mode's Meta-Shift keys that conflict with our layout
    (kbd "M-K") nil
    (kbd "M-J") nil
    (kbd "M-L") nil)

  ;; Apply common writing mode setup
  (my-setup-writing-mode evil-org-mode-map 'evil-org-mode-hook)

  ;; ORG-MODE SPECIFIC KEYBINDINGS
  ;; Add org-specific bindings here (e.g., org-agenda, org-capture, etc.)
  ;; Example:
  ;; (map! :map evil-org-mode-map
  ;;       :localleader
  ;;       :desc "Org agenda" "a" #'org-agenda
  ;;       :desc "Org capture" "c" #'org-capture)
  )

(provide 'init-keybinds-org)
