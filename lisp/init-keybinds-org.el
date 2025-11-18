;;; lisp/init-keybinds-org.el --- Keybindings for Doom Emacs -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Org-mode specific keybindings. Common keybindings are in init-keybinds-common.el
;;; Code:

;; Enable common keys in org-mode
(add-hook 'org-mode-hook #'my-enable-common-keys)

(after! evil-org

  ;; ORG-MODE SPECIFIC KEYBINDINGS
  ;; Add org-specific bindings here (e.g., org-agenda, org-capture, etc.)
  (evil-define-key '(normal insert visual) evil-org-mode-map
    (kbd "M-<down>") #'org-move-item-down
    (kbd "M-<up>") #'org-move-item-up
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

  ;; build SPC-o
  (after! which-key
    ;; this will disable that stubborn "vterm pop up description."
    (which-key-add-key-based-replacements "SPC o t" "org-mode toggles"))
  (map! :leader
        (:prefix ("o" . "org mode")
         :desc "org toggle heading"  "th" #'org-toggle-heading
         :desc "org toggle checkbox" "tc" #'org-toggle-checkbox
         :desc "org toggle item"     "ti" #'org-toggle-item
         ))

  (map! :map evil-org-mode-map
        :n "src" #'my/org-wrap-region-as-c
        :n "srp" #'my/org-wrap-region-as-python)
  )

(provide 'init-keybinds-org)
;;; init-keybinds-org.el ends here
