;;; lisp/init-keybinds-treemacs.el -*- lexical-binding: t; -*-
(after! (treemacs evil-collection)
  ;; Works even if treemacs-evil isn't present
  (map! :map treemacs-mode-map
        "M-i" #'previous-line
        "M-k" #'next-line
        "M-j" #'backward-char
        "M-l" #'forward-char
        "M-s M-l" #'evil-window-right)
  ;; If treemacs-evil is enabled, bind there too
  (when (boundp 'evil-treemacs-state-map)
    (map! :map evil-treemacs-state-map
        "M-i" #'previous-line
        "M-k" #'next-line
        "M-j" #'backward-char
        "M-l" #'forward-char
        "M-s M-l" #'evil-window-right))
  )

(provide 'init-keybinds-treemacs)
