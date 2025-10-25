;;; lisp/init-keybinds-treemacs.el -*- lexical-binding: t; -*-
(after! (treemacs evil-collection)
  ;; Works even if treemacs-evil isn't present
  (map! :map treemacs-mode-map
        "M-e"  #'execute-extended-command
        "<f9>" #'treemacs
        "M-s M-j"    #'evil-window-left
        "M-s M-l"    #'evil-window-right
        "M-s M-i"    #'evil-window-up
        "M-s M-k"    #'evil-window-down
        "M-i" #'previous-line
        "M-k" #'next-line
        "M-j" #'backward-char
        "M-l" #'forward-char
        "i" #'previous-line
        "j" #'backward-char
        "k" #'next-line
        "l" #'forward-char
        "h" nil)
  ;; If treemacs-evil is enabled, bind there too
  (when (boundp 'evil-treemacs-state-map)
    (map! :map evil-treemacs-state-map
        "M-e"  #'execute-extended-command
        "<f9>" #'treemacs
        "M-s M-j"    #'evil-window-left
        "M-s M-l"    #'evil-window-right
        "M-s M-i"    #'evil-window-up
        "M-s M-k"    #'evil-window-down
        "M-i" #'previous-line
        "M-k" #'next-line
        "M-j" #'backward-char
        "M-l" #'forward-char
        "i" #'previous-line
        "j" #'backward-char
        "k" #'next-line
        "l" #'forward-char
        "h" nil)
        ))
  )

(provide 'init-keybinds-treemacs)
