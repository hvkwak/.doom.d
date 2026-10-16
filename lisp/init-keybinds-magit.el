;;; init-keybinds-magit.el --- Magit keybindings -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;
;; Magit keybindings adapted to IJKL movement model.
;;
;;; Code:

;;; Magit Status
(after! magit
  (map! :map magit-mode-map
        :n "i" #'previous-line
        :n "k" #'next-line
        :n "j" #'backward-char
        :n "l" #'forward-char
        :n "u"  #'smart-beginning-of-line
        :nv "o"  #'move-end-of-line
        :n "M-i" #'previous-line
        :n "M-k" #'next-line
        :n "M-j" #'backward-char
        :n "M-l" #'forward-char
        :n "M-e" #'execute-extended-command
        :n "M-q" #'doom/escape)

  (map! :map magit-status-mode-map
        :n "i" #'previous-line
        :n "k" #'next-line
        :n "j" #'backward-char
        :n "l" #'forward-char
        )

  (map! :map magit-log-mode-map
        :n "i" #'previous-line
        :n "k" #'next-line
        :n "j" #'backward-char
        :n "l" #'forward-char
        )

  (map! :map magit-diff-mode-map
        :n "i" #'previous-line
        :n "k" #'next-line
        :n "j" #'backward-char
        :n "l" #'forward-char
        ))

(provide 'init-keybinds-magit)
;;; init-keybinds-magit.el ends here
