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
        :n "j" #'magit-section-backward
        :n "l" #'magit-section-forward
        :n "M-i" #'previous-line
        :n "M-k" #'next-line
        :n "M-j" #'backward-char
        :n "M-l" #'forward-char
        :n "M-e" #'execute-extended-command
        :n "M-q" #'doom/escape)

  (map! :map magit-status-mode-map
        :n "i" #'previous-line
        :n "k" #'next-line
        :n "j" #'magit-section-backward
        :n "l" #'magit-section-forward)

  (map! :map magit-log-mode-map
        :n "i" #'previous-line
        :n "k" #'next-line
        :n "j" #'magit-section-backward
        :n "l" #'magit-section-forward)

  (map! :map magit-diff-mode-map
        :n "i" #'previous-line
        :n "k" #'next-line
        :n "j" #'magit-section-backward
        :n "l" #'magit-section-forward))

(provide 'init-keybinds-magit)
;;; init-keybinds-magit.el ends here
