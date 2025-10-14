;;; lisp/init-keybinds-org.el -*- lexical-binding: t; -*-

(after! evil-org
  (map! :leader
        (:prefix ("k" . "kill")
         :desc "kill buffer"             "k" #'kill-buffer
         :desc "kill frame"              "f" #'delete-frame
         :desc "kill workspace(project)" "p" #'+workspace/kill))

  (evil-define-key '(normal insert visual) evil-org-mode-map

    (kbd "<f9>")       #'treemacs
    (kbd "M-,")        #'evil-jump-backward
    (kbd "M-.")        #'evil-jump-forward
    (kbd "M-9")        #'my/jump-matching-paren
    (kbd "C-b")        #'view-echo-area-messages
    (kbd "C-S-z")      #'undo-fu-only-redo
    (kbd "C-z")        #'undo-fu-only-undo
    (kbd "M-s M-e")    #'my/select-symbol-at-point
    (kbd "M-s M-s")    #'my/save-and-escape
    (kbd "M-s M-p")    #'+workspace/switch-to
    (kbd "M-=")        #'centaur-tabs-extract-window-to-new-frame
    (kbd "<S-down-mouse-1>") #'ignore
    (kbd "<S-mouse-1>")      #'my/select-to-click
    (kbd "<home>")     #'smart-beginning-of-line
    (kbd "M-u")        #'smart-beginning-of-line
    (kbd "M-o")        #'move-end-of-line
    (kbd "C-f")        #'my/consult-line-dwim
    (kbd "M-f")        #'my/consult-line-dwim
    (kbd "M-r")        #'projectile-find-references
    (kbd "M-R")        #'consult-ripgrep
    (kbd "M-'")        #'consult-imenu
    (kbd "M-e")        #'execute-extended-command
    )

  (map! :map evil-org-mode-map
        :n "i" #'previous-line
        :n "k" #'next-line
        :n "j" #'backward-char
        :n "l" #'forward-char
        :n "u" #'smart-beginning-of-line
        :n "o" #'move-end-of-line
        :n "M-i" #'evil-insert
        :n "M-j" #'backward-char ;; works well combined with SHIFT
        :n "M-l" #'forward-char
        :n "M-k" #'next-line
        :n "M-h" nil
        :niv "M-I" nil
        :niv "M-K" nil
        :niv "M-J" nil
        :niv "M-L" nil
        :n "M-s M-j" #'evil-window-left
        :n "M-s M-l" #'evil-window-right
        :n "M-s M-i" #'evil-window-up
        :n "M-s M-k" #'evil-window-down
        :n "M-q" #'evil-escape
        :n "h" #'centaur-tabs-backward
        :n "g" #'centaur-tabs-forward
        :n "H" #'centaur-tabs-move-current-tab-to-left
        :n "G" #'centaur-tabs-move-current-tab-to-right
        :n "z" #'undo-fu-only-undo
        (:prefix ("s" . "save/snipe/switch")
                 "s"   #'my/save-and-escape
                 "n"   #'evil-snipe-s
                 "f"   #'+vertico/switch-workspace-buffer)
        :i "M-q" #'my/insert-escape-and-clear
        :i "C-w" #'kill-region
        :i "M-y" #'yank
        :iv "M-i" #'previous-line
        :iv "M-k" #'next-line
        :iv "M-j" #'backward-char
        :iv "M-l" #'forward-char 
        :i "M-RET"     #'newline-and-indent            ;; same as ENTER
        :i "M-<next>"  #'scroll-up-command             ;; same as PgDn.
        :i "M-<prior>" #'scroll-down-command           ;; same as PgUp
        :i "M-DEL"     #'delete-char                   ;; Delete
        :i "M-;"       (lambda () (interactive) (insert ";"));; same as ;
        :i "M-/"       #'comment-dwim
        )
)
(provide 'init-keybinds-org)
