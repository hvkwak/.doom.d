;;; lisp/init-keybinds-md.el -*- lexical-binding: t; -*-

(after! evil-snipe
  ;; stop evil-snipe from hijacking `s`/`S`
  (map! :map (evil-snipe-local-mode-map evil-snipe-override-mode-map)
        :n "f" nil
        :n "F" nil
        :n "s" nil
        :n "S" nil
        :v "s" nil
        :v "S" nil
        :v "f" nil
        :v "F" nil)
  )

(after! evil-markdown
  (keymap-global-set "C-h" help-map) ;; enables C-h everywhere, + combined with init-lsp.el
  (keymap-global-unset "C-z" t)

  (dolist (k '("q" "w" "e" "r" "t"
               "a"         "f"                     ";"
                   "x" "c" "v" "b" "n" "m" "," "." "/"))
    (define-key evil-markdown-mode-map (kbd k) nil))

  (map! :leader
        (:prefix ("k" . "kill")
         :desc "kill buffer"             "k" #'kill-buffer
         :desc "kill frame"              "f" #'delete-frame
         :desc "kill workspace(project)" "p" #'+workspace/kill)

        (:prefix ("s" . "switch")
        :desc "window left" "j" #'evil-window-left
        :desc "window right" "l" #'evil-window-right
        :desc "window up" "i" #'evil-window-up
        :desc "window down" "k" #'evil-window-down))

  (evil-define-key '(normal insert visual) evil-markdown-mode-map

    (kbd "<f9>")       #'treemacs
    (kbd "M-,")        #'evil-jump-backward
    (kbd "M-.")        #'evil-jump-forward
    (kbd "M-9")        #'my/jump-matching-paren
    (kbd "C-b")        #'view-echo-area-messages
    (kbd "C-S-z")      #'undo-fu-only-redo
    (kbd "C-z")        #'undo-fu-only-undo
    (kbd "M-s M-j")    #'my/select-symbol-at-point
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

  (map! :map evil-markdown-mode-map
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
        :n "M-q" #'evil-escape
        :n "h" #'centaur-tabs-backward
        :n "g" #'centaur-tabs-forward
        :n "H" #'centaur-tabs-move-current-tab-to-left
        :n "G" #'centaur-tabs-move-current-tab-to-right
        :n "z" #'undo-fu-only-undo
        (:prefix ("s" . "save/snipe/switch")
                 :n "s"   #'my/save-and-escape
                 :n "n"   #'evil-snipe-s
                 :n "f"   #'+vertico/switch-workspace-buffer)
        :i "M-q" #'my/insert-escape-and-clear
        :i "C-w" #'kill-region
        :i "M-p" #'yank
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
(provide 'init-keybinds-md)
