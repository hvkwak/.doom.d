;;; lisp/init-keybinds-md.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
         :desc "kill workspace(project)" "p" #'+workspace/kill))

  (evil-define-key '(normal insert visual) evil-markdown-mode-map

    (kbd "<f9>")       #'treemacs
    (kbd "M-,")        #'evil-jump-backward
    (kbd "M-.")        #'evil-jump-forward
    (kbd "M-9")        #'my/jump-matching-paren
    (kbd "C-b")        #'view-echo-area-messages
    (kbd "C-S-z")      #'undo-fu-only-redo
    (kbd "C-z")        #'undo-fu-only-undo
    (kbd "M-s M-j")    #'evil-window-left
    (kbd "M-s M-l")    #'evil-window-right
    (kbd "M-s M-i")    #'evil-window-up
    (kbd "M-s M-k")    #'evil-window-down
    (kbd "M-s M-e")    #'my/select-symbol-at-point
    (kbd "M-s M-d")    #'mark-defun
    (kbd "M-s M-p")    #'mark-page
    (kbd "M-s M-f")    #'+vertico/switch-workspace-buffer ;; "s" in evil normal mode
    (kbd "M-s M-s")    #'my/save-and-escape
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
    (kbd "M-h")        (lambda () (interactive))
    (kbd "M-I")        (lambda () (interactive))
    (kbd "M-K")        (lambda () (interactive))
    (kbd "M-J")        (lambda () (interactive))
    (kbd "M-L")        (lambda () (interactive))
    )

  (map! :map evil-markdown-mode-map
        ;; normal mode
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
        :n "M-h" (lambda () (interactive))
        :n "w"  #'evil-yank
        :n "y" #'evil-paste-after
        :n "M-w" #'evil-yank
        :n "M-y" #'evil-paste-after
        :n "M-q" #'evil-escape
        :n "h" #'centaur-tabs-backward
        :n "g" #'centaur-tabs-forward
        :n "H" #'centaur-tabs-move-current-tab-to-left
        :n "G" #'centaur-tabs-move-current-tab-to-right
        :n "z" #'undo-fu-only-undo
        (:prefix ("s" . "save/snipe/switch/select") ;;
                :n "s"   #'my/save-and-escape
                :n "f"   #'+vertico/switch-workspace-buffer
                :n "n"   #'evil-snipe-s
                :n "e"   #'my/select-symbol-at-point
                :n "d"   #'mark-defun
                :n "p"   #'mark-page
                :n "j" #'evil-window-left
                :n "l" #'evil-window-right
                :n "i" #'evil-window-up
                :n "k" #'evil-window-down
                 )


        ;; insert mode
        :i "M-y" #'yank
        :i "S-<left>" nil
        :i "S-<right>" nil
        :i "S-<down>" nil
        :i "S-<up>" nil
        :i "M-i" #'previous-line
        :i "M-k" #'next-line
        :i "M-j" #'backward-char
        :i "M-l" #'forward-char
        :i "M-q" #'my/insert-escape-and-clear
        :i "M-RET"     #'newline-and-indent            ;; same as ENTER
        :i "M-<next>"  #'scroll-up-command             ;; same as PgDn.
        :i "M-<prior>" #'scroll-down-command           ;; same as PgUp
        :i "M-DEL"     #'delete-char                   ;; Delete
        :i "M-;"       (lambda () (interactive) (insert ";"));; same as ;
        :i "M-/"       #'comment-dwim

        ;; visual mode
        :v "i"   (lambda () (interactive))
        :v "k"   (lambda () (interactive))
        :v "j"   (lambda () (interactive))
        :v "l"   (lambda () (interactive))
        :v "M-i" #'previous-line
        :v "M-k" #'next-line
        :v "M-j" #'backward-char
        :v "M-l" #'forward-char
        )
)
(provide 'init-keybinds-md)
