;;; lisp/init-keybinds-org.el -*- lexical-binding: t; -*-

(after! evil-org

  (evil-define-key '(normal insert visual) evil-org-mode-map

    (kbd "<f9>")       #'treemacs
    (kbd "M-,")        #'evil-jump-backward
    (kbd "M-.")        #'evil-jump-forward
    (kbd "M-9")        #'my/jump-matching-paren

    (kbd "C-b")        #'view-echo-area-messages

    (kbd "C-S-z")      #'undo-fu-only-redo
    (kbd "C-z")        #'undo-fu-only-undo

    ;; M-s for select/save/switch
    (kbd "M-s M-e")    #'my/select-symbol-at-point
    (kbd "M-s M-s")    #'my/save-and-escape
    (kbd "M-s M-p")    #'+workspace/switch-to ;; project

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

  ;;; Normal State: navigate, edit structure, execute commands
  (map! :map evil-org-mode-map

        ;;; <Normal State>
        :n "i" #'previous-line
        :n "k" #'next-line
        :n "j" #'backward-char
        :n "l" #'forward-char
        :n "u" #'smart-beginning-of-line
        :n "o" #'move-end-of-line
        :n "M-j" #'backward-char ;; works well combined with SHIFT
        :n "M-l" #'forward-char
        :n "M-k" #'next-line
        :n "M-h" nil
        :n "M-s M-j" #'evil-window-left

        ;; functions with M-
        :n "M-i" #'evil-insert
        :n "M-q" #'evil-escape

        ;; Tabs Navigation
        :n "h" #'centaur-tabs-backward
        :n "g" #'centaur-tabs-forward ;; shift
        :n "H" #'centaur-tabs-move-current-tab-to-left
        :n "G" #'centaur-tabs-move-current-tab-to-right ;; shift

        :n "z" #'undo-fu-only-undo

        (:prefix ("b" . "buffer") ;;
                 :n "b"    #'kill-buffer
                 :n "k"    #'kill-buffer
                 :n "s"    #'+vertico/switch-workspace-buffer
                  )

        (:prefix ("s" . "save/snipe") ;;
                 :n "s"    #'my/save-and-escape
                 :n "n"   #'evil-snipe-s
                  )

        (:prefix ("w" . "window")
                 :n "j" #'evil-window-left ;; switching windows
                 :n "l" #'evil-window-right
                 :n "i" #'evil-window-up
                 :n "k" #'evil-window-down
                 )
        ;;; Normal Mode ends here

        ;;; Insert Mode
        :i "M-q" #'my/insert-escape-and-clear

        ;; Copy and Paste
        :i "C-w" #'kill-region
        :i "M-y" #'yank

        ;; Navigation
        :iv "M-i" #'previous-line
        :iv "M-k" #'next-line
        :iv "M-j" #'backward-char
        :iv "M-l" #'forward-char
        :iv "M-h" #'c-beginning-of-defun
        :iv "M-I" nil
        :iv "M-K" nil
        :iv "M-J" nil
        :iv "M-L" nil

        ;; Prefix M- but same
        :i "M-RET"     #'newline-and-indent            ;; same as ENTER
        :i "M-<next>"  #'scroll-up-command             ;; same as PgDn.
        :i "M-<prior>" #'scroll-down-command           ;; same as PgUp
        :i "M-DEL"     #'delete-char                   ;; Delete
        :i "M-;"       (lambda () (interactive) (insert ";"));; same as ;
        :i "M-/"       #'comment-dwim ;; insert comment
        ;;; Insert Mode ends here

        ;;;
        )
)

;; (after! evil-markdown

;;   ;; Line Navigation
;;   (define-key evil-markdown-mode-map (kbd "<normal-state> i") #'previous-line)
;;   (define-key evil-markdown-mode-map (kbd "<insert-state> M-i") #'previous-line)
;;   (define-key evil-markdown-mode-map (kbd "<visual-state> M-i") #'previous-line)
;;   (define-key evil-markdown-mode-map (kbd "<normal-state> k") #'next-line)
;;   (define-key evil-markdown-mode-map (kbd "<insert-state> M-k") #'next-line)
;;   (define-key evil-markdown-mode-map (kbd "<visual-state> M-k") #'next-line)
;;   (define-key evil-markdown-mode-map (kbd "<normal-state> j") #'backward-char)
;;   (define-key evil-markdown-mode-map (kbd "<insert-state> M-j") #'backward-char)
;;   (define-key evil-markdown-mode-map (kbd "<visual-state> M-j") #'backward-char)
;;   (define-key evil-markdown-mode-map (kbd "<normal-state> l") #'forward-char)
;;   (define-key evil-markdown-mode-map (kbd "<insert-state> M-l") #'forward-char)
;;   (define-key evil-markdown-mode-map (kbd "<visual-state> M-l") #'forward-char)

;;   ;; evil insert in normal state
;;   (define-key evil-markdown-mode-map (kbd "<normal-state> M-i") #'evil-insert)

;;   ;; set to nil to select multiple lines
;;   (define-key evil-markdown-mode-map (kbd "<normal-state> M-K") nil)
;;   (define-key evil-markdown-mode-map (kbd "<insert-state> M-K") nil)
;;   (define-key evil-markdown-mode-map (kbd "<visual-state> M-K") nil)
;;   (define-key evil-markdown-mode-map (kbd "<normal-state> M-J") nil)
;;   (define-key evil-markdown-mode-map (kbd "<insert-state> M-J") nil)
;;   (define-key evil-markdown-mode-map (kbd "<visual-state> M-J") nil)
;;   (define-key evil-markdown-mode-map (kbd "<normal-state> M-L") nil)
;;   (define-key evil-markdown-mode-map (kbd "<insert-state> M-L") nil)
;;   (define-key evil-markdown-mode-map (kbd "<visual-state> M-L") nil)
;;   (define-key evil-markdown-mode-map (kbd "<normal-state> M-i") nil)
;;   (define-key evil-markdown-mode-map (kbd "<insert-state> M-i") nil)
;;   (define-key evil-markdown-mode-map (kbd "<visual-state> M-i") nil)
;;   )

(provide 'init-keybinds-org)
