;;; init-keybinds.el --- Core keybindings for Doom Emacs -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(after! evil
  (keymap-global-set "C-h" help-map)
  (keymap-global-unset "C-z" t)
  (keymap-global-unset "M-SPC" t) ;; disables M-SPC in normal mode

  ;;; Global Map
  (map! :g "M-q" #'doom/escape
        :g "M-y" #'yank)

  (map! :leader
        "k" nil)

  (map! :leader
      (:prefix ("k" . "kill")
       :desc "kill current buffer"             "k" #'kill-current-buffer
       :desc "kill frame"                      "f" #'delete-frame
       :desc "kill current workspace(project)" "w" #'+workspace/kill))

  ;;; Global Map for All States
  (evil-define-key '(normal insert visual replace) global-map
    (kbd "C-w")        #'kill-region
    (kbd "M-m")        #'my-defun-sig-header-mode
    (kbd "M-M")        #'beginning-of-defun
    (kbd "M-a")        #'lsp-ui-doc-toggle
    (kbd "M-A")        #'lsp-signature-toggle-full-docs
    (kbd "M-<f12>")    #'my/toggle-between-header-and-source
    (kbd "<f12>")      #'lsp-find-definition
    (kbd "<f9>")       #'treemacs
    (kbd "M-,")        #'evil-jump-backward
    (kbd "M-.")        #'evil-jump-forward
    (kbd "M-8")        #'my/evil-select-inside-paren
    (kbd "M-9")        #'my/jump-matching-paren
    (kbd "C-b")        #'view-echo-area-messages
    (kbd "C-S-z")      #'undo-fu-only-redo
    (kbd "C-z")        #'undo-fu-only-undo
    (kbd "M-s M-j")    #'evil-window-left
    (kbd "M-s M-l")    #'evil-window-right
    (kbd "M-s M-i")    #'evil-window-up
    (kbd "M-s M-k")    #'evil-window-down
    (kbd "M-s M-e")    #'my/select-symbol-at-point
    (kbd "M-s M-f")    #'mark-defun
    (kbd "M-s M-p")    #'mark-page
    (kbd "M-s M-b")    #'+vertico/switch-workspace-buffer
    (kbd "M-s M-s")    #'my/save-and-escape
    (kbd "M-=")        #'centaur-tabs-extract-window-to-new-frame
    (kbd "<S-down-mouse-1>") #'ignore
    (kbd "<S-mouse-1>")      #'my/select-to-click
    (kbd "<home>")     #'smart-beginning-of-line
    (kbd "M-u")        #'smart-beginning-of-line
    (kbd "M-o")        #'move-end-of-line
    (kbd "C-f")        #'my/consult-line-dwim
    (kbd "M-f")        #'my/consult-line-dwim
    (kbd "M-r")        #'rg-dwim
    (kbd "M-'")        #'consult-imenu
    (kbd "M-e")        #'execute-extended-command)

  ;;; Reset Normal, Motion State Map
  (dolist (k '("q" "w" "e" "r" "t"
               "a"         "f"                     ";"
                   "x" "c" "v" "b" "n" "m" "," "." "/"))
    (define-key evil-normal-state-map (kbd k) nil))
  (dolist (k '("q" "w" "e" "r" "t"
               "a"         "f"                     ";"
               "x" "c" "v" "b" "n" "m" "," "." "/"))
    (define-key evil-motion-state-map (kbd k) nil))
  (dolist (k '("Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P"
               "A" "S" "D" "F"         "J" "K" "L" ":"
               "Z" "X" "C" "V" "B" "N" "M" "<" ">" "?"))
    (define-key evil-normal-state-map (kbd k) nil))

  ;;; Normal State
  (map! :map evil-normal-state-map
        "i" #'previous-line
        "k" #'next-line
        "j" #'backward-char
        "l" #'forward-char
        "u" #'smart-beginning-of-line
        "o" #'move-end-of-line
        "M-i" #'evil-insert
        "M-j" #'backward-char
        "M-l" #'forward-char
        "M-k" #'next-line
        "M-h" (lambda () (interactive))
        "p" (lambda () (interactive))
        "w"  #'evil-yank
        "y" #'evil-paste-after
        "M-w" #'evil-yank
        "M-y" #'evil-paste-after
        "M-q" #'evil-escape
        "h" #'centaur-tabs-backward
        "g" #'centaur-tabs-forward
        "M-h" #'centaur-tabs-backward
        "H" #'centaur-tabs-move-current-tab-to-left
        "G" #'centaur-tabs-move-current-tab-to-right
        "z" #'undo-fu-only-undo
        (:prefix ("s" . "save/snipe/switch/select")
                 :desc "save"                   "s"   #'my/save-and-escape
                 :desc "switch buffer"          "b"   #'+vertico/switch-workspace-buffer
                 :desc "snipe-s"                "n"   #'evil-snipe-s
                 :desc "select word"            "e"   #'my/select-symbol-at-point
                 :desc "select fun(daf, yaf)"   "f"   #'mark-defun
                 :desc "select page"            "p"   #'mark-page
                 :desc "widnow left"            "j" #'evil-window-left
                 :desc "widnow right"           "l" #'evil-window-right
                 :desc "widnow up"              "i" #'evil-window-up
                 :desc "widnow down"            "k" #'evil-window-down)
        ;; DAP
        "<f3>" #'dap-ui-locals
        "<f4>" #'dap-ui-breakpoints
        "<f5>" #'dap-debug
        "<f6>" #'my/dap-debugger-setting
        "<f7>" #'my/dap-debug-close
        "v"  #'dap-eval
        "bb"  #'dap-breakpoint-add
        "bk"  #'dap-breakpoint-delete
        "c"  #'dap-continue
        "n"  #'dap-next)

  ;;; Insert State
  (map! :map evil-insert-state-map
        "C-SPC" #'set-mark-command
        "M-y" #'yank
        "S-<left>" nil
        "S-<right>" nil
        "S-<down>" nil
        "S-<up>" nil
        "M-i" #'previous-line
        "M-k" #'next-line
        "M-j" #'backward-char
        "M-l" #'forward-char
        "M-q" #'my/insert-escape-and-clear
        "M-RET"     #'newline-and-indent
        "M-<next>"  #'scroll-up-command ;; whats this
        "M-<prior>" #'scroll-down-command
        "M-DEL"     #'delete-char
        "M-;"       (lambda () (interactive) (insert ";"))
        "M-/"       #'comment-dwim)

  ;;; Visual State
  (map! :map evil-visual-state-map
        "u"   #'smart-beginning-of-line
        "o"   #'move-end-of-line
        "w"   #'kill-ring-save
        "y"   #'evil-paste-after
        "i"   #'previous-line
        "k"   #'next-line
        "j"   #'backward-char
        "l"   #'forward-char
        "M-i" #'previous-line
        "M-k" #'next-line
        "M-j" #'backward-char
        "M-l" #'forward-char)

  ;;; Emacs State
  (map! :map evil-emacs-state-map
        "C-a" #'evil-exit-emacs-state)

)

(provide 'init-keybinds)
;;; init-keybinds.el ends here
