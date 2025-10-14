;;; init-keybinds.el --- Keybindings for Doom Emacs -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(after! (evil evil-snipe)

  (keymap-global-set "C-h" help-map) ;; enables C-h everywhere, + combined with init-lsp.el
  (keymap-global-unset "C-z" t)

  ;; stop evil-snipe from hijacking `s`/`S`
  (map! :map (evil-snipe-local-mode-map evil-snipe-override-mode-map)
        :n "s" nil
        :n "S" nil
        :v "s" nil
        :v "S" nil)

  ;; Global Map
  (map! :g "M-q" #'doom/escape
        )

  (map! :leader
      (:prefix ("k" . "kill")
       ;; search & jumps
       :desc "kill buffer"             "k" #'kill-buffer
       :desc "kill frame"              "f" #'delete-frame
       :desc "kill workspace(project)" "p" #'+workspace/kill
       ))

  (evil-define-key '(normal insert visual) global-map

    (kbd "M-p")        #'lsp-ui-doc-toggle
    (kbd "M-P")        #'lsp-signature-toggle-full-docs
    (kbd "M-<f12>")    #'my/toggle-between-header-and-source
    (kbd "<f12>")      #'lsp-find-definition
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

  ;;; Original bindings for normal state(* denotes prefix)
  ;; q : q-  record/play macro (q{register})
  ;; w  : move forward to next word begin
  ;; e  : move forward to next word end
  ;; r  : replace single character under cursor
  ;; t  : snipe 1 char
  ;; y  : y-  yank (copy) motion/operator
  ;; u  : undo
  ;; i  : switch to insert mode
  ;; o  : insert new line below and enter insert mode
  ;; p  : paste after cursor
  ;; a  : append after cursor
  ;; s  : snipe 2 char
  ;; d  : d-  delete motion/operator
  ;; f  : move forward to {char}
  ;; g* : g-  “goto” / various extended motions
  ;; h  : move left
  ;; j  : move down
  ;; k  : move up
  ;; l  : move right
  ;;                                 ;  : repeat last f/t/F/T motion
  ;; z* : z-  folding / scrolling / view adjustment commands
  ;; x  : delete character under cursor
  ;; c* : c-  change (delete + insert)
  ;; v  : visual mode
  ;; b  : move backward to word begin
  ;; n  : repeat last search (next)
  ;; m* : m-  mark position
  ;; ,  : repeat last f/t/F/T in opposite direction
  ;; .  : repeat last change
  ;; /  : search forward

  ;;; Reset Normal State Map: Available Keys
  ;; lower-case + unshifted symbols
  (dolist (k '("q" "w" "e" "r" "t"
               "a" "s"     "f"                     ";"
                   "x" "c" "v"     "n" "m" "," "." "/"))
    (define-key evil-normal-state-map (kbd k) nil))
  (dolist (k '("Q" "W" "E" "R" "T" "Y" "U" "I"     "P"
               "A" "S" "D" "F"         "J" "K" "L" ":"
               "Z" "X" "C" "V" "B" "N" "M" "<" ">" "?"))
    (define-key evil-normal-state-map (kbd k) nil))

  ;;; Unavailable keys without M
  ;; "i" #'previous-line
  ;; "k" #'next-line
  ;; "j" #'backward-char
  ;; "l" #'forward-char
  ;; "u" #'smart-beginning-of-line
  ;; "o" #'move-end-of-line
  ;; "g" #'centaur-tabs-backward
  ;; "h" #'centaur-tabs-forward ;; shift
  ;; "G" #'centaur-tabs-move-current-tab-to-left
  ;; "H" #'centaur-tabs-move-current-tab-to-right ;; shift
  ;; "z" #'undo-fu-only-undo

  ;; "b" #'evil-open-below
  ;; "y" evil-yank
  ;; "p" evil-paste-after
  ;; "d" keep evil-delete. it's useful.
  ;; "O" relevant to "o" move-end-of-line. otherwise it opens a new line

  ;;; Normal State: navigate, edit structure, execute commands
  (map! :map evil-normal-state-map

        ;; Some of them after key requires Navigation with M-.
        ;; This quite slows down.
        ;; e.g. if M- in insert? M- in normal?

        ;; Basic Navigation
        "i" #'previous-line
        "k" #'next-line
        "j" #'backward-char
        "l" #'forward-char
        "u" #'smart-beginning-of-line
        "o" #'move-end-of-line
        "M-j" #'backward-char ;; works well combined with SHIFT
        "M-l" #'forward-char
        "M-k" #'next-line
        "M-h" nil
        "M-s M-j" #'evil-window-left

        ;; functions with M-
        "M-m" #'c-beginning-of-defun
        "M-i" #'evil-insert
        "M-q" #'evil-escape

        ;; Tabs Navigation
        "h" #'centaur-tabs-backward
        "g" #'centaur-tabs-forward ;; shift
        "H" #'centaur-tabs-move-current-tab-to-left
        "G" #'centaur-tabs-move-current-tab-to-right ;; shift

        "z" #'undo-fu-only-undo
        (:prefix ("s" . "save/snipe/switch") ;;
                 "s"   #'my/save-and-escape
                 "n"   #'evil-snipe-s
                 "f"   #'+vertico/switch-workspace-buffer
                 ;; "w"   #'+vertico/switch-to ;; M-1 works better
                  )

        (:prefix ("w" . "window")
                 "j" #'evil-window-left ;; switching windows
                 "l" #'evil-window-right
                 "i" #'evil-window-up
                 "k" #'evil-window-down
                 )

        ;; enter inserst state
        ;; "b" #'evil-open-below

        ;; dap
        ;; "<f3>" #'dap-ui-locals
        ;; "<f4>" #'dap-ui-breakpoints ;; was once eval-buffer-and-close
        ;; "<f5>" #'dap-debug
        ;; "<f6>" #'my/dap-debugger-setting
        ;; "<f7>" #'my/dap-debug-close
        ;; ""  #'dap-eval
        ;; ""  #'dap-breakpoint-add
        ;; ""  #'dap-breakpoint-delete
        ;; ""  #'dap-continue
        ;; ""  #'dap-next
        ;;; Normal Mode ends here
        )

  ;;; Insert State
  (map! :map evil-insert-state-map

        "M-q" #'my/insert-escape-and-clear

        ;; Copy and Paste
        "C-w" #'kill-region
        "M-y" #'yank

        ;; Navigation
        "M-i" #'previous-line
        "M-k" #'next-line
        "M-j" #'backward-char
        "M-l" #'forward-char
        "M-h" #'c-beginning-of-defun

        ;; Prefix M- but same
        "M-RET"     #'newline-and-indent            ;; same as ENTER
        "M-<next>"  #'scroll-up-command             ;; same as PgDn.
        "M-<prior>" #'scroll-down-command           ;; same as PgUp
        "M-DEL"     #'delete-char                   ;; Delete
        "M-;"       (lambda () (interactive) (insert ";"));; same as ;
        "M-/"       #'comment-dwim ;; insert comment
        ;;; Insert Mode ends here
        )

  ;;; Visual State
  (map! :map evil-visual-state-map
        ;; Navigation
        "M-i" #'previous-line
        "M-k" #'next-line
        "M-j" #'backward-char
        "M-l" #'forward-char
        )

  ;;; Emacs State
  (map! :map evil-emacs-state-map
        "C-a" #'evil-exit-emacs-state
        )

)

(after! (evil cc-mode)
  (map! :map c-mode-base-map
        :ni "C-d" #'consult-lsp-diagnostics
        :ni "C-h k" #'describe-key))  ;; optional

(after! (evil help-mode)
  (define-key help-mode-map (kbd "<normal-state> i") #'previous-line))

;;; Vertico candidate navigation
(after! vertico
  ;; Up/Down through candidates
  (define-key vertico-map (kbd "M-i") #'vertico-previous)
  (define-key vertico-map (kbd "M-k") #'vertico-next)
  ;; Left/Right within the minibuffer input
  (define-key vertico-map (kbd "M-j") #'backward-char)
  (define-key vertico-map (kbd "M-l") #'forward-char)
  ;; Home/End equivalents
  (define-key vertico-map (kbd "M-u") #'smart-beginning-of-line) ; or move-beginning-of-line
  (define-key vertico-map (kbd "M-o") #'move-end-of-line)
  ;; quit M-q
  (define-key vertico-map (kbd "M-q") #'evil-escape))


(provide 'init-keybinds)
;;; init-keybinds.el ends here
