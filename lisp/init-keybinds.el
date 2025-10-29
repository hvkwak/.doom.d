;;; init-keybinds.el --- Keybindings for Doom Emacs -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(after! evil
  (keymap-global-set "C-h" help-map) ;; enables C-h everywhere, + combined with init-lsp.el
  (keymap-global-unset "C-z" t)

  ;;; Global Map doom escape.
  (map! :g "M-q" #'doom/escape
        :g "M-y" #'yank)

  (map! :leader
        "k" nil ;;frees k for kill
        )

  (map! :leader
      (:prefix ("k" . "kill")
       :desc "kill current buffer"             "k" #'kill-current-buffer
       :desc "kill frame"                      "f" #'delete-frame
       :desc "kill current workspace(project)" "w" #'+workspace/kill))

  ;;; global-map for three modes
  (evil-define-key '(normal insert visual) global-map
    (kbd "M-SPC")      (lambda () (interactive)) ;; no more cycle-spacing
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
    (kbd "M-s M-f")    #'mark-defun ;; f for function
    (kbd "M-s M-p")    #'mark-page
    (kbd "M-s M-b")    #'+vertico/switch-workspace-buffer ;; "s" in evil normal mode
    (kbd "M-s M-s")    #'my/save-and-escape
    (kbd "M-=")        #'centaur-tabs-extract-window-to-new-frame
    (kbd "<S-down-mouse-1>") #'ignore
    (kbd "<S-mouse-1>")      #'my/select-to-click
    (kbd "<home>")     #'smart-beginning-of-line
    (kbd "M-u")        #'smart-beginning-of-line
    (kbd "M-o")        #'move-end-of-line
    (kbd "C-f")        #'my/consult-line-dwim
    (kbd "M-f")        #'my/consult-line-dwim
    (kbd "M-r")        #'my/consult-ripgrep-dwim
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
  ;; "s" prefix

  ;; "y" evil-yank
  ;; "p" evil-paste-after
  ;; "d" keep evil-delete. it's useful.
  ;; "O" should be on the list due to "O" opens a new line

  ;;; Normal State
  (map! :map evil-normal-state-map
        "i" #'previous-line
        "k" #'next-line
        "j" #'backward-char
        "l" #'forward-char
        "u" #'smart-beginning-of-line
        "o" #'move-end-of-line
        "M-i" #'evil-insert
        "M-j" #'backward-char ;; works well combined with SHIFT
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
        "M-h" #'centaur-tabs-backward ;; no M-g for forward
        "H" #'centaur-tabs-move-current-tab-to-left
        "G" #'centaur-tabs-move-current-tab-to-right
        "z" #'undo-fu-only-undo
        (:prefix ("s" . "save/snipe/switch/select") ;;
                 :desc "save"                   "s"   #'my/save-and-escape
                 :desc "switch buffer"          "b"   #'+vertico/switch-workspace-buffer
                 :desc "snipe-s"                "n"   #'evil-snipe-s
                 :desc "select word"            "e"   #'my/select-symbol-at-point
                 :desc "select fun(daf, yaf)"   "f"   #'mark-defun
                 :desc "select page"            "p"   #'mark-page
                 :desc "widnow left"            "j" #'evil-window-left
                 :desc "widnow right"           "l" #'evil-window-right
                 :desc "widnow up"              "i" #'evil-window-up
                 :desc "widnow down"            "k" #'evil-window-down
                )
        ;; dap
        "<f3>" #'dap-ui-locals
        "<f4>" #'dap-ui-breakpoints ;; was once eval-buffer-and-close
        "<f5>" #'dap-debug
        "<f6>" #'my/dap-debugger-setting
        "<f7>" #'my/dap-debug-close
        "v"  #'dap-eval
        "bb"  #'dap-breakpoint-add
        "bk"  #'dap-breakpoint-delete ;; k for kill
        "c"  #'dap-continue
        "n"  #'dap-next
        )

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
        "M-RET"     #'newline-and-indent            ;; same as ENTER
        "M-<next>"  #'scroll-up-command             ;; same as PgDn.
        "M-<prior>" #'scroll-down-command           ;; same as PgUp
        "M-DEL"     #'delete-char                   ;; Delete
        "M-;"       (lambda () (interactive) (insert ";"));; same as ;
        "M-/"       #'comment-dwim)

  ;;; Visual State
  (map! :map evil-visual-state-map
        "w"   #'evil-yank
        "y"   #'evil-paste-after
        "i" #'previous-line
        "k" #'next-line
        "j" #'backward-char
        "l" #'forward-char
        "M-i" #'previous-line
        "M-k" #'next-line
        "M-j" #'backward-char
        "M-l" #'forward-char)

  ;;; Emacs State
  (map! :map evil-emacs-state-map
        "C-a" #'evil-exit-emacs-state)

  )

(after! cc-mode
  (map! :map c-mode-base-map
        :ni "C-d" #'consult-lsp-diagnostics
        :ni "C-h k" #'describe-key))  ;; optional



(after! help-mode
  (map! :map help-mode-map
       :n "i" #'previous-line))

(after! image-mode
  (map! :map image-mode-map
        :n "g" #'centaur-tabs-forward
        :n "G" #'centaur-tabs-move-current-tab-to-right
        )
  )

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

(after! evil-snipe
  ;; stop evil-snipe from hijacking `s`/`S`
  (map! :map (evil-snipe-local-mode-map evil-snipe-override-mode-map)
        :n "f" nil
        :n "F" nil
        :v "f" nil
        :v "F" nil
        :m "f" nil
        :m "F" nil
        :n "s" nil
        :n "S" nil
        :v "s" nil
        :v "S" nil))

(provide 'init-keybinds)
;;; init-keybinds.el ends here
