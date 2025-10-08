;;; init-keybinds.el --- Keybindings for Doom Emacs -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
(after! evil
  ;;; Common Keybindings for nie states
  (map! :nie
        "M-q" #'evil-escape
        "C-a" #'evil-emacs-state
        "C-z" #'undo

        ;; navigation
        "M-i" #'previous-line
        "M-k" #'next-line
        "M-j" #'backward-char
        "M-l" #'forward-char

        ;; find references
        "M-'" #'consult-imenu        ;; C-i is for TAB. change to M-'.
        "C-f" #'my/consult-line-dwim ;; Ctrl + f
        "M-f" #'my/consult-line-dwim
        "M-r" #'projectile-find-references
        "M-R" #'consult-ripgrep

        ;; kill/save
        "M-s M-s" #'save-buffer
        ;;"C-x C-s" #'save-buffer ;; exists already
        "C-S-s" #'save-all-c-h-buffers

        ;; jump
        "M-," #'better-jumper-jump-backward
        "M-." #'better-jumper-jump-forward

        ;; home/end
        "<HOME>" #'smart-beginning-of-line ;; Home
        "M-u"    #'smart-beginning-of-line ;; Home
        "M-o"    #'move-end-of-line        ;; End

        ;; lsp ui doc, header toggle, definition
        "M-p"     #'lsp-ui-doc-toggle
        "M-P"     #'lsp-signature-toggle-full-docs ;; with shift, C-S-SPC: lsp-signature-activate
        "M-<f12>" #'my/toggle-between-header-and-source  ;; toggle between header and source. (include-src)
        "<f12>"   #'lsp-find-definition        ;; toggle between definition and declaration

        "M-=" #'centaur-tabs-extract-window-to-new-frame
        "<S-down-mouse-1>" #'ignore
        "<S-mouse-1>"      #'my/select-to-click
        )
  
  ;;; Normal State: navigate, edit structure, execute commands
  (map! :map evil-normal-state-map
        "M-e" #'execute-extended-command ;; one more M-x

        ;; Tabs Navigation
        "j" #'centaur-tabs-backward
        "l" #'centaur-tabs-forward
        "J" #'centaur-tabs-move-current-tab-to-left
        "L" #'centaur-tabs-move-current-tab-to-right

        "kf" #'kill-buffer                     ;; kill buffer
        "kw" #'+workspace/kill                 ;; kill project

        ;; switch to file, project
        "M-s M-f" #'+vertico/switch-workspace-buffer ;; buffer
        "M-s M-p" #'+workspace/switch-to             ;; project
        "M-s M-j" #'evil-window-left ;; switching windows
        "M-s M-l" #'evil-window-right
        "M-s M-i" #'evil-window-up
        "M-s M-k" #'evil-window-down

        ;; Open Echo Area
        "C-b" #'view-echo-area-messages ;; or C-h e

        ;; dap
        "<f3>" #'dap-ui-locals
        "<f4>" #'dap-ui-breakpoints ;; was once eval-buffer-and-close
        "<f5>" #'dap-debug
        "<f6>" #'my/dap-debugger-setting
        "<f7>" #'my/dap-debug-close
        "<f8>" #'dap-breakpoint-delete
        "M-v"  #'dap-eval
        "M-b"  #'dap-breakpoint-add
        "M-n"  #'dap-next
        "M-m"  #'dap-continue
        ;;; Normal Mode ends here
        )

  ;;; Insert State
  (map! :map evil-insert-state-map
        "M-y" #'yank           ;; C-y for yank still works
        "M-w" #'kill-ring-save ;;
        "C-w" #'kill-region

        ;; More M- for convenience
        "M-RET"     #'newline-and-indent            ;; same as ENTER
        "M-<next>"  #'scroll-up-command             ;; same as PgDn.
        "M-<prior>" #'scroll-down-command           ;; same as PgUp
        "M-DEL"     #'delete-char                   ;; Delete
        "M-;" (lambda () (interactive) (insert ";"));; same as ;
        "M-e" #'my/select-symbol-at-point ;; select symbol
        "M-/" #'comment-dwim ;; insert comment
        ;;; Insert Mode ends here
        )

  ;;; Visual State
  (map! :map evil-visual-state-map
        "M-y" #'yank           ;; C-y for yank still works
        "C-w" #'kill-region
        )
)

(after! cc-mode
  (map! :map c-mode-base-map
        "C-d" #'consult-lsp-diagnostics
        ))

;;; move in M-x
(after! vertico
  (map! :map vertico-map
        "M-q" #'evil-escape
        "M-i" #'vertico-previous
        "M-k" #'vertico-next
        "M-j" #'left-char   ; or #'vertico-exit if you prefer
        "M-l" #'right-char)) ; pick anything you like here

(provide 'init-keybinds)
;;; init-keybinds.el ends here
