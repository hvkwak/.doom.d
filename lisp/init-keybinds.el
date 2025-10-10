;;; init-keybinds.el --- Keybindings for Doom Emacs -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(keymap-global-set "C-h" help-map) ;; enables C-h everywhere, + combined with init-lsp.el
(keymap-global-unset "C-z" t)

;;; Global Map
(after! evil
  (evil-define-key '(normal insert visual) global-map
    (kbd "C-b")        #'view-echo-area-messages
    (kbd "M-s M-e")    #'my/select-symbol-at-point
    (kbd "C-w")        #'kill-region
    (kbd "M-y")        #'yank
    (kbd "C-S-z")      #'undo-fu-only-redo
    (kbd "C-z")        #'undo-fu-only-undo
    (kbd "M-p")        #'lsp-ui-doc-toggle
    (kbd "M-P")        #'lsp-signature-toggle-full-docs
    (kbd "M-<f12>")    #'my/toggle-between-header-and-source
    (kbd "<f12>")      #'lsp-find-definition
    (kbd "M-s M-s")    #'save-buffer
    (kbd "C-x C-s")    #'save-buffer
    (kbd "C-S-s")      #'save-all-c-h-buffers
    (kbd "M-,")        #'better-jumper-jump-backward
    (kbd "M-.")        #'better-jumper-jump-forward
    (kbd "M-9")        #'my/jump-matching-paren
    (kbd "M-q")        #'doom/escape
    (kbd "<f9>")       #'treemacs
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
    (kbd "M-i")        #'previous-line
    (kbd "M-k")        #'next-line
    (kbd "M-j")        #'backward-char
    (kbd "M-l")        #'forward-char
    (kbd "M-h")        (lambda () (interactive))
    (kbd "M-e")        #'execute-extended-command)
)

(after! evil
  ;;; Normal State: navigate, edit structure, execute commands
  (map! :map evil-normal-state-map
        ;; Tabs Navigation
        "8" #'centaur-tabs-backward
        "9" #'centaur-tabs-forward
        "*" #'centaur-tabs-move-current-tab-to-left
        "(" #'centaur-tabs-move-current-tab-to-right

        (:prefix ("K" . "Kill…")
                 "f" #'kill-buffer
                 "w" #'+workspace/kill)

        ;; switch to file
        "M-s M-f" #'+vertico/switch-workspace-buffer ;; needed for vterm
        "M-s M-j" #'evil-window-left ;; switching windows
        "M-s M-l" #'evil-window-right
        "M-s M-i" #'evil-window-up
        "M-s M-k" #'evil-window-down

        ;; dap
        "<f3>" #'dap-ui-locals
        "<f4>" #'dap-ui-breakpoints ;; was once eval-buffer-and-close
        "<f5>" #'dap-debug
        "<f6>" #'my/dap-debugger-setting
        "<f7>" #'my/dap-debug-close
        "M-v"  #'dap-eval
        "M-b"  #'dap-breakpoint-add
        "M-B"  #'dap-breakpoint-delete
        "M-n"  #'dap-next
        "M-m"  #'dap-continue
        ;;; Normal Mode ends here
        )

  ;;; Insert State
  (map! :map evil-insert-state-map
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
  (define-key vertico-map (kbd "M-q") #'doom/escape))


(provide 'init-keybinds)
;;; init-keybinds.el ends here
