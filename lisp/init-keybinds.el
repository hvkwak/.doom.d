;;; init-keybinds.el --- Keybindings for Doom Emacs -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(keymap-global-set "C-h" help-map) ;; enables C-h everywhere, combined with init-lsp.el
(keymap-global-unset "C-z" t)
(with-eval-after-load 'evil
  (define-key general-override-mode-map (kbd "C-h") help-map) ;; Top-priority override, if you use general.el
  (define-key evil-motion-state-map (kbd "C-z") #'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "C-z") #'undo-fu-only-undo)
  (define-key evil-insert-state-map (kbd "C-z") #'undo-fu-only-undo)
  (define-key evil-visual-state-map (kbd "C-z") #'undo-fu-only-undo)
  (define-key evil-motion-state-map (kbd "C-S-z") #'undo-fu-only-redo)
  (define-key evil-normal-state-map (kbd "C-S-z") #'undo-fu-only-redo)
  (define-key evil-insert-state-map (kbd "C-S-z") #'undo-fu-only-redo)
  (define-key evil-visual-state-map (kbd "C-S-z") #'undo-fu-only-redo)
  (define-key evil-normal-state-map (kbd "C-f") #'my/consult-line-dwim) ;; keep it here
  (define-key evil-insert-state-map (kbd "C-f") #'my/consult-line-dwim) ;; ok this, too.
  )

(after! evil
  ;; 1) Make C-h a prefix everywhere (which-key will show the menu)
  (keymap-global-set "C-h" help-map)   ;; or (global-set-key (kbd "C-h") help-map)

  ;;; Doom's leader key system SPC to switch
  (map! :leader
        )
  ;;; Common Keybindings for niv states
  (map! :map override
        :niv
        "M-9"  #'my/jump-matching-paren
        "<f9>" #'treemacs
        "M-q"  #'evil-escape
        "C-a"  #'evil-emacs-state

        ;; navigation
        "M-i" #'previous-line
        "M-k" #'next-line
        "M-j" #'backward-char
        "M-l" #'forward-char

        ;; find references
        "M-'" #'consult-imenu        ;; C-i is for TAB. change to M-'.
        "M-f" #'my/consult-line-dwim
        "M-r" #'projectile-find-references
        "M-R" #'consult-ripgrep

        ;; kill/save
        "M-s M-s" #'save-buffer
        "C-x C-s" #'save-buffer
        "C-S-s" #'save-all-c-h-buffers ;; c mode

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
        ;; Another M-x for normal state
        "M-e" #'execute-extended-command

        ;; Tabs Navigation
        "j" #'centaur-tabs-backward
        "l" #'centaur-tabs-forward
        "J" #'centaur-tabs-move-current-tab-to-left
        "L" #'centaur-tabs-move-current-tab-to-right

        "kf" #'kill-buffer                     ;; kill buffer
        "kw" #'+workspace/kill                 ;; kill project

        ;; switch to file, project
        "M-s M-f" #'+vertico/switch-workspace-buffer ;; needed for vterm
        ;;"M-s M-p" #'+workspace/switch-to             ;; project
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
        "M-e" #'execute-extended-command
        "M-s M-e" #'my/select-symbol-at-point ;; select symbol

        ;; More M- for convenience
        "M-RET"     #'newline-and-indent            ;; same as ENTER
        "M-<next>"  #'scroll-up-command             ;; same as PgDn.
        "M-<prior>" #'scroll-down-command           ;; same as PgUp
        "M-DEL"     #'delete-char                   ;; Delete
        "M-;" (lambda () (interactive) (insert ";"));; same as ;
        "M-/" #'comment-dwim ;; insert comment
        ;;; Insert Mode ends here
        )

  ;;; Visual State
  (map! :map evil-visual-state-map
        "M-y" #'yank           ;; C-y for yank still works
        "C-w" #'kill-region
        )
  (map! :map evil-emacs-state-map
        "C-a" #'evil-exit-emacs-state
        )

  )

;;; you serious? this complicated for navigation in treemacs?
(after! (treemacs evil-collection)
  ;; Works even if treemacs-evil isn't present
  (map! :map treemacs-mode-map
        "M-i" #'previous-line
        "M-k" #'next-line
        "M-j" #'backward-char
        "M-l" #'forward-char
        "M-s M-l" #'evil-window-right)
  ;; If treemacs-evil is enabled, bind there too
  (when (boundp 'evil-treemacs-state-map)
    (map! :map evil-treemacs-state-map
        "M-i" #'previous-line
        "M-k" #'next-line
        "M-j" #'backward-char
        "M-l" #'forward-char
        "M-s M-l" #'evil-window-right))
  )

(after! (evil cc-mode)
  (map! :map c-mode-base-map
        :ni "C-d" #'consult-lsp-diagnostics
        :ni "C-h k" #'describe-key))  ;; optional

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
