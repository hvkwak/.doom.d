;;; init-keybinds.el --- Keybindings for Doom Emacs -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;;;

(after! cc-mode
  (map! :map c-mode-base-map
        "C-d" #'consult-lsp-diagnostics))
(define-key key-translation-map (kbd "M-q") (kbd "C-g")) ;; C-g with M-q doom/escape.

(map! :map global-map ;;(c-mode-map c++-mode-map)

      ;;; More M- for convenience
      "M-RET"     #'newline-and-indent            ;; same as ENTER
      "M-<next>"  #'scroll-up-command             ;; same as PgDn.
      "M-<prior>" #'scroll-down-command           ;; same as PgUp
      "M-DEL"     #'delete-char                   ;; Delete
      ;;"M-;" (lambda () (interactive) (insert ";"));; same as ;

      ;;; Line Navigation
      "M-i" #'previous-line
      "M-k" #'next-line
      "M-j" #'backward-char
      "M-l" #'forward-char
      "M-;" #'forward-word
      "M-h" #'backward-word
      "C-<up>"    #'my-previous-3-lines
      "C-<down>"  #'my-next-3-lines

      ;;; Moving around windows(f for Fenster)
      "M-f M-j" #'windmove-left
      "M-f M-l" #'windmove-right
      "M-f M-i" #'windmove-up
      "M-f M-k" #'windmove-down

      ;;; Comment Insert
      "M-/" #'comment-dwim

      ;;; Navigate between tabs or buffers with SHIFT
      "M-8" #'centaur-tabs-forward
      "M-*" #'centaur-tabs-move-current-tab-to-left ;; with SHIFT pressed

      ;;; Home, End
      "<home>" #'smart-beginning-of-line
      "M-u"    #'smart-beginning-of-line
      "M-o"    #'move-end-of-line

      ;;; Jump: <, >
      "M-," #'better-jumper-jump-backward
      "M-." #'better-jumper-jump-forward

      ;;; Save and Undo
      "C-s"   #'save-buffer ;; M-s M-s does it, too
      "C-S-s" #'save-all-c-h-buffers
      "C-z"   #'undo

      ;;; Mouse selection + Shift
      "<S-down-mouse-1>" #'ignore
      "<S-mouse-1>"      #'my/select-to-click

      ;;; Tab extract/jump matching paren
      "M-=" #'centaur-tabs-extract-window-to-new-frame
      "M-9" #'my-jump-matching-paren ;; works well with S-9

      ;;; Noch verfuegbare Tasten mit M-
      ;;   ,  ,  ,  ,  ,  ,  ,
      ;;  a,  ,  ,  , g,
      ;;   ,  ,  ,  ,  ,  ,  ,
      ;;  z, x, c,
      "M-e" #'execute-extended-command ;; same as M-x

      ;;; Copy and yank
      "M-y" #'yank           ;; C-y for yank still works
      "M-w" #'kill-ring-save ;; copy

      ;;; Prefix M-d: deletion
      "M-d M-l" #'kill-whole-line
      "M-d M-d" #'kill-word ;; originally was M-d
      "M-d M-f" #'kill-buffer
      "M-d M-w" #'+workspace/kill

      ;;; Prefix M-s: save, select and switch
      "M-s M-s" #'save-buffer
      "M-s M-e" #'my/select-symbol-at-point
      "M-s M-l" #'my/select-current-line
      "M-s M-f" #'+vertico/switch-workspace-buffer ;; switch to file
      "M-s M-p" #'+workspace/switch-to             ;; switch to project

      ;;; find references
      "M-f M-f" #'my/consult-line-dwim
      "C-f"     #'my/consult-line-dwim
      "M-r"     #'projectile-find-references
      "M-R"     #'consult-ripgrep
      "C-i"     #'consult-imenu

      ;;; Lsp-doc // TODO: Prefix M-p could be more useful than this
      "M-p" #'lsp-ui-doc-toggle
      "M-P" #'lsp-signature-toggle-full-docs ;; with shift, C-S-SPC: lsp-signature-activate

      ;;; Daps
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

      ;;; Toggle between header and source
      "M-t" #'my/toggle-between-header-and-source  ;; toggle between header and source. (include-src)
      "<f12>"   #'lsp-find-definition        ;; toggle between definition and declaration

      ;;; Open echo area
      "C-b" #'view-echo-area-messages     ; open echo area. it is still C-h e
      )



(provide 'init-keybinds)
;;; init-keybinds.el ends here
