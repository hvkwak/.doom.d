;;; init-keybinds.el --- Keybindings for Doom Emacs -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;;;

(define-key key-translation-map (kbd "M-q") (kbd "C-g")) ;; C-g with M-q doom/escape.
(map! :map global-map ;;(c-mode-map c++-mode-map)

      ;; (fest) keep it same as without prefix M-
      "M-RET" #'newline-and-indent            ;; same as ENTER
      "M-<next>"  #'scroll-up-command         ;; same as PgDn
      "M-<prior>" #'scroll-down-command       ;; same as PgUp
      ;;"M-DEL" #'delete-forward-char         ;; No more. kill word backward.
      ;;"M-;" (lambda () (interactive) (insert ";")) ;; Dies auch nicht.

      ;; (fest) navigate lines
      "M-i" #'previous-line
      "M-k" #'next-line
      "M-j" #'backward-char
      "M-l" #'forward-char
      "M-h" #'backward-word
      "M-;" #'forward-word
      ;;"M-h" #'my/c-move-to-prev-arg
      ;;"M-;" #'my/c-move-to-next-arg

      ;; (fest) insert comment
      "M-/" #'comment-dwim

      ;; (fest) C mode indent line/region:
      ;; C-i

      ;; (fest) navigate between buffers
      "M-8"       #'centaur-tabs-backward
      "M-9"       #'centaur-tabs-forward
      "M-*"       #'centaur-tabs-move-current-tab-to-left ;; with SHIFT pressed
      "M-("       #'centaur-tabs-move-current-tab-to-right ;; with SHIFT pressed

      ;; (fest) Prefix M-s!
      "M-s M-j" #'windmove-left ;; moving around windows
      "M-s M-l" #'windmove-right
      "M-s M-i" #'windmove-up
      "M-s M-k" #'windmove-down
      "M-s M-s" #'save-buffer
      "M-s M-e" #'my/select-symbol-at-point ;; select word
      "M-s M-f" #'consult-lsp-diagnostics

      ;; (fest) home, end
      "<home>" #'smart-beginning-of-line ;; home
      "M-u"    #'smart-beginning-of-line ;; home
      "M-o"    #'move-end-of-line ;; end

      ;; (fest) jump
      "M-," #'better-jumper-jump-backward
      "M-." #'better-jumper-jump-forward

      ;; (fest) save and undo
      "C-s" #'save-buffer
      "C-S-s" #'save-all-c-h-buffers
      "C-z" #'undo

      ;; (fest) home, end, mouse selection with shift
      "<S-down-mouse-1>" #'ignore                 ; Ignore the initial mouse down event
      "<S-mouse-1>"      #'my/select-to-click     ; Bind Shift + mouse click to your function;

      ;; Noch verfuegbare Tasten with M-
      ;;   ,  ,  , r,  ,
      ;;   ,  ,  ,  , g,
      ;;   ,  ,  ,  ,  ,  ,  ,
      ;;  z, x, c,
      ;; M-e for M-x!
      ;; M-d is back: kill-word
      "M-e" #'execute-extended-command ;; this was M-x

      ;; vertico/switch-workspace-buffer
      "M-f" #'+vertico/switch-workspace-buffer

      ;; toggle between header and source. (include-src)
      "M-t" #'my/toggle-between-header-and-source

      ;; lsp-ui
      "M-p" #'lsp-ui-doc-toggle
      "M-a" #'lsp-signature-toggle-full-docs ;; C-S-SPC: lsp-signature-activate

      ;; dap debug key bindings
      "<f3>" #'dap-ui-locals
      "<f4>" #'dap-ui-breakpoints
      "<f5>" #'dap-debug
      "<f6>" #'my/dap-debugger-setting
      "<f7>" #'my/dap-debug-close
      "<f8>" #'dap-breakpoint-delete
      "M-v" #'dap-eval
      "M-b" #'dap-breakpoint-add
      "M-n" #'dap-next
      "M-m" #'dap-continue
      ;; "<f4>" #'eval-buffer-and-close ;; for edit-debug-template

      ;; yank
      "M-y" #'yank ;; this was once C-y

      ;; Search Functions - Consult
      "C-f" #'my/consult-line-dwim
      "C-S-f" #'consult-ripgrep

      ;; find definition, find references
      "<f12>" #'lsp-find-definition     ; toggle between definition and deklaration
      "M-r"   #'projectile-find-references ; instead of lsp-find-references

      ;; open echo area
      "C-b" #'view-echo-area-messages     ; open echo area. it is still C-h e
)
(provide 'init-keybinds)
;;; init-keybinds.el ends here
