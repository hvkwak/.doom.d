;;; init-keybinds.el --- Keybindings for Doom Emacs -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;;;

(define-key key-translation-map (kbd "M-q") (kbd "C-g")) ;; C-g with M-q doom/escape.
(map! :map global-map ;;(c-mode-map c++-mode-map)

      ;; (fest) keep it same as without prefix M-, because M- is kept pushed.
      "M-RET"     #'newline-and-indent            ;; same as ENTER
      "M-<next>"  #'scroll-up-command             ;; same as PgDn.
      "M-<prior>" #'scroll-down-command           ;; same as PgUp
      "M-DEL"     #'backward-delete-char-untabify ;; same as Backspace
      ;;"M-;" (lambda () (interactive) (insert ";"));; same as ;

      ;; (fest) navigate lines
      "M-;" #'forward-word
      "M-h" #'backward-word
      "M-i" #'previous-line
      "M-k" #'next-line
      "M-j" #'backward-char
      "M-l" #'forward-char
      "C-<up>"  #'my-previous-3-lines
      "C-<down>"  #'my-next-3-lines

      ;; (fest) insert comment
      "M-/" #'comment-dwim

      ;; (fest) C mode indent line/region:
      ;; C-i ;; already there.

      ;; (fest) navigate between buffers, or kill
      "M-8"       #'centaur-tabs-backward
      "M-9"       #'centaur-tabs-forward
      "M-0"       #'kill-buffer
      "M-*"       #'centaur-tabs-move-current-tab-to-left ;; with SHIFT pressed
      "M-("       #'centaur-tabs-move-current-tab-to-right ;; with SHIFT pressed

      ;; (fest) home, end
      "<home>" #'smart-beginning-of-line ;; home
      "M-u"    #'smart-beginning-of-line ;; home
      "M-o"    #'move-end-of-line ;; end

      ;; (fest) jump
      "M-," #'better-jumper-jump-backward
      "M-." #'better-jumper-jump-forward

      ;; (fest) save and undo
      "C-s"   #'save-buffer
      "C-S-s" #'save-all-c-h-buffers
      "C-z"   #'undo

      ;; (fest) home, end, mouse selection with shift
      "<S-down-mouse-1>" #'ignore                 ; Ignore the initial mouse down event
      "<S-mouse-1>"      #'my/select-to-click     ; Bind Shift + mouse click to your function;

      ;; (fest) noch was
      "M-=" #'centaur-tabs-extract-window-to-new-frame
      "M-[" #'my-jump-matching-paren

      ;; Moving around windows with M-w
      "M-w M-j" #'windmove-left ;; moving around windows
      "M-w M-l" #'windmove-right
      "M-w M-i" #'windmove-up
      "M-w M-k" #'windmove-down

      ;; Noch verfuegbare Tasten with M-
      ;;   ,  ,  ,  ,  ,
      ;;  a,  ,  , f, g,
      ;;   ,  ,  ,  ,  ,  ,  ,
      ;;  z, x, c,
      "M-e" #'execute-extended-command ;; same as M-x

      ;; copy and yank
      "M-y" #'yank           ;; C-y for yank still works
      "M-r" #'kill-ring-save ;; copy

      ;; Prefix M-s
      "M-s M-s" #'save-buffer
      "M-s M-w" #'my/select-symbol-at-point ;; select word
      "M-s M-l" #'my/select-current-line
      "M-s M-k" #'kill-line
      "M-s M-d" #'consult-lsp-diagnostics
      "M-s M-i" #'consult-imenu
      "M-s M-f" #'+vertico/switch-workspace-buffer ;; changes buffer

      ;; lsp-ui
      "M-p" #'lsp-ui-doc-toggle
      "C-p" #'lsp-signature-toggle-full-docs ;; C-S-SPC: lsp-signature-activate

      ;; daps
      "<f3>" #'dap-ui-locals
      "<f4>" #'dap-ui-breakpoints ;; was once eval-buffer-and-close
      "<f5>" #'dap-debug
      "<f6>" #'my/dap-debugger-setting
      "<f7>" #'my/dap-debug-close
      "<f8>" #'dap-breakpoint-delete
      "M-v" #'dap-eval
      "M-b" #'dap-breakpoint-add
      "M-n" #'dap-next
      "M-m" #'dap-continue

      ;; C: consult search functions, find definition, references
      "C-f"   #'my/consult-line-dwim
      "C-S-f" #'consult-ripgrep
      "C-r"   #'projectile-find-references         ;; instead of lsp-find-references
      "<f12>" #'lsp-find-definition                ;; toggle between definition and deklaration

      ;; toggle between header and source
      "M-t" #'my/toggle-between-header-and-source  ;; toggle between header and source. (include-src)

      ;; open echo area
      "C-b" #'view-echo-area-messages     ; open echo area. it is still C-h e
)
(provide 'init-keybinds)
;;; init-keybinds.el ends here
