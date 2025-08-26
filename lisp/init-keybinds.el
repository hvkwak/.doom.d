;;; init-keybinds.el --- Keybindings for Doom Emacs -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;;;

(define-key key-translation-map (kbd "M-q") (kbd "C-g")) ;; C-g with M-q
(map! :map global-map ;;(c-mode-map c++-mode-map)

      ;; (fest) keep it same as without prefix M-
      "M-;" (lambda () (interactive) (insert ";"))
      "M-RET" #'newline-and-indent            ;; same as ENTER
      "M-DEL" #'backward-delete-char-untabify ;; same as Backspace

      ;; (fest) navigate lines
      "M-i" #'previous-line
      "M-k" #'next-line
      "M-j" #'backward-char
      "M-l" #'forward-char
      ;;"M-h" #'my/c-move-to-prev-arg
      ;;"M-;" #'my/c-move-to-next-arg

      ;; (fest) insert comment
      "M-/" #'comment-dwim

      ;; (fest) navigate between buffers
      "C-8"       #'switch-to-prev-buffer
      "C-9"       #'switch-to-next-buffer
      "M-8"       #'switch-to-prev-buffer
      "M-9"       #'switch-to-next-buffer

      ;; (fest) Prefix M-s
      "M-s M-j" #'windmove-left ;; moving around windows
      "M-s M-l" #'windmove-right
      "M-s M-i" #'windmove-up
      "M-s M-k" #'windmove-down
      "M-s M-s" #'my/select-symbol-at-point ;; select word
      "M-s M-f" #'flycheck-list-errors

      ;; to delete?
      "C-<left>" #'windmove-left
      "C-<right>" #'windmove-right
      "C-<up>" #'windmove-up
      "C-<down>" #'windmove-down

      ;; (fest) home, end
      "<home>" #'smart-beginning-of-line ;; home
      "M-u"    #'smart-beginning-of-line ;; home
      "M-o"    #'move-end-of-line ;; end

      ;; (fest) jump
      "M-," #'better-jumper-jump-backward
      "M-." #'better-jumper-jump-forward
      "M-<left>" #'better-jumper-jump-backward
      "M-<right>" #'better-jumper-jump-forward

      ;; (fest) save and undo
      "C-s" #'save-buffer
      "C-S-s" #'save-all-c-h-buffers
      "C-z" #'undo

      ;; (fest) home, end, mouse selection with shift
      "<S-down-mouse-1>" #'ignore                 ; Ignore the initial mouse down event
      "<S-mouse-1>"      #'my/select-to-click     ; Bind Shift + mouse click to your function;

      ;; Search Functions - Consult
      "C-f" #'consult-line
      "C-S-f" #'consult-ripgrep

      ;; Noch verfuegbare Tasten with M-
      ;;   ,  ,  ,  , t,
      ;;   ,  ,  ,  , g,
      ;;   ,  ,  ,  ,  ,  ,  ,
      ;;  z, x, c,

      ;; M-x becomes M-d!
      "M-d" #'execute-extended-command ;; this was M-x

      ;; vertico/switch-workspace-buffer
      "M-f" #'+vertico/switch-workspace-buffer

      ;; C-g with M-q
      ;;"M-q" #'doom/escape

      ;; lsp-ui
      "M-a" #'lsp-signature-toggle-full-docs ;; C-S-SPC: lsp-signature-activate
      "M-p" #'lsp-ui-doc-toggle

      ;; debug key bindings
      ;; "<f4>" #'eval-buffer-and-close ;; for edit-debug-template
      "<f5>" #'dap-debug
      "<f6>" #'my/dap-debugger-setting
      "<f7>" #'my/dap-debug-close
      "M-v" #'dap-eval
      "M-b" #'dap-breakpoint-add
      "M-S-b" #'dap-breakpoint-delete
      "M-n" #'dap-next
      "M-m" #'dap-continue

      ;; copy and paste
      "C-c C-c" #'kill-ring-save ;; Ctrl-C
      "C-v" #'yank               ;; Ctrl-V
      "M-e" #'yank

      ;; find definition, header-source toggle
      "<f12>" #'lsp-find-definition     ; toggle between definition and deklaration
      "M-r"   #'lsp-find-references

      ;; open echo area
      "C-b" #'view-echo-area-messages     ; open echo area. it is still C-h e
      )
(provide 'init-keybinds)
;;; init-keybinds.el ends here
