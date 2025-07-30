;;; init-keybinds.el --- Keybindings for Doom Emacs -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(map! :map global-map ;;(c-mode-map c++-mode-map)

      ;; lsp-ui
      "M-a" #'lsp-signature-toggle-full-docs ;; C-S-SPC: lsp-signature-activate
      "M-7" #'lsp-ui-doc-toggle
      "M-f" #'flycheck-list-errors
      "M-8" #'my/c-move-to-prev-arg
      "M-9" #'my/c-move-to-next-arg

      ;; navigate lines
      "M-i" #'previous-line
      "M-k" #'next-line
      "M-j" #'backward-char
      "M-l" #'forward-char

      ;; moving around windows
      "C-j" #'windmove-left
      "C-l" #'windmove-right
      "C-i" #'windmove-up
      "C-k" #'windmove-down

      ;; navigate between buffers
      "C-8"       #'switch-to-prev-buffer
      "C-9"       #'switch-to-next-buffer

      ;; debug key bindings
      "<f4>" #'eval-buffer-and-close ;; debug template
      "<f5>" #'dap-debug
      "<f6>" #'my/dap-debugger-setting
      "<f7>" #'my/dap-debug-close
      "<f9>" #'dap-next
      "<f10>" #'dap-eval
      "M-<down>" #'dap-breakpoint-add
      "M-<up>" #'dap-breakpoint-delete

      ;; home, end, page up, page down, and delete.
      "<home>" #'smart-beginning-of-line ;; home
      "M-u"    #'smart-beginning-of-line ;; home
      "M-o"    #'move-end-of-line ;; end
      "M-DEL"  #'delete-char ;; delete with backspace
      "M-h"    #'scroll-down ;; Page Up
      "M-n"    #'scroll-up   ;; Page Down

      ;; search functions - consult
      "C-f" #'consult-line
      "C-S-f" #'consult-ripgrep

      ;; jump, copy and paste, and more.
      "C-s" #'save-buffer
      "M-e" #'yank
      "C-z" #'undo                      ;
      "M-," #'better-jumper-jump-backward
      "M-." #'better-jumper-jump-forward

      ;; home, end, mouse selection with shift
      "<S-down-mouse-1>" #'ignore                 ; Ignore the initial mouse down event
      "<S-mouse-1>"      #'my/select-to-click     ; Bind Shift + mouse click to your function;

      ;; find definition, header-source toggle
      "<f12>" #'lsp-find-definition     ; toggle between definition and deklaration
      "M-<f12>"   #'my/toggle-between-header-and-sources

      ;; open echo area
      "C-b" #'view-echo-area-messages     ; open echo area. it is still C-h e
)
(provide 'init-keybinds)
;;; init-keybinds.el ends here
