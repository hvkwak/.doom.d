;;; init-keybinds.el --- Keybindings for Doom Emacs -*- lexical-binding: t; -*-

(map! :map global-map

      ;; lsp-ui
      "M-a" #'lsp-signature-toggle-full-docs ;; C-S-SPC: lsp-signature-activate
      "M-7" #'lsp-ui-doc-toggle
      "M-f" #'flycheck-list-errors

      ;; navigate lines
      "M-i" #'previous-line
      "M-k" #'next-line
      "M-j" #'backward-char
      "M-l" #'forward-char

      ;; navigate between buffers
      "M-8"       #'switch-to-prev-buffer
      "M-9"       #'switch-to-next-buffer
      "C-8"       #'switch-to-prev-buffer
      "C-9"       #'switch-to-next-buffer
      "C-1"       #'switch-to-prev-buffer
      "C-2"       #'switch-to-next-buffer
      "C-<left>"  #'switch-to-prev-buffer
      "C-<right>" #'switch-to-next-buffer
      "C-<tab>" #'+vertico/switch-workspace-buffer ;; same as C-x-b

      ;; moving around windows
      "C-j" #'windmove-left
      "C-l" #'windmove-right
      "C-i" #'windmove-up
      "C-k" #'windmove-down

      ;; debug key bindings
      "C-e" #'eval-buffer-and-close ;; debug template
      "<f5>" #'my-dap-debug
      "<f6>" #'my-dap-debugger-setting
      "<f8>" #'my-dap-debug-close
      "<f9>" #'+treemacs/toggle ;; it is already <f9>. keep it this way.

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
      "M-<right>" #'better-jumper-jump-forward
      "M-<left>" #'better-jumper-jump-backward

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
