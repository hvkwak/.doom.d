;;; init-keybinds.el --- Keybindings for Doom Emacs -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(map! :map global-map ;;(c-mode-map c++-mode-map)

      ;; lsp-ui
      "M-a" #'lsp-signature-toggle-full-docs ;; C-S-SPC: lsp-signature-activate
      "M-f" #'flycheck-list-errors
      "M-7" #'lsp-ui-doc-toggle
      "M-8" #'my/c-move-to-prev-arg
      "M-9" #'my/c-move-to-next-arg

      ;; (fest) navigate lines
      "M-i" #'previous-line
      "M-k" #'next-line
      "M-j" #'backward-char
      "M-l" #'forward-char

      ;; (fest) moving around windows
      "M-s M-j" #'windmove-left
      "M-s M-l" #'windmove-right
      "M-s M-i" #'windmove-up
      "M-s M-k" #'windmove-down
      "C-<left>" #'windmove-left
      "C-<right>" #'windmove-right
      "C-<up>" #'windmove-up
      "C-<down>" #'windmove-down

      ;; (fest) navigate between buffers
      "C-8"       #'switch-to-prev-buffer
      "C-9"       #'switch-to-next-buffer

      ;; debug key bindings
      "<f4>" #'eval-buffer-and-close ;; for edit-debug-template
      "<f5>" #'dap-debug
      "<f6>" #'my/dap-debugger-setting
      "<f7>" #'my/dap-debug-close
      "M-n" #'dap-next
      "M-m" #'dap-continue
      "M-e" #'dap-eval
      "M-b" #'dap-breakpoint-add
      "M-d" #'dap-breakpoint-delete

      ;; home, end, page up, page down, and delete.
      "<home>" #'smart-beginning-of-line ;; home
      "M-u"    #'smart-beginning-of-line ;; home
      "M-o"    #'move-end-of-line ;; end
      "M-DEL"  #'delete-char ;; delete with backspace

      ;; Search Functions - Consult
      "C-f" #'consult-line
      "C-S-f" #'consult-ripgrep

      ;; jump, copy and paste, and more.
      "C-s" #'save-buffer
      "C-a" #'save-all-c-h-buffers
      "C-z" #'undo                      ;
      "M-," #'better-jumper-jump-backward
      "M-." #'better-jumper-jump-forward
      "M-<left>" #'better-jumper-jump-backward
      "M-<right>" #'better-jumper-jump-forward
      "s-c" #'kill-ring-save ;; SuperL
      "s-v" #'yank

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
