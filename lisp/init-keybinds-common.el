;;; init-keybinds-common.el --- Keybindings for Doom Emacs -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary: common keybinds for init-keybinds-md.el and init-keybinds-org.el
;;; Code:

;; Disable evil-snipe keybindings that conflict with our custom bindings
(after! evil-snipe
  (map! :map (evil-snipe-local-mode-map evil-snipe-override-mode-map)
        :n "f" nil
        :n "F" nil
        :n "s" nil
        :n "S" nil
        :v "s" nil
        :v "S" nil
        :v "f" nil
        :v "F" nil))

(defvar my-common-keys-mode-map (make-sparse-keymap)
  "Keymap for `my-writing-keys-mode'.")

(define-minor-mode my-common-keys-mode
  "Shared writing keys for Org/Markdown (and friends)."
  :init-value nil
  :keymap my-common-keys-mode-map)

;; Use Doom's map! so we can do state-specific keys cleanly.
(with-eval-after-load 'evil
  (map! :map my-common-keys-mode-map
        ;; Multistate
        ;; (normal/insert/visual)
        ;; If you want truly all three states: use evil-define-key on '(normal insert visual)'
        "M-SPC"      (cmd! (ignore))          ;; no cycle-spacing
        "C-w"        #'kill-region
        "<f9>"       #'treemacs
        "M-,"        #'evil-jump-backward
        "M-."        #'evil-jump-forward
        "M-9"        #'my/jump-matching-paren
        "M-8"        #'my/evil-select-inside-paren
        "C-b"        #'view-echo-area-messages
        "C-S-z"      #'undo-fu-only-redo
        "C-z"        #'undo-fu-only-undo
        "M-s M-j"    #'evil-window-left
        "M-s M-l"    #'evil-window-right
        "M-s M-i"    #'evil-window-up
        "M-s M-k"    #'evil-window-down
        "M-s M-e"    #'my/select-symbol-at-point
        "M-s M-d"    #'mark-defun
        "M-s M-p"    #'mark-page
        "M-s M-f"    #'+vertico/switch-workspace-buffer
        "M-s M-s"    #'my/save-and-escape
        "M-="        #'centaur-tabs-extract-window-to-new-frame
        "<S-down-mouse-1>" #'ignore
        "<S-mouse-1>"      #'my/select-to-click
        "<home>"     #'smart-beginning-of-line
        "M-u"        #'smart-beginning-of-line
        "M-o"        #'move-end-of-line
        "C-f"        #'my/consult-line-dwim
        "M-f"        #'my/consult-line-dwim
        "M-r"        #'my/consult-ripgrep-dwim
        "M-'"        #'consult-imenu
        "M-e"        #'execute-extended-command
        "M-h"        nil

        ;; Normal state: hjkl & friends
        :n "i" #'previous-line
        :n "k" #'next-line
        :n "j" #'backward-char
        :n "l" #'forward-char
        :n "u" #'smart-beginning-of-line
        :n "o" #'move-end-of-line
        :n "M-i" #'evil-insert
        :n "M-j" #'backward-char
        :n "M-l" #'forward-char
        :n "M-k" #'next-line
        :n "M-h" (cmd! (message ""))   ;; noop
        :n "w"  #'evil-yank
        :n "y"  #'evil-paste-after
        :n "M-w" #'evil-yank
        :n "M-y" #'evil-paste-after
        :n "M-q" #'evil-escape
        :n "h"  #'centaur-tabs-backward
        :n "g"  #'centaur-tabs-forward
        :n "H"  #'centaur-tabs-move-current-tab-to-left
        :n "G"  #'centaur-tabs-move-current-tab-to-right
        :n "z"  #'undo-fu-only-undo

        ;; Local “s” prefix (save/snipe/switch/select)
        (:prefix ("s" . "save/snipe/switch/select")
         :n "s" #'my/save-and-escape
         :n "f" #'+vertico/switch-workspace-buffer
         :n "n" #'evil-snipe-s
         :n "e" #'my/select-symbol-at-point
         :n "d" #'mark-defun
         :n "p" #'mark-page
         :n "j" #'evil-window-left
         :n "l" #'evil-window-right
         :n "i" #'evil-window-up
         :n "k" #'evil-window-down)

        ;; Insert state (keep typing-friendly)
        :i "M-y"       #'yank
        :i "S-<left>"  nil
        :i "S-<right>" nil
        :i "S-<down>"  nil
        :i "S-<up>"    nil
        :i "M-i"       #'previous-line
        :i "M-k"       #'next-line
        :i "M-j"       #'backward-char
        :i "M-l"       #'forward-char
        :i "M-q"       #'my/insert-escape-and-clear
        :i "M-RET"     #'newline-and-indent
        :i "M-<next>"  #'scroll-up-command
        :i "M-<prior>" #'scroll-down-command
        ;; If you want defaults, comment the next two lines:
        :i "M-DEL"     #'delete-char
        :i "M-;"       (cmd! (insert ";"))
        :i "M-/"       #'comment-dwim

        ;; Visual state
        :v "w"   #'evil-yank
        :v "y"   #'evil-paste-after
        :v "i" #'previous-line
        :v "k" #'next-line
        :v "j" #'backward-char
        :v "l" #'forward-char
        :v "M-i" #'previous-line
        :v "M-k" #'next-line
        :v "M-j" #'backward-char
        :v "M-l" #'forward-char))

;;; Shared setup for writing modes
(defun my-setup-writing-mode (mode-map mode-hook)
  "Configure common writing mode settings for MODE-MAP and enable via MODE-HOOK."
  ;; Enable C-h as help prefix globally
  (keymap-global-set "C-h" help-map)
  (keymap-global-unset "C-z" t)

  ;; Leader key bindings
  (map! :leader
        (:prefix ("k" . "kill")
         :desc "kill current buffer"     "k" #'kill-current-buffer
         :desc "kill current frame"              "f" #'delete-frame
         :desc "kill current workspace(project)" "p" #'+workspace/kill))

  ;; Enable common keybindings (with highest priority via emulation-mode-map-alists)
  (add-hook mode-hook #'my-common-keys-mode))

(provide 'init-keybinds-common)
