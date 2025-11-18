;;; init-keybinds-common.el --- Keybindings for Doom Emacs -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; common keybinds for init-keybinds-md.el and init-keybinds-org.el
;;; Code:

;;; Minor Mode Definition
;; Minor mode that holds all shared keys
(defvar my-common-keys-mode-map (make-sparse-keymap)
  "Keymap for `my-common-keys-mode'.")

(define-minor-mode my-common-keys-mode
  "Shared common keys for Evil / Org / Markdown (and friends)."
  :init-value nil
  :keymap my-common-keys-mode-map)

;; Optional helper so you can reuse it in hooks
(defun my-enable-common-keys ()
  (my-common-keys-mode 1))


;;; Globals
(after! evil
  ;; set help-map and unset undo
  (keymap-global-set "C-h" help-map)
  (keymap-global-unset "C-z" t)

  ;; Leader Keys
  ;; unset M-SPC key in insert mode
  ;; doom-leader-alt-key in insert-state M-SPC is now "\\"
  ;; doom-localleader-alt-key in insert-state M-SPC m is now "C-\\"
  ;; inserting "\\" is M-\\
  (setq doom-leader-alt-key "\\")
  (setq doom-localleader-alt-key "C-\\")

  (map! :leader
        "k" nil
        "o" nil
        )

  (map! :leader
        (:prefix ("k" . "kill")
         :desc "kill current buffer"             "k" #'kill-current-buffer
         :desc "kill frame"                      "f" #'delete-frame
         :desc "kill current workspace(project)" "w" #'+workspace/kill))

  (map! :g "M-q" #'doom/escape
        :g "M-y" #'yank)

  (evil-define-key '(normal insert visual replace) global-map
    (kbd "M-h")        (lambda () (interactive) ())
    (kbd "M-k")        (lambda () (interactive) ())
    (kbd "M-p")        (lambda () (interactive) ())
    (kbd "M-t")        (lambda () (interactive) ())
    (kbd "M-\\")       (lambda () (interactive) (insert "\\"))
    (kbd "M-SPC")      (lambda () (interactive) (insert " "))
    (kbd "C-w")        #'kill-region
    (kbd "M-m")        #'my-defun-sig-header-mode
    (kbd "M-M")        #'beginning-of-defun
    (kbd "M-a")        #'lsp-ui-doc-toggle
    (kbd "M-A")        #'lsp-signature-toggle-full-docs
    (kbd "M-<f12>")    #'my/toggle-between-header-and-source
    (kbd "<f12>")      #'lsp-find-definition
    (kbd "<f9>")       #'treemacs
    (kbd "M-,")        #'evil-jump-backward
    (kbd "M-.")        #'evil-jump-forward
    (kbd "M-8")        #'my/evil-select-inside-paren
    (kbd "M-9")        #'my/jump-matching-paren
    (kbd "C-b")        #'view-echo-area-messages
    (kbd "C-S-z")      #'undo-fu-only-redo
    (kbd "C-z")        #'undo-fu-only-undo
    (kbd "M-s M-j")    #'evil-window-left
    (kbd "M-s M-l")    #'evil-window-right
    (kbd "M-s M-i")    #'evil-window-up
    (kbd "M-s M-k")    #'evil-window-down
    (kbd "M-s M-e")    #'my/select-symbol-at-point
    (kbd "M-s M-f")    #'mark-defun
    (kbd "M-s M-p")    #'mark-page
    (kbd "M-s M-b")    #'+vertico/switch-workspace-buffer
    (kbd "M-s M-s")    #'my/save-and-escape
    (kbd "M-=")        #'centaur-tabs-extract-window-to-new-frame
    (kbd "<S-down-mouse-1>") #'ignore
    (kbd "<S-mouse-1>")      #'my/select-to-click
    (kbd "<home>")     #'smart-beginning-of-line
    (kbd "M-u")        #'smart-beginning-of-line
    (kbd "M-o")        #'move-end-of-line
    (kbd "C-f")        #'my/consult-line-dwim
    (kbd "M-f")        #'my/consult-line-dwim
    (kbd "M-r")        #'rg-dwim
    (kbd "M-'")        #'consult-imenu
    (kbd "M-e")        #'execute-extended-command)
  )

;;; Evil Mode - exceptions.
(after! evil
  (map! :nm "i" #'previous-line
        :nm "k" #'next-line
        :nm "j" #'backward-char
        :nm "l" #'forward-char
        )

  (map! :map evil-motion-state-map
        "<C-i>"  nil ;; disable to use C-i evil-scroll-up
        )
  )

(after! evil

  ;;; Normal
  (map! :map my-common-keys-mode-map
        :nm "u" #'smart-beginning-of-line
        :nm "o" #'move-end-of-line
        :n "M-i" #'evil-insert
        :n "w"  #'evil-yank
        :n "y"  #'evil-paste-after
        :n "M-w" #'evil-yank
        :n "M-y" #'evil-paste-after
        :n "M-q" #'evil-escape
        :n "M-j"  #'centaur-tabs-backward
        :n "M-l"  #'centaur-tabs-forward
        :n "M-J"  #'centaur-tabs-move-current-tab-to-left
        :n "M-L"  #'centaur-tabs-move-current-tab-to-right
        :n "z"  #'undo-fu-only-undo)

  ;;; Normal prefix "s" prefix
  (map! :map my-common-keys-mode-map
        (:prefix ("s" . "save/snipe/switch/select")
         :desc "save"          :n "s" #'my/save-and-escape
         :desc "switch buffer" :n "b" #'+vertico/switch-workspace-buffer
         :desc "snipe-s"       :n "n" #'evil-snipe-s
         :desc "snipe-t"       :n "n" #'evil-snipe-t
         :desc "select wrod"   :n "e" #'my/select-symbol-at-point
         :desc "select fun"    :n "f" #'mark-defun
         :desc "select page"   :n "p" #'mark-page
         :desc "window left"   :n "j" #'evil-window-left
         :desc "window right"  :n "l" #'evil-window-right
         :desc "window up"     :n "i" #'evil-window-up
         :desc "window down"   :n "k" #'evil-window-down))

  ;;; Motion
  (map! :map my-common-keys-mode-map
        :nm "h" nil
        :nm "k" #'next-line
        :nm "<C-i>" nil
        :nm "C-i" #'evil-scroll-up
        :nm "C-k" #'evil-scroll-down)

  ;;; Insert
  (map! :map my-common-keys-mode-map
        :i "S-<left>"  nil
        :i "S-<right>" nil
        :i "S-<down>"  nil
        :i "S-<up>"    nil
        :i "M-SPC"     (lambda () (interactive) (insert " "))
        :i "C-SPC"     #'set-mark-command
        :i "M-y"       #'yank
        :i "M-i"       #'previous-line
        :i "M-k"       #'next-line
        :i "M-j"       #'backward-char
        :i "M-l"       #'forward-char
        :i "M-q"       #'my/insert-escape-and-clear
        :i "M-RET"     #'newline-and-indent
        :i "M-<next>"  #'scroll-up-command
        :i "M-<prior>" #'scroll-down-command
        :i "M-DEL"     #'delete-char
        :i "M-;"       (lambda () (interactive) (insert ";"))
        :i "M-/"       #'comment-dwim)

  ;;; Visual
  (map! :map my-common-keys-mode-map
        :v "u"   #'smart-beginning-of-line
        :v "o"   #'move-end-of-line
        :v "w"   #'kill-ring-save
        :v "y"   #'evil-paste-after
        :v "i"   #'previous-line
        :v "k"   #'next-line
        :v "j"   #'backward-char
        :v "l"   #'forward-char
        :v "M-i" #'previous-line
        :v "M-k" #'next-line
        :v "M-j" #'backward-char
        :v "M-l" #'forward-char)
  )

;;; emacs-state
(after! evil
  (map! :map evil-emacs-state-map
        "C-a" #'evil-exit-emacs-state))

;;; ENABLE common keys!
(after! evil
  ;;; add hook to use common-keys
  (add-hook 'evil-local-mode-hook #'my-enable-common-keys)
  )

(provide 'init-keybinds-common)
;;; init-keybinds-common.el ends here
