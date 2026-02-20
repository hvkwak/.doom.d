;;; init-keybinds-common.el --- Keybindings for Doom Emacs -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;
;; Common keybindings shared across modes (org, markdown, etc.)
;;
;; Movement Model (IJKL instead of HJKL):
;;   i = up       (standard vim: enter insert mode)
;;   k = down     (standard vim: up)
;;   j = left     (standard vim: down)
;;   l = right    (unchanged)
;;   h = tab switching (standard vim: left)
;;
;; Insert mode is accessed via M-i in normal mode.
;;
;; Additional remaps:
;;   w = yank (copy)      y = paste
;;   u = beginning of line   o = end of line
;;   z = undo
;;
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

  ;; Override default Meta bindings with movement keys (due to dape!)
  ;; (M-i=tab-to-tab-stop, M-j=default-indent-new-line, M-k=kill-sentence,
  ;;  M-l=downcase-word, M-u=upcase-word, M-o=facemenu-keymap)
  (define-key global-map (kbd "M-i") #'previous-line)
  (define-key global-map (kbd "M-k") #'next-line)
  (define-key global-map (kbd "M-j") #'backward-char)
  (define-key global-map (kbd "M-l") #'forward-char)
  (define-key global-map (kbd "M-u") #'smart-beginning-of-line)
  (define-key global-map (kbd "M-o") #'move-end-of-line)

  ;; Disable certain Meta keys to prevent accidental triggers
  ;; (M-h=mark-paragraph, M-p/M-t=various)
  (evil-define-key '(normal insert visual replace) global-map
    (kbd "M-h")        #'ignore
    (kbd "M-k")        #'ignore
    (kbd "M-p")        #'ignore
    (kbd "M-t")        #'ignore
    (kbd "M-\\")       (cmd! (insert "\\"))
    (kbd "M-SPC")      (cmd! (insert " "))
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
    (kbd "M-9")        #'my/evil-select-inside-paren
    (kbd "M-0")        #'my/jump-matching-paren
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

;;; More - Evil Mode
(after! evil
  (map! :vnm "i"         #'previous-line
        :vnm "k"         #'next-line
        :vnm "j"         #'backward-char
        :vnm "l"         #'forward-char
        :vm "M-i"       #'previous-line
        :vnm "M-k"       #'next-line
        :vnm "M-j"       #'backward-char
        :vnm "M-l"       #'forward-char
        :nm "h"         #'centaur-tabs-backward
        ;; :nm "g"         #'centaur-tabs-forward
        :nm "H"         #'centaur-tabs-move-current-tab-to-left
        ;; :nm "G"         #'centaur-tabs-move-current-tab-to-right
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
        :n "z"  #'undo-fu-only-undo)

  ;;; Normal prefix "s" prefix
  (map! :map my-common-keys-mode-map
        (:prefix ("s" . "save/snipe/switch/select")
         :desc "save"          :n "s" #'my/save-and-escape
         :desc "switch buffer" :n "b" #'+vertico/switch-workspace-buffer
         :desc "snipe-s"       :n "ns" #'evil-snipe-s
         :desc "snipe-t"       :n "nt" #'evil-snipe-t
         :desc "select word"   :n "e" #'my/select-symbol-at-point
         :desc "select fun"    :n "f" #'mark-defun
         :desc "select page"   :n "p" #'mark-page
         :desc "window left"   :n "j" #'evil-window-left
         :desc "window right"  :n "l" #'evil-window-right
         :desc "window up"     :n "i" #'evil-window-up
         :desc "window down"   :n "k" #'evil-window-down)
        )

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
        :i "M-SPC"     (cmd! (insert " "))
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
        :i "M-;"       (cmd! (insert ";"))
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
        :v "M-l" #'forward-char
        :v "<tab>" #'indent-for-tab-command
        )
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
