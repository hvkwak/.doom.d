;;; init-keybinds-modes.el --- Mode-specific keybindings -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary: keybindings for other modes
;;; Code:

;;; Dired
(after! dired
  (map! :map dired-mode-map
        :n "i" #'dired-previous-line
        :n "k" #'dired-next-line
        :n "j" #'dired-up-directory
        :n "l" #'dired-find-file
        :i "M-i" #'dired-previous-line
        :i "M-k" #'dired-next-line
        :i "M-j" #'dired-up-directory
        :i "M-l" #'dired-find-file
        )
  )

;;; Vterm
(after! vterm

  (map! :leader
        "o t" nil)

  (map! :map vterm-mode-map
        :n "M-i" #'evil-insert
        )

  (evil-define-key 'insert vterm-mode-map (kbd "M-i") #'previous-line)
  (evil-define-key 'insert vterm-mode-map (kbd "M-k") #'next-line)
  (evil-define-key 'insert vterm-mode-map (kbd "M-j") #'backward-char)
  (evil-define-key 'insert vterm-mode-map (kbd "M-l") #'forward-char)
  (evil-define-key 'insert vterm-mode-map (kbd "M-u") #'smart-beginning-of-line)
  (evil-define-key 'insert vterm-mode-map (kbd "M-o") #'move-end-of-line)
  (evil-define-key 'insert vterm-mode-map (kbd "M-U") nil)
  (evil-define-key 'insert vterm-mode-map (kbd "M-O") nil)
  (evil-define-key 'insert vterm-mode-map (kbd "M-K") nil)
  (evil-define-key 'insert vterm-mode-map (kbd "M-I") nil)
  (evil-define-key 'insert vterm-mode-map (kbd "M-1") #'+workspace/switch-to-0)
  (evil-define-key 'insert vterm-mode-map (kbd "M-2") #'+workspace/switch-to-1)
  (evil-define-key 'insert vterm-mode-map (kbd "M-3") #'+workspace/switch-to-2)
  (evil-define-key 'insert vterm-mode-map (kbd "M-4") #'+workspace/switch-to-3)
  (evil-define-key 'insert vterm-mode-map (kbd "M-w") #'evil-yank)
  (evil-define-key 'insert vterm-mode-map (kbd "M-y") #'evil-paste-after)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c") (lambda () (interactive) (vterm-send-key "c" nil nil t))))

;;; Help Mode
(after! help-mode
  (map! :map help-mode-map
        :n "i" #'previous-line))

;;; Image Mode
(after! image-mode
  (map! :map image-mode-map
        :n "g" #'centaur-tabs-forward
        :n "G" #'centaur-tabs-move-current-tab-to-right))

;;; Vertico
(after! vertico
  (define-key vertico-map (kbd "M-i") #'vertico-previous)
  (define-key vertico-map (kbd "M-k") #'vertico-next)
  (define-key vertico-map (kbd "M-j") #'backward-char)
  (define-key vertico-map (kbd "M-l") #'forward-char)
  (define-key vertico-map (kbd "M-u") #'smart-beginning-of-line)
  (define-key vertico-map (kbd "M-o") #'move-end-of-line)
  (define-key vertico-map (kbd "M-q") #'evil-escape))

;;; evil-snipe (disable)
;; keybindings that conflict with our custom bindings
(after! evil-snipe
  (map! :map (evil-snipe-local-mode-map evil-snipe-override-mode-map)
        :nvm "f" nil
        :nvm "F" nil
        :nvm "s" nil
        :nvm "S" nil
        :nvm "s" nil
        :nvm "S" nil
        :nvm "f" nil
        :nvm "F" nil))

;;; C/C++ Mode
(after! cc-mode
  (map! :map c-mode-base-map
        :ni "C-d" #'consult-lsp-diagnostics
        :ni "C-h k" #'describe-key))

(after! company
  (define-key company-active-map (kbd "M-i") #'company-select-previous)
  (define-key company-active-map (kbd "M-k") #'company-select-next)
  (define-key company-active-map (kbd "M-q") #'company-abort) ;; combine with my M-q?
  ;; (define-key company-active-map (kbd "RET") #'company-complete-selection)
  ;; (define-key company-active-map (kbd "<return>") #'company-complete-selection)
  (define-key company-active-map (kbd "RET") #'my/company-accept-and-trim-duplicate) ;; instead of company-complete-selection
  (define-key company-active-map (kbd "<return>") #'my/company-accept-and-trim-duplicate)
  (define-key company-active-map (kbd "TAB") #'company-select-next)
  (define-key company-active-map (kbd "<tab>") #'company-select-next)
  (define-key company-active-map (kbd "S-TAB") #'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") #'company-select-previous))

(add-hook 'rg-mode-hook
  (lambda ()
    ;;(switch-to-buffer-other-window (current-buffer))
    (define-key compilation-mode-map       (kbd "RET") #'my/rg-goto-and-quit)
    (define-key compilation-minor-mode-map (kbd "RET") #'my/rg-goto-and-quit)
    (define-key compilation-button-map     (kbd "RET") #'my/rg-goto-and-quit)
    (define-key compilation-mode-map       (kbd "q")   #'my/rg-quit-and-kill)
    (define-key compilation-minor-mode-map (kbd "q")   #'my/rg-quit-and-kill)
    (define-key compilation-button-map     (kbd "q")   #'my/rg-quit-and-kill)
    ))

(after! yasnippet
  (define-key yas-keymap (kbd "TAB")       #'yas-next-field)
  (define-key yas-keymap (kbd "<tab>")     #'yas-next-field)
  (define-key yas-keymap (kbd "S-TAB")     #'yas-prev-field)
  (define-key yas-keymap (kbd "<backtab>") #'yas-prev-field))

(provide 'init-keybinds-modes)
;; ;;; init-keybinds-modes.el ends here
