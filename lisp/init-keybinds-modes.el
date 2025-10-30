;;; init-keybinds-modes.el --- Mode-specific keybindings -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

;;; Vterm
(after! vterm
  (set-popup-rule! "^\\*vterm\\*" :size 0.25 :vslot -4 :select t :quit t :ttl 0)
  (with-eval-after-load 'vterm
    (define-key vterm-mode-map (kbd "M-i") #'previous-line)
    (define-key vterm-mode-map (kbd "M-k") #'next-line)
    (define-key vterm-mode-map (kbd "M-j") #'backward-char)
    (define-key vterm-mode-map (kbd "M-l") #'forward-char)
    (define-key vterm-mode-map (kbd "M-u") #'smart-beginning-of-line)
    (define-key vterm-mode-map (kbd "M-o") #'move-end-of-line)
    (define-key vterm-mode-map (kbd "M-U") nil)
    (define-key vterm-mode-map (kbd "M-O") nil)
    (define-key vterm-mode-map (kbd "M-K") nil)
    (define-key vterm-mode-map (kbd "M-I") nil)
    (define-key vterm-mode-map (kbd "M-1") #'+workspace/switch-to-0)
    (define-key vterm-mode-map (kbd "M-2") #'+workspace/switch-to-1)
    (define-key vterm-mode-map (kbd "M-3") #'+workspace/switch-to-2)
    (define-key vterm-mode-map (kbd "M-4") #'+workspace/switch-to-3)
    (define-key vterm-mode-map (kbd "M-w") #'evil-yank)
    (define-key vterm-mode-map (kbd "M-y") #'evil-paste-after)
    (define-key vterm-mode-map (kbd "C-c") (lambda () (interactive) (vterm-send-key "c" nil nil t)))
    (with-eval-after-load 'evil
      (evil-define-key 'insert vterm-mode-map (kbd "C-c") (lambda () (interactive) (vterm-send-key "c" nil nil t))))))

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

;;; Evil Snipe
(after! evil-snipe
  (map! :map (evil-snipe-local-mode-map evil-snipe-override-mode-map)
        :n "f" nil
        :n "F" nil
        :v "f" nil
        :v "F" nil
        :m "f" nil
        :m "F" nil
        :n "s" nil
        :n "S" nil
        :v "s" nil
        :v "S" nil))

;;; C/C++ Mode
(after! cc-mode
  (map! :map c-mode-base-map
        :ni "C-d" #'consult-lsp-diagnostics
        :ni "C-h k" #'describe-key))


(provide 'init-keybinds-modes)
;;; init-keybinds-modes.el ends here
