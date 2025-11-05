;;; init-vterm.el --- Change how vterm works -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(after! vterm
  (set-popup-rule! "^\\*vterm\\*" :size 0.25 :vslot -4 :select t :quit t :ttl 0))
;; (defun my/vterm-init ()
;;   "Automatically source .profile at vterm start."
;;   (vterm-send-string "source ~/.profile" t)
;;   (vterm-send-return))
;; (add-hook 'vterm-mode-hook #'my/vterm-init)


(provide 'init-vterm)
;;; init-vterm.el ends here
