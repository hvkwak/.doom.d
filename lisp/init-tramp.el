;;; init-tramp.el --- tramp configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(after! tramp
  (setq tramp-default-method "ssh")
  (setq tramp-verbose 1)
  (setq tramp-use-ssh-controlmaster-options nil)
  (setq tramp-connection-timeout 10)  ; Initial connection timeout
  )

;; TODO: Redefining general--unalias could break silently when general.el updates. This is fragile.
;; define new general--unalias to reduce wrong type argument listp error. this will reduce some waiting time.
(defun general--unalias (thing &optional state-p)
  "Safe general--unalias that guards against nil and bad inputs."
  (message "[GENERAL] unalias: %S (state-p: %S)" thing state-p)
  (let ((aliases (if state-p general-state-aliases general-keymap-aliases)))
    (cond
     ((symbolp thing) (or (cdr (assq thing aliases)) thing))
     ((stringp thing) (or (cdr (assq (intern thing) aliases)) (intern thing)))
     ((null thing) nil)
     (t (progn
          (message "[GENERAL ERROR] invalid input to general--unalias: %S" thing)
          nil)))))

;; disable autosave
(defun my/disable-tramp-autosave-and-lockfiles ()
  "Disable lockfiles and autosave for TRAMP buffers to avoid fallback encoding."
  (when (and buffer-file-name (file-remote-p buffer-file-name))
    (setq-local create-lockfiles nil)
    (setq-local auto-save-default nil)
    (auto-save-mode -1)))
(add-hook 'find-file-hook #'my/disable-tramp-autosave-and-lockfiles)

(provide 'init-tramp)
;;; init-tramp.el ends here
