;;; init-modeline.el --- modeline configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;
;; Doom Modeline visual configuration, custom faces, font scaling,
;; and integration with nerd-icons/all-the-icons.
;;
;;; Code:

;; 1. Mode-line Face Settings
;; Removed the `:height` property that caused vertical alignment issues.
;; Only font family and weight are defined here.
(defun my/set-modeline-faces ()
  (custom-set-faces!
    '(mode-line
      :family "JetBrains Mono"
      :weight medium
      :box (:line-width (1 . 1) :color "#000000" :style nil)
      )
    '(mode-line-inactive
      :family "JetBrains Mono"
      :weight regular
      :box (:line-width (1 . 1) :color "#000000" :style nil))))

;; 2. Doom Modeline Package Configuration
(use-package! doom-modeline
  :init (doom-modeline-mode 1)
  :config
  ;; Dedicated modeline font (controls size and font globally to ensure proper vertical alignment)
  (setq doom-modeline-font (font-spec :family "JetBrains Mono" :size 32 :weight 'medium))

  ;; Modeline height and left bar width (30–32 height works well for a 16px font)
  (setq doom-modeline-height 32)
  (setq doom-modeline-bar-width 4)

  ;; Icons and visual settings
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-unicode-fallback t)

  ;; Buffer path style (.doom.d/lisp/foo.el)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)

  ;; Git / VCS (Magit)
  (setq doom-modeline-vcs-icon t)
  (setq doom-modeline-vcs-max-length 25)

  ;; Flycheck / Syntax Checker
  (setq doom-modeline-check-icon t)
  (setq doom-modeline-check 'auto)

  ;; LSP status display
  (setq doom-modeline-lsp t)
  (setq doom-modeline-lsp-icon t)

  ;; Cursor position and encoding
  (setq doom-modeline-enable-buffer-position t)
  (setq doom-modeline-buffer-encoding t)
  (setq doom-modeline-percent-position '(-3 "%p"))

  ;; Battery status
  (setq doom-modeline-battery nil))

;; Initial execution
(my/set-modeline-faces)

;; Register hooks to preserve font styling across theme switches
(add-hook 'doom-load-theme-hook #'my/set-modeline-faces)
(add-hook 'modus-themes-after-load-theme-hook #'my/set-modeline-faces)

(provide 'init-modeline)
;;; init-modeline.el ends here
