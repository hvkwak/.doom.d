;;; init-ui.el --- Change how UI looks -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(setq doom-theme 'doom-moonlight)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(menu-bar-mode 1)

;; directly modify Doom's theme settings using custom-set-faces
;; (after! solaire-mode
;;   (solaire-global-mode -1))

(custom-set-faces
  '(hl-line ((t (:background "#3e4451" :underline nil))))
  '(cursor ((t (:background "green")))) ;; change cursor background to green.
  '(region ((t (:background "#4671d5" :foreground "#ffffff"))))  ; Region selection
  )

;; keyboard cursor
(setq-default cursor-type 'bar)
(blink-cursor-mode 1)

;; mouse cursor
(setq-default void-text-area-pointer 'nil)
(setq scroll-preserve-screen-position nil)

;; Scroll one line at a time
(setq mouse-wheel-scroll-amount '(4 ((shift) . 8)))  ; 2 lines at a time, +shift 4 lines.

;; Do not accelerate scrolling
(setq mouse-wheel-progressive-speed nil)  ; Do not accelerate scrolling

(after! lsp-mode
  ;; Soft background highlights for readability
  (set-face-attribute 'lsp-face-highlight-textual nil
                      :background "#2f3e5e" :foreground nil :weight 'bold)
  (set-face-attribute 'lsp-face-highlight-read nil
                      :background "#3e4d6c" :foreground nil :weight 'bold)
  (set-face-attribute 'lsp-face-highlight-write nil
                      :background "#5e4c6e" :foreground nil :weight 'bold))

(after! which-key
  (setq which-key-popup-type 'minibuffer ;; better than side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.25
        which-key-side-window-max-width 0.33
        which-key-min-display-lines 6))

(after! treemacs
  (setq treemacs-project-follow-mode t)
  (treemacs-follow-mode t))

(provide 'init-ui)
;;; init-ui.el ends here
