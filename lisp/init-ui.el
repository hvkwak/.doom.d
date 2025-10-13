;;; init-ui.el --- Change how UI looks -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
(setq doom-theme 'doom-moonlight)
(setq doom-font (font-spec :family "JetBrains Mono" :size 20))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(menu-bar-mode 1)

(custom-set-faces
  '(hl-line ((t (:background "#3e4451" :underline nil))))
  ;;'(cursor ((t (:background "green")))) ;; change cursor background to green.
  '(region ((t (:background "#4671d5" :foreground "#ffffff"))))  ; Region selection
  '(centaur-tabs-selected ((t (:background "#2b2b2b" :foreground "#c3e88d" :weight bold))))
  '(centaur-tabs-selected-modified ((t (:background "#2b2b2b" :foreground "#c3e88d" :weight bold))))
  '(centaur-tabs-unselected ((t (:background "#2b2b2b" :foreground "#888888"))))
  '(centaur-tabs-unselected-modified ((t (:background "#2b2b2b" :foreground "#888888"))))
  )

;; keyboard cursor - configured via evil-cursor specs
(setq-default cursor-type 'bar)
(blink-cursor-mode 1)

;; Different cursor appearance for Evil modes using evil's cursor system
(after! evil
  (setq ;;evil-normal-state-cursor   '(box "#d0d0d0")
        evil-insert-state-cursor   '(bar "green")
        evil-visual-state-cursor   '(box "#c3e88d")
        evil-replace-state-cursor  '(hbar "orange")
        evil-operator-state-cursor '(hollow "#d0d0d0")
        evil-motion-state-cursor   '(box "#d0d0d0")
        evil-emacs-state-cursor    '(bar "cyan")))

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
                      :background "#2f3e5e" :foreground 'unspecified' :weight 'bold)
  (set-face-attribute 'lsp-face-highlight-read nil
                      :background "#3e4d6c" :foreground 'unspecified' :weight 'bold)
  (set-face-attribute 'lsp-face-highlight-write nil
                      :background "#5e4c6e" :foreground 'unspecified' :weight 'bold))

(after! which-key
  (setq which-key-popup-type 'minibuffer ;; better than side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.25
        which-key-side-window-max-width 0.33
        which-key-min-display-lines 6)
)

(after! treemacs
  ;; follow the current project root
  (treemacs-project-follow-mode 1)
  ;; show indent guides
  (treemacs-indent-guide-mode 1)

  ;; same icon for include as src
  (treemacs-modify-theme "Default"
  :icon-directory (treemacs-join-path treemacs-dir "icons/default/svgrepo")
  :config
  (progn
    (treemacs-create-icon :file "dir-src-closed.png" :extensions ("include-closed"))
    (treemacs-create-icon :file "dir-src-open.png"   :extensions ("include-open"))
    )
  )
)




(provide 'init-ui)
;;; init-ui.el ends here
