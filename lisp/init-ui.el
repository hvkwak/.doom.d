;;; init-ui.el --- UI appearance configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;
;; Visual configuration: theme, fonts, cursors, faces, scrolling behavior,
;; and UI customizations for LSP, which-key, and treemacs.
;;
;;; Code:

;;; Theme Subs
;; (load-file (expand-file-name "themes/leuven-theme.el" doom-user-dir))
;; (load-file (expand-file-name "themes/sunny-day-theme.el" doom-user-dir))
;; (load-file (expand-file-name "themes/aalto-light-theme-source-code.el" doom-user-dir))
;; (setq doom-theme 'leuven)
;; (setq leuven-scale-outline-headlines nil)
;; (setq leuven-scale-org-document-title nil)
;; (setq doom-theme 'aalto-light)
;; (setq doom-theme 'doom-one-light)
;; (setq doom-theme 'doom-solarized-light)


;;; Fonts
;; (setq doom-font (font-spec :family "JetBrains Mono" :size 28.0 )) ;
(setq doom-font (font-spec :family "JetBrains Mono" :size 18 :weight 'bold))


;;; Display Settings

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(menu-bar-mode -1)


;;; Theme
;; (add-to-list 'custom-theme-load-path (expand-file-name "themes/" doom-user-dir))
;; (setq doom-theme 'professional)

;;; Face Customization
;; (custom-set-faces
;;   '(hl-line ((t (:background "#e2e6ec" :underline nil))))
;;   '(header-line ((t (:foreground "#383a42" :weight bold))))
;;   ;; '(cursor ((t (:background "#202124")))) ;; change cursor background to green.
;;   '(region ((t (:background "#4671d5" :foreground "#ffffff"))))  ; Region selection
;;   '(centaur-tabs-selected ((t (:background "#eaeef2" :foreground "#0f66c8" :weight bold))))
;;   '(centaur-tabs-selected-modified ((t (:background "#eaeef2" :foreground "#0f66c8" :weight bold))))
;;   '(centaur-tabs-unselected ((t (:background "#eaeef2" :foreground "#888888"))))
;;   '(centaur-tabs-unselected-modified ((t (:background "#eaeef2" :foreground "#888888"))))

;;   ;; this works good with professional
;;   '(doom-modeline-buffer-modified ((t (:foreground "#666666" :weight bold))))
;;   '(doom-modeline-buffer-path ((t (:foreground "#00aa00" :weight bold))))
;;   '(doom-modeline-project-dir ((t (:foreground "#57b2ec" :weight bold))))
;;   '(doom-modeline-warning ((t (:foreground "#aa0000" :weight bold))))
;;   '(show-paren-match ((t (:foreground "#e45649" :background "#f0f0f0" :weight bold))))
;;   '(font-lock-string-face ((t (:foreground "#00AA00")))) ;; changes colors of string
;;   )

(use-package modus-themes
  :ensure t
  :demand t
  :init
  ;; Starting with version 5.0.0 of the `modus-themes', other packages
  ;; can be built on top to provide their own "Modus" derivatives.
  ;; For example, this is what I do with my `ef-themes' and
  ;; `standard-themes' (starting with versions 2.0.0 and 3.0.0,
  ;; respectively).
  ;;
  ;; The `modus-themes-include-derivatives-mode' makes all Modus
  ;; commands that act on a theme consider all such derivatives, if
  ;; their respective packages are available and have been loaded.
  ;;
  ;; Note that those packages can even completely take over from the
  ;; Modus themes such that, for example, `modus-themes-rotate' only
  ;; goes through the Ef themes (to this end, the Ef themes provide
  ;; the `ef-themes-take-over-modus-themes-mode' and the Standard
  ;; themes have the `standard-themes-take-over-modus-themes-mode'
  ;; equivalent).
  ;;
  ;; If you only care about the Modus themes, then (i) you do not need
  ;; to enable the `modus-themes-include-derivatives-mode' and (ii) do
  ;; not install and activate those other theme packages.
  (modus-themes-include-derivatives-mode 1)
  :bind
  (("<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-load-random))
  :config
  ;; Your customizations here.  All customizations must evaluated
  ;; BEFORE loading the theme.
  (setq modus-themes-to-toggle '(modus-operandi modus-vivendi)
        modus-themes-to-rotate modus-themes-items
        ;; modus-themes-mixed-fonts t
        ;; modus-themes-variable-pitch-ui t
        ;; modus-themes-italic-constructs t
        ;; modus-themes-bold-constructs t
        modus-themes-completions '((t . (bold)))
        modus-themes-prompts '(bold)
        modus-themes-headings
        '((agenda-structure . (variable-pitch light 2.2))
          (agenda-date . (variable-pitch regular 1.3))
          (t . (regular 1.15))))

  (setq modus-themes-common-palette-overrides nil)

  ;; Finally, load your theme of choice (or a random one with
  ;; `modus-themes-load-random', `modus-themes-load-random-dark',
  ;; `modus-themes-load-random-light').
  (modus-themes-load-theme 'modus-operandi))




;; Default keyboard cursor
(setq-default cursor-type 'bar)
(blink-cursor-mode 1)

;; Evil mode cursors - different cursor appearance for each Evil state
(after! evil
  (setq evil-normal-state-cursor   '(box "#202124")
        evil-insert-state-cursor   '(bar "#202124")
        evil-visual-state-cursor   '(box "#c3e88d")
        evil-replace-state-cursor  '(hbar "orange")
        evil-operator-state-cursor '(hollow "#d0d0d0")
        evil-motion-state-cursor   '(box "#d0d0d0")
        evil-emacs-state-cursor    '(bar "cyan")))

;; Mouse cursor
(setq-default void-text-area-pointer 'nil)

;;; Mouse & Scrolling
(setq scroll-preserve-screen-position nil)

;; Scroll one line at a time
(setq mouse-wheel-scroll-amount '(4 ((shift) . 8)))  ; 4 lines at a time, +shift 8 lines.

;; Do not accelerate scrolling
(setq mouse-wheel-progressive-speed nil)

;;; LSP UI Customization
;; Symbol highlighting faces (when cursor is on a symbol)
(after! lsp-mode
  (set-face-attribute 'lsp-face-highlight-textual nil
                      :background "darkseagreen2" :foreground "#000000" :weight 'bold)
  (set-face-attribute 'lsp-face-highlight-read nil
                      :background "darkseagreen2" :foreground "#000000" :weight 'bold)
  (set-face-attribute 'lsp-face-highlight-write nil
                      :background "darkseagreen2" :foreground "#000000" :weight 'bold))

;;; Which-Key UI
(after! which-key
  (setq which-key-popup-type 'minibuffer ;; better than side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.25
        which-key-side-window-max-width 0.33
        which-key-min-display-lines 6))

;;; Treemacs UI
(after! treemacs
  ;; Automatically switch to the project root of the current buffer
  (treemacs-project-follow-mode 1)
  (treemacs-follow-mode 1)

  ;; show indent guides
  (treemacs-indent-guide-mode 1)

  ;; same icon for include as src
  (treemacs-modify-theme "Default"
  :icon-directory (treemacs-join-path treemacs-dir "icons/default/svgrepo")
  :config
  (progn
    (treemacs-create-icon :file "dir-src-closed.png" :extensions ("include-closed"))
    (treemacs-create-icon :file "dir-src-open.png"   :extensions ("include-open")))
  ))


(provide 'init-ui)
;;; init-ui.el ends here
