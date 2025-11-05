;;; init-completion.el --- completion -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary: packages for better completion and regex
;;;             these two are activated in init.el
;;;             company - In-buffer code completion (like suggesting function names, variables, etc.)
;;;             vertico - Minibuffer completion UI (for commands like M-x, find-file, etc.)
;;; Code:
(use-package! consult
  ;; Enhances Emacs commands like buffer switching, searching, and navigation with better interfaces and previews.
  :after projectile
  :config
  (setq consult-preview-key 'any) ;; Preview instantly as you cycle
  ;; Use workspace-aware buffer sources to respect workspace boundaries
  (setq consult-buffer-sources
      '(+workspaces--consult-workspace-buffer-source  ;; Only show buffers in current workspace
        consult--source-hidden-buffer))               ;; Optional: include hidden buffers with space prefix
  (setq consult-project-function #'projectile-project-root)
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 \
        --path-separator / --smart-case --no-heading --with-filename --line-number")

  ;; filter out several unrelated buffers
  (setq consult-buffer-filter '("\\`\\*scratch\\*"
                                "\\`\\*Messages\\*"
                                "\\`\\*doom\\*"
                                "\\`\\*lsp-log\\*"
                                "\\`\\*Native-compile-Log\\*"
                                "\\`\\*Ibuffer\\*"
                                "\\`\\bash-completion\\*"
                                "\\`\\*clangd\\*"
                                "\\`\\*clangd::stderr\\*"
                                "\\`\\*Async-native-compile-log\\*"
                                "\\*compilation\\*<\\.emacs\\.d>"
                                ))
  )


;;; rg
(set-popup-rule! "^\\*rg\\*$"
  :side 'bottom
  :size 0.5
  :slot 0
  :select t
  :quit t
  :ttl nil)   ;; keep window until explicitly closed
(defun my/rg-goto-and-quit ()
  "Jump to the search result, delete the rg window, and kill the rg buffer."
  (interactive)
  (let ((rg-buffer (current-buffer))
        (rg-window (selected-window)))
    (compile-goto-error)
    (when (window-live-p rg-window)
      (delete-window rg-window))
    (when (buffer-live-p rg-buffer)
      (kill-buffer rg-buffer))
    ))

(defun my/rg-quit-and-kill ()
  "Delete the rg window and kill the rg buffer."
  (interactive)
  (let ((rg-buffer (current-buffer))
        (rg-window (selected-window)))
    (when (window-live-p rg-window)
      (delete-window rg-window))
    (when (buffer-live-p rg-buffer)
      (kill-buffer rg-buffer))
    ))

(setq rg-custom-type-aliases
      '(("MyC" . "*.c *.cu *.cpp *.cc *.cxx *.h *.hpp")))

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


(defun my/thing-at-point ()
  (when-let ((s (thing-at-point 'symbol t)))
    (substring-no-properties s)))

(defun my/consult-line-dwim ()
  "Run `consult-line` with symbol at point prefilled and selected.
Typing replaces the selection; empty symbol -> plain `consult-line`."
  (interactive)
  (let* ((sym (my/thing-at-point))
         (sym (and sym (> (length sym) 0) sym))) ; avoid subr-x
    (if sym
        (minibuffer-with-setup-hook
            (lambda ()
              ;; Enable the *mode*, not just the var
              (delete-selection-mode 1)
              ;; Select the whole initial input so typing replaces it
              (set-mark (minibuffer-prompt-end))
              (goto-char (point-max))
              (activate-mark))
          ;; Prefer passing INITIAL to consult instead of inserting ourselves
          (consult-line sym))
      (consult-line))))

;; (defun my/git-super-project-root ()
;;   "Find the topmost git repository root (super project if in submodule).
;; Returns the directory containing the outermost .git directory."
;;   (when-let* ((default-directory (or (projectile-project-root)
;;                                      default-directory))
;;               (current-root (locate-dominating-file default-directory ".git")))
;;     (let ((super-root current-root)
;;           (parent (file-name-directory (directory-file-name current-root))))
;;       ;; Keep looking up for .git directories
;;       (while (and parent
;;                   (setq parent (locate-dominating-file parent ".git")))
;;         (setq super-root parent)
;;         (setq parent (file-name-directory (directory-file-name parent))))
;;       super-root)))

;; (defun my/consult-ripgrep-dwim ()
;;   "Run `consult-ripgrep` with symbol-at-point prefilled & selected.
;; If there is no symbol, run plain `consult-ripgrep`.
;; Searches from the super project root (handles submodules correctly)."
;;   (interactive)
;;   (let* ((sym (my/thing-at-point))
;;          (sym (and sym (> (length sym) 0) sym))
;;          (search-root (my/git-super-project-root)))
;;     (if sym
;;         (minibuffer-with-setup-hook
;;             (lambda ()
;;               (delete-selection-mode 1)
;;               (goto-char (minibuffer-prompt-end))
;;               (set-mark (point-max))
;;               (activate-mark))
;;           (consult-ripgrep search-root sym))
;;       (consult-ripgrep search-root))))

(use-package! marginalia
  ;; Adds helpful annotations to minibuffer completion results.
  :general
  (:keymaps 'minibuffer-local-map
            "M-A" 'marginalia-cycle)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package! orderless
  ;; Matches your typed input orderless minibuffer completions.
  :custom
  (completion-styles '(orderless))      ; Use orderless
  (completion-category-defaults nil)    ; I want to be in control!
  (completion-category-overrides
   '((file (styles basic ; For `tramp' hostname completion with `vertico'
                   orderless)))) ; no basic-remote, but basic.
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     ;; orderless-flex                       ; Basically fuzzy finding
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  (setq orderless-case-sensitivity 'smart) ;; TODO: keep it smart? or not sensitive?
)

(after! company
  (setq company-auto-commit nil
        company-minimum-prefix-length 1
        company-idle-delay 0.0
        company-selection-wrap-around t)

  ;; Make TAB cycle candidates instead of accepting immediately
  (define-key company-active-map (kbd "M-i") #'company-select-previous)
  (define-key company-active-map (kbd "M-k") #'company-select-next)
  (define-key company-active-map (kbd "M-SPC") #'company-abort)
  ;; (define-key company-active-map (kbd "TAB") #'company-select-next)
  ;; (define-key company-active-map (kbd "<tab>") #'company-select-next)
  ;; (define-key company-active-map (kbd "S-TAB") #'company-select-previous)
  ;; (define-key company-active-map (kbd "<backtab>") #'company-select-previous)
)

(with-eval-after-load 'yasnippet
  (define-key yas-keymap (kbd "TAB")       #'yas-next-field)
  (define-key yas-keymap (kbd "<tab>")     #'yas-next-field)
  (define-key yas-keymap (kbd "S-TAB")     #'yas-prev-field)
  (define-key yas-keymap (kbd "<backtab>") #'yas-prev-field))


(provide 'init-completion)
;;; init-completion.el ends here
