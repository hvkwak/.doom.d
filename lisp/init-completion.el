;;; init-completion.el --- completion -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages for better completion and regex
;; these two are activated in init.el
;; company - In-buffer code completion (like suggesting function names, variables, etc.)
;; vertico - Minibuffer completion UI (for commands like M-x, find-file, etc.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
(use-package! consult

  ;; Enhances Emacs commands like buffer switching, searching, and navigation with better interfaces and previews.
  :after projectile
  :config
  (setq consult-preview-key 'any) ;; Preview instantly as you cycle
  (setq consult-buffer-sources
      '(consult--source-project-buffer)  ;; Show only project buffers
      ;; '(consult--source-project-buffer  ;; Show only project buffers and recent files
      ;;   consult--source-recent-file)
      )
  (setq consult-project-function #'projectile-project-root)

  ;; filter out several unrelated buffers
  (setq consult-buffer-filter '(;;"\\` \\*" ;; this excluded filters out any buffer whose name starts with a space and an asterisk, may be too broad
                                "\\`\\*scratch\\*"
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
                                )))

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
  (define-key company-active-map (kbd "TAB") #'company-select-next)
  (define-key company-active-map (kbd "<tab>") #'company-select-next)
  (define-key company-active-map (kbd "S-TAB") #'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") #'company-select-previous))

(with-eval-after-load 'yasnippet
  (define-key yas-keymap (kbd "TAB")       #'yas-next-field)
  (define-key yas-keymap (kbd "<tab>")     #'yas-next-field)
  (define-key yas-keymap (kbd "S-TAB")     #'yas-prev-field)
  (define-key yas-keymap (kbd "<backtab>") #'yas-prev-field))

(provide 'init-completion)
;;; init-completion.el ends here
