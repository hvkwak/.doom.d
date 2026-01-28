;;; init-completion.el --- completion -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary: packages for better completion and regex
;;;             these two are activated in init.el
;;;             company - In-buffer code completion (like suggesting function names, variables, etc.)
;;;             vertico - Minibuffer completion UI (for commands like M-x, find-file, etc.)
;;; Code:

;;; consult
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

(defun my/thing-at-point ()
  "Return the symbol at point as a plain string, or nil if none."
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
              ;; (delete-selection-mode 1)
              ;; Select the whole initial input so typing replaces it
              (set-mark (minibuffer-prompt-end))
              (goto-char (point-max))
              (activate-mark))
          ;; Prefer passing INITIAL to consult instead of inserting ourselves
          (consult-line sym))
      (consult-line))))

;;; marginalia
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

;;; orderless
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
  (orderless-case-sensitivity 'smart)
  )

;;; company
(after! company
  (setq company-auto-commit nil
        company-minimum-prefix-length 1
        company-idle-delay 0.5
        company-selection-wrap-around t))

(defun my/company-accept-and-trim-duplicate ()
  "Accept Company candidate and remove duplicated suffix ahead of point.
Example: 'material.pecular' + candidate 'materialSpecular'
→ leaves exactly 'materialSpecular'."
  (interactive)
  (when (and (bound-and-true-p company-candidates)
             (>= (or company-selection 0) 0))
    (let* ((cand (nth company-selection company-candidates))
           (ahead (save-excursion
                    (buffer-substring-no-properties
                     (point)
                     (progn (skip-chars-forward "_[:alnum:]") (point))))))
      ;; Do the normal insert first.
      (company-complete-selection)
      ;; Then trim any overlap between CAND's suffix and the text ahead.
      (when (and cand (> (length ahead) 0))
        (let ((n (cl-loop for i from (min (length ahead) (length cand)) downto 1
                          when (string-suffix-p (substring ahead 0 i) cand)
                          return i)))
          (when n (delete-char n)))))))

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

(provide 'init-completion)
;;; init-completion.el ends here
