;;; init-utils.el --- Completion & search package config -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Config (use-package!/after!) for the completion & search stack, plus
;;; each package's small companion functions (kept next to the config that
;;; needs them rather than in init-functions.el):
;;;   consult    - jump-to-line/search commands
;;;   marginalia - annotations in minibuffer completion
;;;   orderless  - out-of-order completion matching (feeds vertico, configured
;;;                in init-keybinds-modes.el)
;;;   company    - in-buffer code completion
;;;   rg         - ripgrep search UI
;;; Code:

;;; Consult
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

;;; Marginalia
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

;;; Orderless
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

;;; Company
(after! company
  (setq company-auto-commit nil
        company-minimum-prefix-length 1
        company-idle-delay 0.5
        company-selection-wrap-around t)

  ;; disable company auto completion at dape-repl-mode
  (add-hook 'dape-repl-mode-hook (lambda () (company-mode -1))))

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

;;; Ripgrep
(set-popup-rule! "^\\*rg\\*$"
  :side 'bottom
  :size 0.5
  :slot 0
  :select t
  :quit t
  :ttl nil)   ;; keep window until explicitly closed

(advice-add 'rg-dwim :before #'my/evil-set-jump-before)

(setq rg-custom-type-aliases
      '(("MyC" . "*.c *.cu *.cpp *.cc *.cxx *.h *.hpp")))

(provide 'init-utils)
;;; init-utils.el ends here
