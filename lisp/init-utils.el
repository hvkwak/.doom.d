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

;;; rg
(set-popup-rule! "^\\*rg\\*$"
  :side 'bottom
  :size 0.5
  :slot 0
  :select t
  :quit t
  :ttl nil)

(advice-add 'rg-dwim :before #'my/evil-set-jump-before)
(add-hook 'rg-mode-hook #'next-error-follow-minor-mode)

(setq rg-custom-type-aliases
      '(("MyC" . "*.c *.cu *.cpp *.cc *.cxx *.h *.hpp")))

(defvar my/rg-ephemeral-buffer nil
  "The single ephemeral preview buffer kept in memory during rg-mode skimming.")

(defun my/rg-kill-ephemeral-buffer ()
  "Safely kill the ephemeral buffer if it exists and is unmodified."
  (when (and my/rg-ephemeral-buffer
             (buffer-live-p my/rg-ephemeral-buffer)
             (not (buffer-modified-p my/rg-ephemeral-buffer)))
    (kill-buffer my/rg-ephemeral-buffer))
  (setq my/rg-ephemeral-buffer nil))

;; 1. Maintain a single ephemeral preview buffer while skimming.
;; `compilation-find-file' opens the file *before* `compilation-goto-locus'
;; runs, so whether the buffer is new must be detected there.
(defvar my/rg--new-buffer nil
  "Buffer freshly created by the last rg jump, or nil.")

(defun my/rg--rg-marker-p (marker)
  "Non-nil if MARKER points into an rg-mode results buffer."
  (and (markerp marker)
       (buffer-live-p (marker-buffer marker))
       (with-current-buffer (marker-buffer marker)
         (derived-mode-p 'rg-mode))))

(defun my/rg-detect-new-buffer-a (orig-fn marker &rest args)
  "Remember the result of `compilation-find-file' if it created a new buffer."
  (let* ((before (buffer-list))
         (buf (apply orig-fn marker args)))
    (when (and (my/rg--rg-marker-p marker)
               (bufferp buf)
               (not (memq buf before)))
      (setq my/rg--new-buffer buf))
    buf))

(defun my/rg-track-ephemeral-buffer-a (msg mk &rest _)
  "Keep only one unselected, newly opened rg preview buffer alive.
Buffers that were already open before the search are never killed."
  (when (my/rg--rg-marker-p msg)
    (let ((target (and (markerp mk) (marker-buffer mk))))
      (unless (eq target my/rg-ephemeral-buffer)
        (my/rg-kill-ephemeral-buffer)
        (when (and target (eq target my/rg--new-buffer))
          (setq my/rg-ephemeral-buffer target))))
    (setq my/rg--new-buffer nil)))

(advice-add 'compilation-find-file :around #'my/rg-detect-new-buffer-a)
(advice-add 'compilation-goto-locus :after #'my/rg-track-ephemeral-buffer-a)

;; 2. Promote ephemeral buffer to permanent status when selected via RET
(defun my/rg-promote-ephemeral-buffer (&rest _)
  "Remove ephemeral status so the selected buffer is preserved."
  (setq my/rg-ephemeral-buffer nil))

(advice-add 'compile-goto-error :after #'my/rg-promote-ephemeral-buffer)
(with-eval-after-load 'rg
  (advice-add 'rg-hit-select :after #'my/rg-promote-ephemeral-buffer))

;; 3. Cleanup function executed upon quitting
(defun my/rg-clean-ephemeral-on-quit (&rest _)
  "Kill any remaining unselected ephemeral buffer when closing the search process."
  (my/rg-kill-ephemeral-buffer))

;; 4. Attach cleanup triggers for M-q (doom/escape), quit-window, and Doom popups
(advice-add 'quit-window :before #'my/rg-clean-ephemeral-on-quit)
(advice-add '+popup/quit-window :before #'my/rg-clean-ephemeral-on-quit)
(advice-add '+popup/close :before #'my/rg-clean-ephemeral-on-quit)

;; Doom Escape Hook (returns nil to allow the escape chain sequence to continue)
(add-hook 'doom-escape-hook
          (lambda ()
            (my/rg-clean-ephemeral-on-quit)
            nil))

(provide 'init-utils)
;;; init-utils.el ends here
