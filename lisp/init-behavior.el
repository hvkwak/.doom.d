;;; init-behavior.el --- Change how my Doom Emacs behaves -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Behavioral modifications: advice (advice-add), hooks (add-hook),
;;; minor mode definitions (define-minor-mode), with-eval-after-load blocks.
;;; Code:

;;; Evil Jump List Integration
;; Whenever point moves "far" (consult jump, buffer-boundary motions, defun
;; motions, LSP/xref jumps), record the origin in Evil's jump list first, so
;; `C-o' / `C-i' (or `M-,' / `M-.') can hop back to where you were before the jump.

;; Consult jumps (consult-line, ripgrep, etc.) don't touch Evil's jump list
;; by default, so wire them in explicitly.
(with-eval-after-load 'consult
  (defun my/consult-push-evil-jump (&rest _)
    (when (bound-and-true-p evil-mode)
      (evil-set-jump)))
  ;; Fires once right before the real jump commits, not on every live-preview
  ;; candidate, so it won't spam the jump list.
  (add-hook 'consult-before-jump-hook #'my/consult-push-evil-jump)

  ;; Older Consult builds lack `consult-before-jump-hook'; advise the
  ;; low-level jump functions instead so the jump still gets recorded.
  (dolist (fn '(consult--jump consult--goto-location))
    (when (fboundp fn)
      (advice-add fn :around
                  (lambda (orig &rest args)
                    (my/consult-push-evil-jump)
                    (apply orig args))))))

;; Reusable advice: record an Evil jump before a motion command, but only
;; when called interactively, so internal/programmatic calls don't pollute
;; the jump list. Shared by the built-ins below and by rg/company in
;; init-utils.el.
(defun my/evil-set-jump-before (&rest _)
  (when (called-interactively-p 'interactive)
    (evil-set-jump)))

;; Built-in motions - always present, so no `with-eval-after-load' needed.
(advice-add 'beginning-of-buffer :before #'my/evil-set-jump-before)
(advice-add 'end-of-buffer       :before #'my/evil-set-jump-before)
(advice-add 'beginning-of-defun  :before #'my/evil-set-jump-before)

;; cc-mode's own defun motions (C/C++/Java), separate from the generic
;; `beginning-of-defun' above.
(with-eval-after-load 'cc-cmds
  (advice-add 'c-beginning-of-defun :before #'my/evil-set-jump-before)
  (advice-add 'c-end-of-defun       :before #'my/evil-set-jump-before))

;; LSP's own find-definition/declaration/references commands.
(with-eval-after-load 'lsp-mode
  (advice-add 'lsp-find-definition   :before #'my/evil-set-jump-before)
  (advice-add 'lsp-find-declaration  :before #'my/evil-set-jump-before)
  (advice-add 'lsp-find-references   :before #'my/evil-set-jump-before))

;; Generic xref backend (used by e.g. eglot and etags).
(with-eval-after-load 'xref
  (advice-add 'xref-find-definitions :before #'my/evil-set-jump-before)
  (advice-add 'xref-find-references  :before #'my/evil-set-jump-before)
  (advice-add 'xref-find-apropos     :before #'my/evil-set-jump-before))

;;; Function Signature in Header Line
;; A buffer-local minor mode that shows the signature of the defun point is
;; currently inside, in the header line — handy for long functions where the
;; `(defun foo (...)' line has scrolled off screen.
(defun my/defun-sig ()
  "One-line signature of the defun point is inside, or nil if not inside one."
  (save-excursion
    (save-restriction
      (condition-case nil
          (progn
            (narrow-to-defun)
            (goto-char (point-min))
            ;; Find the arg-list's opening paren, skipping any that are
            ;; inside a string or comment (via `syntax-ppss').
            (while (and (re-search-forward "(" (line-end-position 60) t)
                        (let ((s (syntax-ppss))) (or (nth 3 s) (nth 4 s)))))
            (when (match-beginning 0)
              (goto-char (match-beginning 0))
              (when-let ((end (ignore-errors (scan-lists (point) 1 0))))
                (let* ((raw (buffer-substring-no-properties (point-min) end))
                       (flat (replace-regexp-in-string "[ \t\n]+" " " raw)))
                  (replace-regexp-in-string "\\`[ \t]+\\|[ \t]+\\'" "" flat)))))
        (error nil)))))

;; Holds whatever `header-line-format' was before the mode was enabled, so it
;; can be restored on disable instead of just blanking the header line.
(defvar-local my/defun-sig--prev-header nil)
(defun my/defun-sig--header ()
  "Compute header content when the mode is enabled.
Falls back to the previous header (rather than blank/nil) when point
isn't inside a defun, e.g. between functions or at top of file."
  (or (my/defun-sig) my/defun-sig--prev-header))

;; TODO: update this into automatic sig header for long implementations.
(define-minor-mode my-defun-sig-header-mode
  "Show current defun signature in the header line (buffer-local)."
  :lighter " SigHdr"
  (if my-defun-sig-header-mode
      (progn
        ;; remember whatever was there before
        (setq my/defun-sig--prev-header header-line-format)
        ;; install dynamic header: `:eval' re-runs my/defun-sig--header on
        ;; every redisplay, so the signature updates as point moves.
        (setq-local header-line-format '(:eval (my/defun-sig--header))))
    ;; restore previous header when disabling
    (setq-local header-line-format my/defun-sig--prev-header)
    (kill-local-variable 'my/defun-sig--prev-header)))

(provide 'init-behavior)
;;; init-behavior.el ends here
