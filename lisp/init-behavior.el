;;; init-behavior.el --- Change how my Doom Emacs behaves -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Behavioral modifications: advice (advice-add), hooks (add-hook),
;;; minor mode definitions (define-minor-mode), with-eval-after-load blocks.
;;; Code:

;;; Evil Jump List Integration
;; Goal: whenever point moves "far" (consult jump, buffer-boundary motions,
;; defun motions, LSP/xref jumps), record the origin in Evil's jump list
;; first, so `C-o' / `C-i' can hop back to where you were before the jump.

;; Make Evil's jump list work with Consult jumps (consult-line, ripgrep, etc.)
(with-eval-after-load 'consult
  (defun my/consult-push-evil-jump (&rest _)
    (when (bound-and-true-p evil-mode)
      (evil-set-jump)))              ;; record current point in evil's jumplist
  ;; Prefer official hook (no spam from live preview)
  ;; consult-before-jump-hook fires once, right before the real jump commits
  ;; (unlike per-candidate preview updates, which would spam the jump list).
  (add-hook 'consult-before-jump-hook #'my/consult-push-evil-jump)

  ;; Fallback for older Consult builds that predate `consult-before-jump-hook':
  ;; wrap the low-level jump functions directly so the jump is still recorded.
  (dolist (fn '(consult--jump consult--goto-location))
    (when (fboundp fn)
      (advice-add fn :around
                  (lambda (orig &rest args)
                    (my/consult-push-evil-jump)
                    (apply orig args))))))

;; One reusable advice fn: record an Evil jump-list entry before a motion
;; command runs, but only when called interactively (so programmatic/internal
;; calls to these functions don't pollute the jump list).
(defun my/evil-set-jump-before (&rest _)
  (when (called-interactively-p 'interactive)
    (evil-set-jump)))

;; Built-ins are always present, so these advices can be added unconditionally
;; at load time (no `with-eval-after-load' needed).
(advice-add 'beginning-of-buffer :before #'my/evil-set-jump-before)
(advice-add 'end-of-buffer       :before #'my/evil-set-jump-before)
(advice-add 'beginning-of-defun :before #'my/evil-set-jump-before)

;; cc-mode defun motions live in cc-cmds
;; (c-beginning-of-defun/c-end-of-defun are C/C++/Java-mode's own defun
;; motions, separate from the generic `beginning-of-defun' above.)
(with-eval-after-load 'cc-cmds
  (advice-add 'c-beginning-of-defun :before #'my/evil-set-jump-before)
  (advice-add 'c-end-of-defun       :before #'my/evil-set-jump-before)) ;; optional but nice

;; LSP/Xref jumps (cover both LSP and generic xref)
;; lsp-mode has its own find-definition/declaration/references commands
;; distinct from Emacs' built-in xref frontend.
(with-eval-after-load 'lsp-mode
  (advice-add 'lsp-find-definition   :before #'my/evil-set-jump-before)
  (advice-add 'lsp-find-declaration  :before #'my/evil-set-jump-before)
  (advice-add 'lsp-find-references   :before #'my/evil-set-jump-before))

;; xref is the generic jump backend used by e.g. eglot and etags;
;; advise it too so those code paths also register jumps.
(with-eval-after-load 'xref
  (advice-add 'xref-find-definitions      :before #'my/evil-set-jump-before)
  (advice-add 'xref-find-references       :before #'my/evil-set-jump-before)
  (advice-add 'xref-find-apropos          :before #'my/evil-set-jump-before))

;;; Function Signature in Header Line
;; A buffer-local minor mode that shows the signature of the defun point is
;; currently inside, in the header line — handy for long functions where the
;; `(defun foo (...)' line has scrolled off screen.

(defun my/defun-sig ()
  "One-line signature if point is inside the defun, else nil.
Narrows to the enclosing defun, then scans forward for the first
top-level `(' that is *not* inside a string or comment (checked via
`syntax-ppss': (nth 3 s) = in-string, (nth 4 s) = in-comment) — that
paren starts the argument list. `scan-lists' finds its matching close
paren, and everything from the start of the defun up to there
(typically `defun NAME (ARGS)') is flattened to one line and trimmed
to form the signature. Returns nil (via `condition-case') if point
isn't inside a defun or anything else goes wrong."
  (save-excursion
    (save-restriction
      (condition-case nil
          (progn
            (narrow-to-defun)
            (goto-char (point-min))
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
