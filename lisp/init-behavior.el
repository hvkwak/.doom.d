;;; init-behavior.el --- Change how my Doom Emacs behaves -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Behavioral modifications: advice (advice-add), hooks (add-hook),
;;; minor mode definitions (define-minor-mode), with-eval-after-load blocks.
;;; Code:

;;; Evil Jump List Integration
;; Make Evil's jump list work with Consult jumps (consult-line, ripgrep, etc.)
(with-eval-after-load 'consult
  (defun my/consult-push-evil-jump (&rest _)
    (when (bound-and-true-p evil-mode)
      (evil-set-jump)))              ;; record current point in evil's jumplist
  ;; Prefer official hook (no spam from live preview)
  (add-hook 'consult-before-jump-hook #'my/consult-push-evil-jump)

  ;; Fallback for older Consult builds
  (dolist (fn '(consult--jump consult--goto-location))
    (when (fboundp fn)
      (advice-add fn :around
                  (lambda (orig &rest args)
                    (my/consult-push-evil-jump)
                    (apply orig args))))))

;; One reusable advice fn
(defun my/evil-set-jump-before (&rest _)
  (when (called-interactively-p 'interactive)
    (evil-set-jump)))

;; Built-ins are always present
(advice-add 'beginning-of-buffer :before #'my/evil-set-jump-before)
(advice-add 'end-of-buffer       :before #'my/evil-set-jump-before)
(advice-add 'beginning-of-defun :before #'my/evil-set-jump-before)

;; cc-mode defun motions live in cc-cmds
(with-eval-after-load 'cc-cmds
  (advice-add 'c-beginning-of-defun :before #'my/evil-set-jump-before)
  (advice-add 'c-end-of-defun       :before #'my/evil-set-jump-before)) ;; optional but nice

;; LSP/Xref jumps (cover both LSP and generic xref)
(with-eval-after-load 'lsp-mode
  (advice-add 'lsp-find-definition   :before #'my/evil-set-jump-before)
  (advice-add 'lsp-find-declaration  :before #'my/evil-set-jump-before)
  (advice-add 'lsp-find-references   :before #'my/evil-set-jump-before))

(with-eval-after-load 'xref
  (advice-add 'xref-find-definitions      :before #'my/evil-set-jump-before)
  (advice-add 'xref-find-references       :before #'my/evil-set-jump-before)
  (advice-add 'xref-find-apropos          :before #'my/evil-set-jump-before))

;;; Function Signature in Header Line
(defun my/defun-sig ()
  "One-line signature if point is inside the defun, else nil."
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

(defvar-local my/defun-sig--prev-header nil)
(defun my/defun-sig--header ()
  "Compute header content when the mode is enabled."
  (or (my/defun-sig) my/defun-sig--prev-header))

(define-minor-mode my-defun-sig-header-mode
  "Show current defun signature in the header line (buffer-local)."
  :lighter " SigHdr"
  (if my-defun-sig-header-mode
      (progn
        ;; remember whatever was there before
        (setq my/defun-sig--prev-header header-line-format)
        ;; install dynamic header
        (setq-local header-line-format '(:eval (my/defun-sig--header))))
    ;; restore previous header when disabling
    (setq-local header-line-format my/defun-sig--prev-header)
    (kill-local-variable 'my/defun-sig--prev-header)))

;;; Flycheck
(after! flycheck
  (flycheck-mode 1)
  ;; (global-flycheck-mode -1)
  ;; (setq flycheck-global-modes nil)
  )

(provide 'init-behavior)
;;; init-behavior.el ends here
