;;; init-glsl.el --- Editor GLSL behavior configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(after! glsl-mode

  ;; GLSL extensions
  (dolist (ext '("\\.vert\\'" "\\.frag\\'" "\\.geom\\'" "\\.comp\\'"))
    (add-to-list 'auto-mode-alist (cons ext 'glsl-mode)))

  ;; enable flycheck
  (flycheck-define-checker glsl-glslang
    "A GLSL syntax checker using glslangValidator."
    :command ("glslangValidator" source)
    :error-patterns
    ((error line-start "ERROR: " (one-or-more (not ":")) ":" line ":" (message) line-end)
     (warning line-start "WARNING: " (one-or-more (not ":")) ":" line ":" (message) line-end))
    :modes glsl-mode)

  ;; Add it to the list of checkers
  (add-to-list 'flycheck-checkers 'glsl-glslang)

  ;; Company backend
  (set-company-backend! 'glsl-mode
    'company-glsl
    'company-capf
    'company-dabbrev-code)

  ;; Validate via glslangValidator in a dedicated buffer
  (defun my/glsl-validate-buffer ()
    "Validate current GLSL file with glslangValidator."
    (interactive)
    (when (and buffer-file-name
               (executable-find "glslangValidator"))
      (let* ((file (file-name-nondirectory buffer-file-name))
             (compilation-buffer-name-function
              (lambda (_mode)
                (format "*glsl-validate:%s*" file))))
        (compile (format "glslangValidator %s"
                         (shell-quote-argument buffer-file-name))))))

  (defun my/glsl-enable-auto-validate ()
    (add-hook 'after-save-hook #'my/glsl-validate-buffer nil t))

  (add-hook 'glsl-mode-hook #'my/glsl-enable-auto-validate)
)

(after! compile
  (defun my/select-glsl-compilation (buffer _status)
    "Focus GLSL validation window when it finishes."
    (when (string-match-p "^\\*glsl-validate.*\\*$"
                          (buffer-name buffer))
      (when-let ((win (get-buffer-window buffer)))
        (select-window win))))
  (add-hook 'compilation-finish-functions #'my/select-glsl-compilation)
  )

(provide 'init-glsl)
;;; init-glsl.el ends here
