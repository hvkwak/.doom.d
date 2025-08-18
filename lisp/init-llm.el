;;; init-llm.el --- llm integration configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
(defun my/read-api-key-from-desktop ()
  "Read the API key from ~/Desktop/password.txt."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "~/Desktop/ChatGPTAPIKey.txt"))
    (string-trim (buffer-string))))

(use-package! gptel
  :config
  (setq! gptel-api-key (my/read-api-key-from-desktop))
  (setq gptel-display-buffer-action
        '((display-buffer-in-side-window)
          (side . right)
          (window-width . 0.5)
          (slot . 1))))

(after! markdown-mode
  ;; Tighter paragraph spacing
  (setq markdown-asymmetric-header t) ; No extra underline chars
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-list-indent-width 2)

  ;; Reduce blank lines before/after headings
  (setq markdown-header-scaling t) ; Keep heading size proportional
  (setq markdown-hide-markup t)    ; Hide syntax chars (*, #, etc.)

  ;; Narrower text width for better readability
  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq-local line-spacing 0.1)   ; tighter line height
              (setq-local fill-column 80)     ; wrap at 80 chars
              (visual-line-mode 1)             ; soft wrap
              (variable-pitch-mode 1)          ; proportional font
              (setq-local markdown-hide-markup t)
              ;; Remove extra top margin for headings
              (setq-local markdown-header-scaling nil)
              (setq-local markdown-fontify-whole-heading-line t)
              )))

(provide 'init-llm)
;;; init-llm.el ends here
