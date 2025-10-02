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
  (setq gptel-api-key (my/read-api-key-from-desktop))
  (setq gptel-model 'gpt-5-mini)
  (setq gptel-display-buffer-action
        '((display-buffer-in-side-window)
          (side . right)
          (window-width . 0.5)
          (slot . 1))))

(use-package! claude-code-ide
  ;; https://github.com/manzaltu/claude-code-ide.el?tab=readme-ov-file
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding

  :config
  (claude-code-ide-emacs-tools-setup) ; Optionally enable Emacs MCP tools
  (setq claude-code-ide-terminal-backend 'vterm
        ;; Use regular window instead of side window
        claude-code-ide-use-side-window nil
        )
  )

(provide 'init-llm)
;;; init-llm.el ends here
