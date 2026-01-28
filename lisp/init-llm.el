;;; init-llm.el --- LLM integration configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;
;; Configuration for LLM tools (gptel, claude-code-ide, etc.)
;; Currently disabled - uncomment and configure as needed.
;;
;;; Code:

;; Example gptel configuration (requires API key):
;; (use-package! gptel
;;   :config
;;   (setq gptel-api-key (getenv "OPENAI_API_KEY"))  ; or use auth-source
;;   (setq gptel-model 'gpt-4o-mini)
;;   (setq gptel-display-buffer-action
;;         '((display-buffer-in-side-window)
;;           (side . right)
;;           (window-width . 0.5))))

;; Example claude-code-ide configuration:
;; (use-package! claude-code-ide
;;   :bind ("C-c C-'" . claude-code-ide-menu)
;;   :config
;;   (setq claude-code-ide-terminal-backend 'vterm))

(provide 'init-llm)
;;; init-llm.el ends here
