;;; init-llm.el --- llm integration configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
(use-package! gptel
  :config
  (setq! gptel-api-key (my/read-api-key-from-desktop))
  (setq gptel-display-buffer-action
        '((display-buffer-in-side-window)
          (side . right)
          (window-width . 0.5)
          (slot . 1)))
)

(defun my/read-api-key-from-desktop ()
  "Read the API key from ~/Desktop/password.txt."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "~/Desktop/ChatGPTAPIKey.txt"))
    (string-trim (buffer-string))))

(provide 'init-llm)
;;; init-llm.el ends here
