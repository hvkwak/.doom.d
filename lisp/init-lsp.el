;;; init-ui.el --- LSP UI Settings -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-position 'right
        lsp-ui-doc-include-signature t
        lsp-ui-doc-max-height 100
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-peek-enable t
        lsp-enable-symbol-highlighting t
        lsp-signature-render-documentation t
        lsp-idle-delay 0.15
        lsp-ui-sideline-delay 0.1
        lsp-diagnostics-provider :none
        )
  )

(after! lsp-clangd
  (setq lsp-clients-clangd-args
        '("--query-driver=/usr/bin/g++,/usr/bin/gcc,/usr/bin/clang++,/usr/bin/clang"
          "--compile-commands-dir=."
          "--header-insertion=never" ;; to not automatically insert #include statements
          )))

;; This enables C-h in c-mode
(with-eval-after-load 'ccls
  (dolist (m '(c-mode-map c++-mode-map objc-mode-map c-ts-mode-map c++-ts-mode-map))
    (when (boundp m)
      (evil-define-key 'normal (symbol-value m)
        (kbd "C-h") nil (kbd "C-j") nil (kbd "C-k") nil (kbd "C-l") nil))))

;; This enables C-h in c-mode
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-h") nil))

(provide 'init-lsp)
;;; init-lsp.el ends here
