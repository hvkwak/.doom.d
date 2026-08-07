;;; init-lsp.el --- LSP configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;
;; LSP-mode and LSP-UI configuration.
;; Includes clangd setup for C/C++ development.
;;
;;; Code:
(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-position 'right
        lsp-ui-doc-include-signature t
        lsp-ui-doc-max-height 100
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-diagnostics t
        ;; lsp-ui-sideline-diagnostic-max-lines 3
        ;; lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-peek-enable t
        lsp-enable-symbol-highlighting t
        lsp-signature-render-documentation t
        lsp-idle-delay 0.15
        lsp-ui-sideline-delay 0.1
        lsp-diagnostics-provider :flycheck
        ;; lsp-ui-sideline-show-diagnostics t
        ;; lsp-ui-sideline-enable t
        )
  )

(after! lsp-clangd
  (setq lsp-clients-clangd-args
        '("--query-driver=/usr/bin/c++,/usr/bin/cc,/usr/bin/g++,/usr/bin/gcc,/usr/bin/clang++,/usr/bin/clang"
          "--compile-commands-dir=."
          "--header-insertion=never" ;; to not automatically insert #include statements
          "--background-index"
          "--clang-tidy"
          "--log=error"
          )))

;; Unbind C-h in lsp-mode to preserve help-map access
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-h") nil))


;;; Flycheck
;; Note: Flycheck is already provided by Doom's `:checkers syntax' module.
;; This just ensures it's globally enabled.
(after! flycheck
  (global-flycheck-mode +1)

  ;; show error left side
  ;; Errors/warnings are indicated by a bitmap glyph in the left fringe
  ;; (instead of e.g. underlining) — `fringe-mode' widens both fringes to
  ;; 16px so the custom arrow bitmap below has room to render clearly.
  (setq flycheck-indication-mode 'left-fringe)
  (fringe-mode '(16 . 16))
  ;; Custom arrow-shaped fringe bitmap: each byte is one row (8 rows total,
  ;; MSB-first), forming a right-pointing triangle used as the flycheck
  ;; indicator glyph instead of Emacs' default fringe bitmap.
  (define-fringe-bitmap 'flycheck-fringe-bitmap-arrow
    (vector #b11111000
            #b11111100
            #b11111110
            #b11111111
            #b11111111
            #b11111110
            #b11111000
            #b11100000
            #b11000000)
    nil nil 'center)
  )

(provide 'init-lsp)
;;; init-lsp.el ends here
