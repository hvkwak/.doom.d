;;; init-utils.el --- utils -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary: packages for better completion, search, navigation and regex
;;;             company - In-buffer code completion (like suggesting function names, variables, etc.)
;;;             vertico - Minibuffer completion UI (for commands like M-x, find-file, etc.)
;;;             consult - Practical search and navigation commands (e.g., searching lines in buffer)
;;; Code:

;; consult-line-dwim
(defun my/thing-at-point ()
  "Return the symbol at point as a plain string, or nil if none."
  (when-let ((s (thing-at-point 'symbol t)))
    (substring-no-properties s)))

(defun my/consult-line-dwim ()
  "Run `consult-line` with symbol at point prefilled and selected.
Typing replaces the selection; empty symbol -> plain `consult-line`."
  (interactive)
  (let* ((sym (my/thing-at-point))
         (sym (and sym (> (length sym) 0) sym))) ; avoid subr-x
    (if sym
        (minibuffer-with-setup-hook
            (lambda ()
              ;; Enable the *mode*, not just the var
              ;; (delete-selection-mode 1)
              ;; Select the whole initial input so typing replaces it
              (set-mark (minibuffer-prompt-end))
              (goto-char (point-max))
              (activate-mark))
          ;; Prefer passing INITIAL to consult instead of inserting ourselves
          (consult-line sym))
      (consult-line))))

;;; marginalia
(use-package! marginalia
  ;; Adds helpful annotations to minibuffer completion results.
  :general
  (:keymaps 'minibuffer-local-map
            "M-A" 'marginalia-cycle)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

;;; orderless
(use-package! orderless
  ;; Matches your typed input orderless minibuffer completions.
  :custom
  (completion-styles '(orderless))      ; Use orderless
  (completion-category-defaults nil)    ; I want to be in control!
  (completion-category-overrides
   '((file (styles basic ; For `tramp' hostname completion with `vertico'
                   orderless)))) ; no basic-remote, but basic.
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     ;; orderless-flex                       ; Basically fuzzy finding
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  (orderless-case-sensitivity 'smart)
  )

;;; company
(after! company
  (setq company-auto-commit nil
        company-minimum-prefix-length 1
        company-idle-delay 0.5
        company-selection-wrap-around t)

  ;; disable company auto completion at dape-repl-mode
  (add-hook 'dape-repl-mode-hook (lambda () (company-mode -1))))

(defun my/company-accept-and-trim-duplicate ()
  "Accept Company candidate and remove duplicated suffix ahead of point.
Example: 'material.pecular' + candidate 'materialSpecular'
→ leaves exactly 'materialSpecular'."
  (interactive)
  (when (and (bound-and-true-p company-candidates)
             (>= (or company-selection 0) 0))
    (let* ((cand (nth company-selection company-candidates))
           (ahead (save-excursion
                    (buffer-substring-no-properties
                     (point)
                     (progn (skip-chars-forward "_[:alnum:]") (point))))))
      ;; Do the normal insert first.
      (company-complete-selection)
      ;; Then trim any overlap between CAND's suffix and the text ahead.
      (when (and cand (> (length ahead) 0))
        (let ((n (cl-loop for i from (min (length ahead) (length cand)) downto 1
                          when (string-suffix-p (substring ahead 0 i) cand)
                          return i)))
          (when n (delete-char n)))))))

;;; rg
(set-popup-rule! "^\\*rg\\*$"
  :side 'bottom
  :size 0.5
  :slot 0
  :select t
  :quit t
  :ttl nil)   ;; keep window until explicitly closed

(advice-add 'rg-dwim :before #'my/evil-set-jump-before)

;; 1. bind the keys when rg-mode buffer opens
(add-hook 'rg-mode-hook
  (lambda ()
    ;; (evil-local-set-key 'normal (kbd "<return>") #'my/rg-goto)
    ;; (evil-local-set-key 'normal (kbd "RET")      #'my/rg-goto)
    (evil-local-set-key 'normal (kbd "q")        #'my/rg-quit-and-kill)))

;; ;; 2. 커서가 결과 텍스트(버튼) 위에 있을 때 동작하는 compilation-button-map 수정
;; (after! compile
;;   (define-key compilation-button-map (kbd "<return>") #'my/rg-goto)
;;   (define-key compilation-button-map (kbd "RET")      #'my/rg-goto))


;; (defun my/rg-goto ()
;;   "Jump to the search result under point, keeping the rg results window open.
;; Records an Evil jump at the position we're leaving, same as
;; `my/evil-set-jump-before' does elsewhere (init-behavior.el): call
;; `evil-set-jump' once, right before point moves, and nothing after --
;; an extra post-jump call would insert a bogus ring entry at the
;; destination that gets in the way of a clean `C-o' back. The rg
;; results buffer sits in its own popup window, so the jump has to be
;; set in the window `compile-goto-error' is about to jump into, before
;; that window's point gets overwritten with the match location."
;;   (interactive)
;;   (let ((dest-window (get-mru-window nil t)))
;;     (when (window-live-p dest-window)
;;       (with-selected-window dest-window
;;         (my/evil-set-jump-before)))
;;     (compile-goto-error)))

;; (defun my/rg-quit-and-kill ()
;;   "Delete the rg window and kill the rg buffer."
;;   (interactive)
;;   (let ((rg-buffer (current-buffer))
;;         (rg-window (selected-window)))
;;     (when (window-live-p rg-window)
;;       (delete-window rg-window))
;;     (when (buffer-live-p rg-buffer)
;;       (kill-buffer rg-buffer))))

(setq rg-custom-type-aliases
      '(("MyC" . "*.c *.cu *.cpp *.cc *.cxx *.h *.hpp")))

(provide 'init-utils)
;;; init-completion.el ends here
