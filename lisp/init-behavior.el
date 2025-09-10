;;; init-behavior.el --- Change how my Doom Emacs behaves -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
(setq native-comp-jit-compilation t)

(defun eval-buffer-by-name (buffer-name)
  "Evaluate the buffer with the given BUFFER-NAME."
  (interactive "Buffer name: ")
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (eval-buffer))))

(defun eval-buffer-and-close ()
  "Eval buffer and close.."
  (interactive)
  (eval-buffer-by-name "*DAP Templates*")
  (+workspace/close-window-or-workspace)
  )

(defun my-indent-setup ()
  "Set up the TAB key to indent with a single press."
  (local-set-key (kbd "<tab>") 'indent-for-tab-command))

(add-hook 'prog-mode-hook 'my-indent-setup)  ; For programming modes
(add-hook 'text-mode-hook 'my-indent-setup)  ; For text modes

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
If point is already at the beginning of the line, move to the beginning of the
line. If point is at the first non-whitespace character, move to the beginning
of the line. Extend the selection when used with the Shift key."
  (interactive "^")  ; The caret (^) makes the command support shift-selection
  (let ((orig-pos (point)))
    (back-to-indentation)
    (when (= orig-pos (point))
      (move-beginning-of-line 1))))

(defun my/select-to-click (event)
  "Set EVENT at current position and extend selection to the position clicked with the mouse."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((pos (posn-point (event-end event))))
    (unless (region-active-p)
      (push-mark))
    (goto-char pos)
    (activate-mark)))

;; (defun my/toggle-between-header-and-source ()
;;   "Toggle between a C++ header file and its corresponding source file."
;;   (interactive)
;;   (let ((current-file (buffer-file-name)))
;;     (if (string-match-p "\\.cpp$" current-file)
;;         ;; If the current file is a .cpp file, find the corresponding .h file
;;         (let ((header-file (replace-regexp-in-string "/src/" "/include/"
;;                           (replace-regexp-in-string "\\.cpp$" ".h" current-file))))
;;           (if (file-exists-p header-file)
;;               (find-file header-file)
;;             (message "Header file does not exist.")))
;;       ;; If the current file is a .h file, find the corresponding .cpp file
;;       (let ((source-file (replace-regexp-in-string "/include/" "/src/"
;;                        (replace-regexp-in-string "\\.h$" ".cpp" current-file))))
;;         (if (file-exists-p source-file)
;;             (find-file source-file)
;;           (message "Source file does not exist."))))))

;; (defun my/c-move-to-next-arg ()
;;   "Move cursor to the beginning of the next argument inside function call."
;;   (interactive)
;;   (when (looking-at "[^,)]+")
;;     (goto-char (match-end 0)))
;;   (skip-chars-forward " \t")
;;   (if (looking-at ",")
;;       (progn
;;         (forward-char)
;;         (skip-chars-forward " \t"))
;;     (when (looking-at ")")
;;       (forward-char))))

;; (defun my/c-move-to-prev-arg ()
;;   "Move cursor to the beginning of the previous argument inside a function call."
;;   (interactive)
;;   (skip-chars-backward " \t")
;;   (when (looking-back "," (1- (point)))
;;     (backward-char))
;;   (when (re-search-backward "[,(]" nil t)
;;     (forward-char)
;;     (skip-chars-forward " \t")))

(defun save-all-c-h-buffers ()
  "Save all open buffers visiting .c or .h files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (let ((file (buffer-file-name)))
        (when (and file
                   (or (string-suffix-p ".c" file t)
                       (string-suffix-p ".h" file t)
                       (string-suffix-p ".cpp" file t)
                       (string-suffix-p ".py" file t))
          (save-buffer))))))
  (message "Saved all files."))

(defun insert-doxygen-function-comment ()
  "Insert a Doxygen-style comment block above a function."
  (interactive)
  (beginning-of-line)
  (insert "/**\n")
  (insert " * @brief \n")
  (insert " * \n")
  (insert " * @param \n")
  (insert " * @return \n")
  (insert " */"))

(defun my/vterm-init ()
  "Automatically source .profile at vterm start."
  (sleep-for 2)
  (vterm-send-string "source ~/.profile" t)
  (vterm-send-return))
(add-hook 'vterm-mode-hook #'my/vterm-init)

(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "M-8") nil)
  (define-key vterm-mode-map (kbd "M-9") nil)
  (define-key vterm-mode-map (kbd "M-8") #'switch-to-prev-buffer)
  (define-key vterm-mode-map (kbd "M-9") #'switch-to-next-buffer))

;; add jump points when using beginning-of-buffer.
(after! better-jumper
  (advice-add 'beginning-of-buffer :before #'better-jumper-set-jump))

;; add jump points when using consult-line or consult-ripgrip
;;; Make Consult-confirmed jumps integrate with better-jumper
(defun my/better-jumper-before-consult-jump (orig-fn &rest args)
  "Record a jump with better-jumper just before Consult performs a real jump."
  (better-jumper-set-jump)
  (apply orig-fn args))

;; Consult uses these internal functions when you confirm a candidate.
;; We advise them so previews don't spam the jump list, only confirmed jumps do.
(dolist (fn '(consult--jump consult--goto-location))
  (when (fboundp fn)
    (advice-add fn :around #'my/better-jumper-before-consult-jump)))

(defun my/select-symbol-at-point ()
  "Select the symbol (word with _ and letters) at point.
The region will deactivate automatically once you move the cursor."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if bounds
        (progn
          (goto-char (car bounds))
          (push-mark (cdr bounds) nil t)) ; transient mark
      (message "No symbol at point."))
  )
)

;; cc-mode overrides: unbind M-e so the global takes effect
;; M-e is used for "M-e"       #'centaur-tabs-forward
;; (after! cc-mode
;;   (dolist (map (list c-mode-base-map c++-mode-map))
;;     (define-key map (kbd "M-e") nil)))

(custom-set-faces
 '(centaur-tabs-selected ((t (:background "#2b2b2b" :foreground "#c3e88d" :weight bold))))
 '(centaur-tabs-selected-modified ((t (:background "#2b2b2b" :foreground "#c3e88d" :weight bold))))
 '(centaur-tabs-unselected ((t (:background "#2b2b2b" :foreground "#888888"))))
 '(centaur-tabs-unselected-modified ((t (:background "#2b2b2b" :foreground "#888888"))))
 )



(provide 'init-behavior)
;;; init-behavior.el ends here
