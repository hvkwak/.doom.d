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

(defun my/toggle-between-header-and-source ()
  "Toggle between C/C++/CUDA header and source file.
Assumes project layout with `src/` and `include/` at the root."
  (interactive)
  (let* ((file (or (buffer-file-name) (user-error "Not visiting a file")))
         (ext  (downcase (or (file-name-extension file) "")))
         (name (file-name-base file))
         (root (locate-dominating-file file "src"))
         ;; build candidate paths
         (src   (expand-file-name (concat "src/" name ".c") root))
         (src++ (expand-file-name (concat "src/" name ".cpp") root))
         (src-cu (expand-file-name (concat "src/" name ".cu") root))
         (hdr   (expand-file-name (concat "include/" name ".h") root))
         (hdr++ (expand-file-name (concat "include/" name ".hpp") root)))
    (cond
     ;; if we’re in a source → look for header
     ((member ext '("c" "cpp" "cc" "cxx" "cu"))
      (cond
       ((file-exists-p hdr)   (find-file hdr))
       ((file-exists-p hdr++) (find-file hdr++))
       (t (message "No matching header found."))))
     ;; if we’re in a header → look for source
     ((member ext '("h" "hh" "hpp" "hxx"))
      (cond
       ((file-exists-p src)   (find-file src))
       ((file-exists-p src++) (find-file src++))
       ((file-exists-p src-cu)(find-file src-cu))
       (t (message "No matching source found."))))
     (t (message "Not a C/C++/CUDA file.")))))

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
  (define-key vterm-mode-map (kbd "M-f") nil)
  (define-key vterm-mode-map (kbd "M-8") #'switch-to-prev-buffer)
  (define-key vterm-mode-map (kbd "M-9") #'switch-to-next-buffer)
  (define-key vterm-mode-map (kbd "M-f") #'+vertico/switch-workspace-buffer)
  )

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

(defun my-next-3-lines ()
  (interactive)
  (dotimes (_ 3) (next-line 1)))

(defun my-previous-3-lines ()
  (interactive)
  (dotimes (_ 3) (previous-line 1)))

(global-set-key (kbd "C-<up>")  'my-previous-3-lines)
(global-set-key (kbd "C-<down>")    'my-next-3-lines)

(after! flycheck
  (global-flycheck-mode -1)
  (setq flycheck-global-modes nil))

(defun my-jump-matching-paren ()
  "Jump to the matching parenthesis/bracket/brace.
If point is on an opening, go forward. If on a closing, go backward."
  (interactive)
  (cond
   ((looking-at "\\s(") (forward-sexp 1))
   ((looking-at "\\s{") (forward-sexp 1))
   ((looking-at "\\s[") (forward-sexp 1))
   ((looking-back "\\s)" 1) (backward-sexp 1))
   ((looking-back "\\s}" 1) (backward-sexp 1))
   ((looking-back "\\s]" 1) (backward-sexp 1))
   (t (user-error "Not on a paren/brace/bracket"))))

;; bind it globally (pick your key)
(global-set-key (kbd "C-%") #'my-jump-matching-paren)


(provide 'init-behavior)
;;; init-behavior.el ends here
