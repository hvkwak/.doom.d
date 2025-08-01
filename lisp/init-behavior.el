;;; init-behavior.el --- Change how my Doom Emacs behaves -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(setq native-comp-jit-compilation t) ;; nil would disable native-compilation entirely.


(defun eval-buffer-by-name (buffer-name)
  "Evaluate the buffer with the given BUFFER-NAME."
  (interactive "BBuffer name: ")
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
  "Toggle between a C++ header file and its corresponding source file."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (if (string-match-p "\\.cpp$" current-file)
        ;; If the current file is a .cpp file, find the corresponding .h file
        (let ((header-file (replace-regexp-in-string "/src/" "/include/"
                          (replace-regexp-in-string "\\.cpp$" ".h" current-file))))
          (if (file-exists-p header-file)
              (find-file header-file)
            (message "Header file does not exist.")))
      ;; If the current file is a .h file, find the corresponding .cpp file
      (let ((source-file (replace-regexp-in-string "/include/" "/src/"
                       (replace-regexp-in-string "\\.h$" ".cpp" current-file))))
        (if (file-exists-p source-file)
            (find-file source-file)
          (message "Source file does not exist."))))))

(defun my/c-move-to-next-arg ()
  "Move cursor to the beginning of the next argument inside function call."
  (interactive)
  (when (looking-at "[^,)]+")
    (goto-char (match-end 0)))
  (skip-chars-forward " \t")
  (if (looking-at ",")
      (progn
        (forward-char)
        (skip-chars-forward " \t"))
    (when (looking-at ")")
      (forward-char))))

(defun my/c-move-to-prev-arg ()
  "Move cursor to the beginning of the previous argument inside a function call."
  (interactive)
  (skip-chars-backward " \t")
  (when (looking-back "," (1- (point)))
    (backward-char))
  (when (re-search-backward "[,(]" nil t)
    (forward-char)
    (skip-chars-forward " \t")))

(provide 'init-behavior)
;;; init-behavior.el ends here
