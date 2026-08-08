;;; init-functions.el --- Utility functions -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; General-purpose utility functions - interactive commands not tied to any
;;; one package or mode. No configuration, hooks, or advice - just callable
;;; commands. Helpers specific to a single feature (e.g. rg, org, glsl, dape)
;;; live alongside that feature's own init-*.el instead.
;;; Code:

;;; Movement
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

(defun my/jump-matching-paren ()
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

;;; Selection & Region
(defun my/select-to-click (event)
  "Set EVENT at current position and extend selection to the position clicked with the mouse."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((pos (posn-point (event-end event))))
    (unless (region-active-p)
      (push-mark))
    (goto-char pos)
    (activate-mark)))

(defun my/select-symbol-at-point ()
  "Select the symbol (word with _ and letters) at point.
The region will deactivate automatically once you move the cursor."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if bounds
        (progn
          (goto-char (car bounds))
          (push-mark (cdr bounds) nil t)) ; transient mark
      (message "No symbol at point."))))

(defun my/evil-select-inside-paren ()
  "Visual-select text inside the nearest (), {}, or []."
  (interactive)
  (require 'evil)
  (condition-case nil
      (let* ((open (save-excursion (cond ((looking-at "\\s(\\|\\s{\\|\\s[") (point)) ((looking-back "\\s)\\|\\s}\\|\\s]" 1) (backward-sexp 1) (point)) (t (backward-up-list 1) (point))))) (close (save-excursion (goto-char open) (forward-sexp 1) (point))))
        (evil-visual-select (1+ open) (1- close) 'exclusive))
    (error (user-error "No surrounding list found"))))

;;; Snippets & Insertion
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

;;; Save & Escape
(defun my/save-and-escape ()
  "save-buffer and back to normal state"
  (interactive)
  (call-interactively #'save-buffer)
  (when (fboundp 'evil-escape)
    (call-interactively #'evil-escape)))

(defun my/insert-escape-and-clear ()
  "Escape from insert mode and deactivate any active region."
  (interactive)
  (evil-escape)
  (run-at-time 0 nil
               (lambda ()
                 (when (use-region-p) (deactivate-mark))
                 (when (evil-insert-state-p) (evil-normal-state)))))

(provide 'init-functions)
;;; init-functions.el ends here
