;;; init-functions.el --- Utility functions -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Pure utility functions - all defun declarations that are interactive commands.
;;; No configuration, hooks, or advice - just callable commands.
;;; Code:

;;; Buffer & Evaluation
(defun eval-buffer-by-name (buffer-name)
  "Evaluate the buffer with the given BUFFER-NAME."
  (interactive "Buffer name: ")
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (eval-buffer))))

(defun eval-buffer-and-close ()
  "Eval buffer and close."
  (interactive)
  (eval-buffer-by-name "*DAP Templates*")
  (+workspace/close-window-or-workspace))

;;; Navigation & Movement
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
     ;; if we're in a source → look for header
     ((member ext '("c" "cpp" "cc" "cxx" "cu"))
      (cond
       ((file-exists-p hdr)   (find-file hdr))
       ((file-exists-p hdr++) (find-file hdr++))
       (t (message "No matching header found."))))
     ;; if we're in a header → look for source
     ((member ext '("h" "hh" "hpp" "hxx"))
      (cond
       ((file-exists-p src)   (find-file src))
       ((file-exists-p src++) (find-file src++))
       ((file-exists-p src-cu)(find-file src-cu))
       (t (message "No matching source found."))))
     (t (message "Not a C/C++/CUDA file.")))))

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

(defun my/select-current-line ()
  "Select the current line. Repeat to extend selection by line."
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (if (use-region-p)
        ;; Extend the region
        (set-mark (region-beginning))
      ;; Start new region
      (set-mark start))
    (goto-char end)))

;;; Parentheses & Matching
(defun my/evil-select-inside-paren ()
  "Visual-select text inside the nearest (), {}, or []."
  (interactive)
  (require 'evil)
  (condition-case nil
      (let* ((open (save-excursion (cond ((looking-at "\\s(\\|\\s{\\|\\s[") (point)) ((looking-back "\\s)\\|\\s}\\|\\s]" 1) (backward-sexp 1) (point)) (t (backward-up-list 1) (point))))) (close (save-excursion (goto-char open) (forward-sexp 1) (point))))
        (evil-visual-select (1+ open) (1- close) 'exclusive))
    (error (user-error "No surrounding list found"))))

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

;;; File Operations
(defun save-all-c-h-buffers ()
  "Save all open buffers visiting C/C++ source files (.c, .h, .cpp) and Python files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (let ((file (buffer-file-name)))
        (when (and file
                   (or (string-suffix-p ".c" file t)
                       (string-suffix-p ".h" file t)
                       (string-suffix-p ".cpp" file t)
                       (string-suffix-p ".py" file t)))
          (save-buffer)))))
  (message "Saved all C/C++/Python files."))

;;; Code Documentation
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

;;; Debugging & Inspection
(defun my/locate-key (key)
  "Show which active keymaps bind KEY (highest precedence first)."
  (interactive "kKey: ")
  (require 'cl-lib)
  (let* ((maps (current-active-maps t))
         (res (cl-loop for m in maps
                       for b = (lookup-key m key)
                       when (and b (not (numberp b)))
                       collect (list
                                ;; try to name the map (minor mode name if possible)
                                (car (rassq m minor-mode-map-alist))
                                m
                                b))))
    (with-current-buffer (get-buffer-create "*Key Locator*")
      (erase-buffer)
      (dolist (x res)
        (pcase-let ((`(,minor-name ,map ,binding) x))
          (princ (format "%-32s %-32S → %S\n"
                         (or minor-name "") map binding)
                 (current-buffer))))
      (display-buffer (current-buffer)))))

(provide 'init-functions)
;;; init-functions.el ends here
