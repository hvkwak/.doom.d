;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dark+)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;;
;;
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general changes and completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq native-comp-deferred-compilation nil) ;; This would disable native-compilation entirely.
(menu-bar-mode 1)

;; directly modify Doom's theme settings using custom-set-faces
;; (after! solaire-mode
;;   (solaire-global-mode -1))

(custom-set-faces
  ;;'(default ((t (:background "#000000"))))
  ;;'(hl-line ((t (:background "#000000"))))
  '(cursor ((t (:background "green")))) ;; change cursor background to green.
  )

;; keyboard cursor
(setq-default cursor-type 'bar)
(blink-cursor-mode 1)

;; mouse cursor
(setq-default void-text-area-pointer 'nil)
(setq scroll-preserve-screen-position t)

(after! treemacs
  (setq treemacs-project-follow-mode t)
  (treemacs-follow-mode t))

(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-position 'right
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-peek-enable t))

;; packages for better completion and regex
(use-package! marginalia
  :general
  (:keymaps 'minibuffer-local-map
            "M-A" 'marginalia-cycle)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package! consult
  :after projectile
  :config
  (setq consult-preview-key 'any) ;; Preview instantly as you cycle
  (setq consult-buffer-sources
      '(consult--source-project-buffer)  ;; Show only project buffers
      ;; '(consult--source-project-buffer  ;; Show only project buffers and recent files
      ;;   consult--source-recent-file)
      )
  (setq consult-project-function #'projectile-project-root)

  ;; filter out several unrelated buffers
  (setq consult-buffer-filter '("\\` \\*"
                                "\\`\\*scratch\\*"
                                "\\`\\*Messages\\*"
                                "\\`\\*doom\\*"
                                "\\`\\*lsp-log\\*"
                                "\\`\\*Native-compile-Log\\*"
                                "\\`\\*Ibuffer\\*"
                                "\\`\\bash-completion\\*"
                                )))

(use-package! orderless
  :custom
  (completion-styles '(orderless))      ; Use orderless
  (completion-category-defaults nil)    ; I want to be in control!
  (completion-category-overrides
   '((file (styles basic-remote ; For `tramp' hostname completion with `vertico'
                   orderless))))
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
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugging: dap-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! dap-mode
  :defer t
  :custom
  (dap-auto-configure-mode t "Automatically configure dap.")
  ;;(dap-auto-configure-features '(sessions locals breakpoints expressions tooltip)  "Remove the button panel in the top.")
  (dap-auto-configure-features '(tooltip) "no auto configure features.")

  :config
  (require 'dap-lldb)
  (require 'dap-cpptools)

  ;;; set the debugger executable (c++)
  (setq dap-lldb-debug-program '("/usr/bin/lldb-vscode"))

  ;;; ask user for executable to debug if not specified explicitly (c++)
  (setq dap-lldb-debugged-program-function (lambda () (read-file-name "Select file to debug.")))

  ;;; default debug template for (c++)
  (dap-register-debug-template
   "C++ LLDB dap"
   (list :type "lldb-vscode"
         :cwd (projectile-project-root)
         :args nil
         :request "launch"
         :program (concat (projectile-project-root) "build/main"))
   )

  (defun dap-debug-create-or-edit-json-template ()
    "Edit the C++ debugging configuration or create + edit if none exists yet."
    (interactive)
    (let ((filename (concat (lsp-workspace-root) "/launch.json"))
          (default "~/.emacs.d/default-launch.json"))
      (unless (file-exists-p filename)
        (copy-file default filename))
      (find-file-existing filename))))

(defun new-frame-on-second-monitor ()
  "Create a new frame named 'My Frame' on the second monitor with an empty buffer."
  (interactive)
  (let* ((display-geometry (display-monitor-attributes-list))
         (second-monitor (nth 1 display-geometry)) ; Adjust index if needed, 1: second monitor.
         (geometry (alist-get 'geometry second-monitor))
         (x (nth 0 geometry))
         (y (nth 1 geometry))
         (frame (make-frame `((name . "Debugger Frame")
                              (left . ,x)
                              (top . ,y)
                              ;;(fullscreen . fullboth) ;; toggle-frame-full screen will do.
                              (width . 1600)    ; Adjust frame width if needed
                              (height . 900)
                              )))) ; Adjust frame height if needed
    (select-frame-set-input-focus frame)
    (with-current-buffer (generate-new-buffer "*dummy*")
      (switch-to-buffer (current-buffer)))))

(defun my-dap-debug ()
  "runs dap-debug with the template defined above."
  (interactive)
  (let ((config (cdr (assoc "C++ LLDB dap" dap-debug-template-configurations))))
    (if config
        (dap-debug config)
      (message "Debug configuration not found!"))
    ;; (if config
    ;;     (my-debugger-setting)
    ;;   (message "Debug configuration not found!"))
  )
)

(defun my-dap-debugger-setting ()
  "Custom function to run when a DAP session is created."
  (interactive)
  (set-frame-name "main")
  (new-frame-on-second-monitor)
  (toggle-frame-fullscreen)
  (dap-ui-breakpoints)
  (dap-ui-sessions)
  (dap-ui-locals)
  (dap-ui-repl)
  (dap-ui-expressions)
  (delete-windows-on "*dummy*")
  (select-frame-by-name "main")
  )

(defun my-dap-debug-close ()
  "closes dap-debug session including dap-disconnect."
  (interactive)
  (dap-disconnect (dap--cur-session)) ;; (mapc #'dap-disconnect (dap--get-sessions)) is alternative to disconnect all sessions.
  (select-frame-by-name "Debugger Frame")
  (doom/delete-frame-with-prompt)
  (kill-buffer "*dap-ui-breakpoints*")
  (kill-buffer "*dap-ui-locals*")
  (kill-buffer "*dap-ui-sessions*")
  (kill-buffer "*dap-ui-expressions*")
  (kill-buffer "*dap-ui-repl*")
  (kill-llldb-dap-buffers) ;; kills buffer 'C++ LLDB dap*'
  (kill-buffer "*dummy*")
  )

(defun kill-llldb-dap-buffers ()
  "Kill all buffers whose name starts with '*C++ LLDB dap'."
  (dolist (buf (buffer-list))
    (when (string-match-p "LLDB dap" (buffer-name buf))
      (kill-buffer buf))))

(defun my-dap-ui--show-buffer (buf)
  "Show BUF according to defined rules."
  (when-let (win (display-buffer buf))
    (set-window-dedicated-p win t)
    (select-window win))
)
(advice-add 'dap-ui--show-buffer :override #'my-dap-ui--show-buffer)


;; display-buffer-alist for 'my-dap-debugger-setting
(setq display-buffer-alist
      '(("\\*Consult Project Buffer\\*" ;; Match buffer name
         (display-buffer-in-side-window)
         (window-height . 0.4)  ;; Adjust height (40% of the frame)
         (side . bottom))
        ;; ("\\*Consult Buffer\\*" ;; Match buffer name
        ;;  (display-buffer-in-side-window)
        ;;  (window-height . 0.4)  ;; Adjust height (40% of the frame)
        ;;  (side . bottom))
        ("\\*dap-ui-breakpoints\\*"
         (display-buffer-use-some-frame
          display-buffer-in-side-window)
         (inhibit-same-window . t)
         (reusable-frames . "Debugger Frame"))
        ("\\*dap-ui-locals\\*"
         (display-buffer-use-some-frame
          display-buffer-in-side-window)
         (inhibit-same-window . t)
         (reusable-frames . "Debugger Frame"))
        ("\\*dap-ui-sessions\\*"
         (display-buffer-use-some-frame
          display-buffer-in-side-window)
         (inhibit-same-window . t)
         (reusable-frames . "Debugger Frame"))
        ("\\*dap-ui-expressions\\*"
         (display-buffer-use-some-frame
          display-buffer-in-side-window)
         (inhibit-same-window . t)
         (reusable-frames . "Debugger Frame"))
        ("\\*dap-ui-repl\\*"
         (display-buffer-use-some-frame
          display-buffer-in-side-window)
         (inhibit-same-window . t)
         (reusable-frames . "Debugger Frame"))
        ;; ("\\*Occur\\*"  ;; Match debugger buffers (*gud*, *gud-session*, etc.)
        ;;  (display-buffer-use-some-frame)
        ;;  (inhibit-same-window . t)
        ;;  (reusable-frames . "Debugger Frame"))
        ))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projectile                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! projectile ;; Set the compile command for CMake projects
  (setq projectile-project-compilation-cmd "cmake -S . -B build && cmake --build build")
  (setq projectile-project-run-cmd "./build/main")
  (setq compile-command "rm -r build && mkdir build && cmake -S . -B build && cmake --build build")
  )

(use-package! consult-projectile
  :after (consult projectile)
  :bind (("C-c p f" . consult-projectile-find-file)
         ("C-c p p" . consult-projectile-switch-project)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; util functions                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eval-buffer-by-name (buffer-name)
  "Evaluate the buffer with the given BUFFER-NAME."
  (interactive "BBuffer name: ")
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (eval-buffer))))

(defun eval-buffer-and-close ()
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
 of the line."
  (interactive)
  (let ((orig-pos (point)))
    (back-to-indentation)
    (when (= orig-pos (point))
      (move-beginning-of-line 1))))

(defun my/select-to-click (event)
  "Set mark at current position and extend selection to the position
  clicked with the mouse."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((pos (posn-point (event-end event))))
    (unless (region-active-p)
      (push-mark))
    (goto-char pos)
    (activate-mark)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bindings                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move
(global-set-key (kbd "M-i")  'previous-line)
(global-set-key (kbd "M-k")  'next-line)
(global-set-key (kbd "M-j")  'backward-char)
(global-set-key (kbd "M-l")  'forward-char)
(global-set-key (kbd "C-j")  'windmove-left) ;; moving around windows
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-i")    'windmove-up)
(global-set-key (kbd "C-k")  'windmove-down)

(global-set-key (kbd "C-c t t")  'treemacs)
(global-set-key (kbd "C-e")  'eval-buffer-and-close) ;; debug template
(global-set-key (kbd "<f5>")  'my-dap-debug)
(global-set-key (kbd "<f6>")  'my-dap-debugger-setting)
(global-set-key (kbd "<f8>")  'my-dap-debug-close)
(global-set-key (kbd "C-z")  'undo)

(global-set-key (kbd "<home>")  'smart-beginning-of-line) ;; home
;; (global-set-key (kbd "M-h")  'smart-beginning-of-line) ;; home
;; (global-set-key (kbd "M-S-l")  'move-end-of-line) ;; end
;; (global-set-key (kbd "M-S-i")  'scroll-up-command) ;; PgUp
;; (global-set-key (kbd "M-S-k")  'scroll-down-command) ;; PgDn
(global-set-key (kbd "M-\\")  'delete-char)

;; search functions - consult
(global-set-key (kbd "C-<tab>")  'consult-buffer)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-f") 'consult-line)
(global-set-key (kbd "C-S-s") 'consult-ripgrep)

;; Shift + mouse click
(global-set-key [S-down-mouse-1] 'ignore)  ; Ignore the initial mouse down event
(global-set-key [S-mouse-1] 'my/select-to-click)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keep this for later use, practical use of overriding.                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun my-message (format-string &rest args)
;;   "Custom message function that logs to a file."
;;   (with-temp-file "~/emacs_log.txt"
;;     (insert (apply #'format format-string args)))
;;   (apply #'message format-string args))  ; Call original message function

;; (advice-add 'message :override #'my-message)
