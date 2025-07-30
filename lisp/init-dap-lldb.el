;;; init-dap-lldb.el --- DAP Mode-LLDB Configurations -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary: disabled. Use init-dap-gdb.el instead.

;;; Code:
;; (use-package! dap-mode
;;   :defer t
;;   :custom
;;   (dap-auto-configure-mode t "Automatically configure dap.")
;;   ;;(dap-auto-configure-features '(sessions locals breakpoints expressions tooltip)  "Remove the button panel in the top.")
;;   (dap-auto-configure-features '(tooltip) "no auto configure features.")

;;   :config
;;   (require 'dap-lldb)
;;   (require 'dap-cpptools)

;;   ;;; set the debugger executable (c++)
;;   (setq dap-lldb-debug-program '("/usr/bin/lldb-vscode"))

;;   ;;; ask user for executable to debug if not specified explicitly (c++)
;;   (setq dap-lldb-debugged-program-function (lambda () (read-file-name "Select file to debug.")))

;;   ;;; default debug template for (c++)
;;   (dap-register-debug-template
;;    "C++ LLDB dap"
;;    (list :type "lldb-vscode"
;;          :cwd (projectile-project-root)
;;          :args nil
;;          :request "launch"
;;          :program (concat (projectile-project-root) "build/main"))
;;    )

;;   (defun dap-debug-create-or-edit-json-template ()
;;     "Edit the C++ debugging configuration or create + edit if none exists yet."
;;     (interactive)
;;     (let ((filename (concat (lsp-workspace-root) "/launch.json"))
;;           (default "~/.emacs.d/default-launch.json"))
;;       (unless (file-exists-p filename)
;;         (copy-file default filename))
;;       (find-file-existing filename))))

;; (defun new-frame-on-second-monitor ()
;;   "Create a new frame named 'My Frame' on the second monitor with an empty buffer."
;;   (interactive)
;;   (let* ((display-geometry (display-monitor-attributes-list))
;;          (second-monitor (nth 1 display-geometry)) ; Adjust index if needed, 1: second monitor.
;;          (geometry (alist-get 'geometry second-monitor))
;;          (x (nth 0 geometry))
;;          (y (nth 1 geometry))
;;          (frame (make-frame `((name . "Debugger Frame")
;;                               (left . ,x)
;;                               (top . ,y)
;;                               ;;(fullscreen . fullboth) ;; toggle-frame-full screen will do.
;;                               (width . 1600)    ; Adjust frame width if needed
;;                               (height . 900)
;;                               )))) ; Adjust frame height if needed
;;     (select-frame-set-input-focus frame)
;;     (with-current-buffer (generate-new-buffer "*dummy*")
;;       (switch-to-buffer (current-buffer)))))

;; (defun my-dap-debug ()
;;   "runs dap-debug with the template defined above."
;;   (interactive)
;;   (let ((config (cdr (assoc "C++ LLDB dap" dap-debug-template-configurations))))
;;     (if config
;;         (dap-debug config)
;;       (message "Debug configuration not found!"))
;;     ;; (if config
;;     ;;     (my-debugger-setting)
;;     ;;   (message "Debug configuration not found!"))
;;   )
;; )

;; (defun my-dap-debugger-setting ()
;;   "Custom function to run when a DAP session is created."
;;   (interactive)
;;   (set-frame-name "main")
;;   (new-frame-on-second-monitor)
;;   (toggle-frame-fullscreen)
;;   (dap-ui-breakpoints)
;;   (dap-ui-sessions)
;;   (dap-ui-locals)
;;   (dap-ui-repl)
;;   (dap-ui-expressions)
;;   (delete-windows-on "*dummy*")
;;   (select-frame-by-name "main")
;;   )

;; (defun my-dap-debug-close ()
;;   "closes dap-debug session including dap-disconnect."
;;   (interactive)
;;   (dap-disconnect (dap--cur-session)) ;; (mapc #'dap-disconnect (dap--get-sessions)) is alternative to disconnect all sessions.
;;   (select-frame-by-name "Debugger Frame")
;;   (doom/delete-frame-with-prompt)
;;   (kill-buffer "*dap-ui-breakpoints*")
;;   (kill-buffer "*dap-ui-locals*")
;;   (kill-buffer "*dap-ui-sessions*")
;;   (kill-buffer "*dap-ui-expressions*")
;;   (kill-buffer "*dap-ui-repl*")
;;   (kill-llldb-dap-buffers) ;; kills buffer 'C++ LLDB dap*'
;;   (kill-buffer "*dummy*")
;;   )

;; (defun kill-llldb-dap-buffers ()
;;   "Kill all buffers whose name starts with '*C++ LLDB dap'."
;;   (dolist (buf (buffer-list))
;;     (when (string-match-p "LLDB dap" (buffer-name buf))
;;       (kill-buffer buf))))

;; (defun my-dap-ui--show-buffer (buf)
;;   "Show BUF according to defined rules."
;;   (when-let (win (display-buffer buf))
;;     (set-window-dedicated-p win t)
;;     (select-window win))
;; )
;; (advice-add 'dap-ui--show-buffer :override #'my-dap-ui--show-buffer)


;; ;; display-buffer-alist for 'my-dap-debugger-setting
;; (setq display-buffer-alist
;;       '(("\\*Consult Project Buffer\\*" ;; Match buffer name
;;          (display-buffer-in-side-window)
;;          (window-height . 0.4)  ;; Adjust height (40% of the frame)
;;          (side . bottom))
;;         ;; ("\\*Consult Buffer\\*" ;; Match buffer name
;;         ;;  (display-buffer-in-side-window)
;;         ;;  (window-height . 0.4)  ;; Adjust height (40% of the frame)
;;         ;;  (side . bottom))
;;         ("\\*dap-ui-breakpoints\\*"
;;          (display-buffer-use-some-frame
;;           display-buffer-in-side-window)
;;          (inhibit-same-window . t)
;;          (reusable-frames . "Debugger Frame"))
;;         ("\\*dap-ui-locals\\*"
;;          (display-buffer-use-some-frame
;;           display-buffer-in-side-window)
;;          (inhibit-same-window . t)
;;          (reusable-frames . "Debugger Frame"))
;;         ("\\*dap-ui-sessions\\*"
;;          (display-buffer-use-some-frame
;;           display-buffer-in-side-window)
;;          (inhibit-same-window . t)
;;          (reusable-frames . "Debugger Frame"))
;;         ("\\*dap-ui-expressions\\*"
;;          (display-buffer-use-some-frame
;;           display-buffer-in-side-window)
;;          (inhibit-same-window . t)
;;          (reusable-frames . "Debugger Frame"))
;;         ("\\*dap-ui-repl\\*"
;;          (display-buffer-use-some-frame
;;           display-buffer-in-side-window)
;;          (inhibit-same-window . t)
;;          (reusable-frames . "Debugger Frame"))
;;         ;; ("\\*Occur\\*"  ;; Match debugger buffers (*gud*, *gud-session*, etc.)
;;         ;;  (display-buffer-use-some-frame)
;;         ;;  (inhibit-same-window . t)
;;         ;;  (reusable-frames . "Debugger Frame"))
;;         ))

(provide 'init-dap-lldb)
;;; init-dap-lldb.el ends here
