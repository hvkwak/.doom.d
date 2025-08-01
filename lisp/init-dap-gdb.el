;;; init-dap-gdb.el --- dap-gdb configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
(use-package! dap-mode
  :defer t
  :init
  ;; Auto configure no features.
  (setq dap-auto-configure-features '())

  :config
  (require 'dap-gdb)
  (dap-register-debug-template
  "GDB::Run with arguments"
  (list :type "gdb"
        :request "launch"
        :name "GDB::Run with arguments"
        :arguments "0 0 0 0"
        :target nil
        :cwd nil))
)


(defun my/new-frame-on-second-monitor ()
  "Create a new frame named 'Debugger Frame' on the second monitor with an empty buffer."
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
      (switch-to-buffer (current-buffer))))
  (toggle-frame-maximized)
  )

(defvar my/dap-debug-frame nil
  "Reference to the frame used during DAP debugging.")

(defun my/dap-debugger-setting ()
  "Custom function to run when a DAP session is created."
  (interactive)
  ;; Create and mark the new frame
  (let ((new-frame (my/new-frame-on-second-monitor)))
    (when (frame-live-p new-frame)
      (set-frame-parameter new-frame 'my-dap-frame t)
      (setq my/dap-debug-frame new-frame)
      (select-frame-set-input-focus new-frame)))

  ;; Open useful DAP UI windows
  ;;(dap-ui-sessions)
  (dap-ui-locals)
  ;;(dap-ui-repl)
  ;;(dap-ui-expressions)
  (dap-ui-breakpoints)

  ;; Clean up dummy window
  (delete-windows-on "*dummy*"))

(defun my/dap-debug-close ()
  "Gracefully close DAP session and associated UI frame."
  (interactive)

  ;; Disconnect from current session
  (condition-case nil
      (let ((session (dap--cur-session)))
        (when session
          (dap-disconnect session)))
    (error (message "DAP disconnect failed")))

  ;; Delete the custom DAP frame if it exists
  (when (and my/dap-debug-frame (frame-live-p my/dap-debug-frame))
    (condition-case nil
        (progn
          (select-frame-set-input-focus my/dap-debug-frame)
          (select-frame my/dap-debug-frame)
          (delete-frame my/dap-debug-frame))
      (error (message "Failed to delete DAP debug frame")))
    (setq my/dap-debug-frame nil))

  ;; Delete DAP-related buffers
  (dolist (buf '("*dap-ui-breakpoints*"
                 "*dap-ui-locals*"
                 "*dap-ui-sessions*"
                 "*dap-ui-expressions*"
                 "*dap-ui-repl*"
                 "*GDB::Run out*"
                 "*dummy*"))
    (when (get-buffer buf)
      (kill-buffer buf)))

  ;; Delete GDB::Run buffers
  (dolist (buf (buffer-list))
    (let ((name (buffer-name buf)))
      (when (and name (string-prefix-p "*GDB::Run" name))
        (kill-buffer buf))))

  ;; Delete LLDB buffers
  (dolist (buf (buffer-list))
    (let ((name (buffer-name buf)))
      (when (and name (string-prefix-p "*LLDB" name))
        (kill-buffer buf))))
)

(defun my/dap-ui--show-buffer (buf)
  "Show BUF according to defined rules."
  (when-let (win (display-buffer buf))
    (set-window-dedicated-p win t)
    (select-window win))
)
(advice-add 'dap-ui--show-buffer :override #'my/dap-ui--show-buffer)


;; display-buffer-alist for 'my-dap-debugger-setting
(setq display-buffer-alist
      '(("\\*Consult Project Buffer\\*" ;; Match buffer name
         (display-buffer-in-side-window)
         (window-height . 0.4)  ;; Adjust height (40% of the frame)
         (side . bottom))
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
        ("\\*GDB::Run.*\\*"
         (display-buffer-use-some-frame
          display-buffer-in-side-window)
         (inhibit-same-window . t)
         (reusable-frames . "Debugger Frame"))
        ;; ("\\*Occur\\*"  ;; Match debugger buffers (*gud*, *gud-session*, etc.)
        ;;  (display-buffer-use-some-frame)
        ;;  (inhibit-same-window . t)
        ;;  (reusable-frames . "Debugger Frame"))
        ))

(provide 'init-dap-gdb)
;;; init-dap-gdb.el ends here
