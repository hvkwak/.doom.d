;;; init-dap-gdb.el --- dap-gdb configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(after! dap-mode
  (require 'dap-gdb)
  (setq dap-auto-configure-features '())

  ;; Debug template without arguments
  (dap-register-debug-template
   "GDB::Run"
   (list :type "gdb"
         :request "launch"
         :name "GDB::Run with arguments"
         :arguments ""
         :target nil
         :cwd nil))
  )

(defconst my/dap-debug-frame-name "Debugger Frame")

(defvar my/main-frame-name nil
  "Holds the name of the last selected frame.")

(defvar my/dap-debug-frame nil
  "Reference to the frame used during DAP debugging.")

(defun my/new-frame-on-second-monitor ()
  "Create a new frame named 'Debugger Frame' on the second monitor with an empty buffer and return frame."
  (interactive)
  (let* ((display-geometry (display-monitor-attributes-list))
         (second-monitor (nth 1 display-geometry)) ; 1 = second monitor
         (geometry (alist-get 'geometry second-monitor))
         (x (nth 0 geometry))
         (y (nth 1 geometry))
         (frame (make-frame `((name . "Debugger Frame")
                              (left . ,x)
                              (top . ,y)
                              (width . 1600)
                              (height . 900)))))
    (select-frame-set-input-focus frame)
    (with-current-buffer (generate-new-buffer "*dummy*")
      (switch-to-buffer (current-buffer)))
    (toggle-frame-maximized frame)
    frame)) ;; return the frame object


(defun my/dap-debugger-setting ()
  "Run DAP UI from the MAIN frame; panels show in Debugger Frame via your alist."
  (interactive)
  (setq my/main-frame-name (frame-parameter (selected-frame) 'name))
  (let* ((orig (selected-frame))
         (dbg  (or (car (filtered-frame-list
                         (lambda (f)
                           (string= (frame-parameter f 'name) "Debugger Frame"))))
                   (my/new-frame-on-second-monitor))))

    (when (frame-live-p dbg)
      (setq my/dap-debug-frame dbg))

    ;; Open ui
    (when (frame-live-p my/dap-debug-frame)
      (select-frame-by-name my/dap-debug-frame-name))
    (split-window-horizontally)

    ;; (select-frame-by-name my/main-frame-name)
    ;; (dap-ui-locals)

    ;; (select-frame-by-name my/main-frame-name)
    ;; (dap-ui-breakpoints)

    ;; (when (frame-live-p orig)
    ;;   (select-frame-by-name my/main-frame-name))
    ;; (dap-ui-breakpoints) ;; ... weiter erweiterbar

    ;; Clean dummy only in Debugger Frame
    ;; (when (frame-live-p dbg)
    ;;   (with-selected-frame dbg
    ;;     (delete-windows-on "*dummy*")))

    ;; Select MAIN
    (select-frame-by-name my/main-frame-name)))

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
      (when (and name (string-prefix-p "*GDB" name))
        (kill-buffer buf))))

  ;; Delete LLDB buffers
  (dolist (buf (buffer-list))
    (let ((name (buffer-name buf)))
      (when (and name (string-prefix-p "*LLDB" name))
        (kill-buffer buf))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add to list: display buffer alist                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/dap-ui--show-buffer (buf)
  "Show BUF according to defined rules."
  (when-let (win (display-buffer buf))
    (set-window-dedicated-p win t)
    (select-window win))
)
(advice-add 'dap-ui--show-buffer :override #'my/dap-ui--show-buffer)

(add-to-list 'display-buffer-alist
             '("\\*dap-ui-locals\\*"
               (display-buffer-use-some-frame display-buffer-in-side-window)
               (side . left)
               (slot . 0) ;; position below slot 0
               (window-height . 0.5)
               (inhibit-same-window . t)
               (reusable-frames . "Debugger Frame")))

(add-to-list 'display-buffer-alist
             '("\\*dap-ui-breakpoints\\*"
               (display-buffer-use-some-frame display-buffer-in-side-window)
               (side . right)
               (slot . 0) ;; top position in the side window
               (window-height . 0.5)
               (inhibit-same-window . t)
               (reusable-frames . "Debugger Frame")))

(add-to-list 'display-buffer-alist
             '("\\*dap-ui-sessions\\*"
               (display-buffer-use-some-frame display-buffer-in-side-window)
               (inhibit-same-window . t)
               (reusable-frames . "Debugger Frame")))

(add-to-list 'display-buffer-alist
             '("\\*dap-ui-expressions\\*"
               (display-buffer-use-some-frame display-buffer-in-side-window)
               (inhibit-same-window . t)
               (reusable-frames . "Debugger Frame")))

(add-to-list 'display-buffer-alist
             '("\\*dap-ui-repl\\*"
               (display-buffer-use-some-frame display-buffer-in-side-window)
               (inhibit-same-window . t)
               (reusable-frames . "Debugger Frame")))

(add-to-list 'display-buffer-alist
             '("\\*GDB::Run.*\\*"
               (display-buffer-use-some-frame display-buffer-in-side-window)
               (inhibit-same-window . t)
               (reusable-frames . "Debugger Frame")))


(provide 'init-dap-gdb)
;;; init-dap-gdb.el ends here
