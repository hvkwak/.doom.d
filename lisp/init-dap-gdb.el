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

(defvar my/dap-debug-frame nil
  "Reference to the frame used during DAP debugging.")


(defun my/dap-debugger-setting ()
  "Run DAP UI from the MAIN frame; panels show in Debugger Frame via your alist."
  (interactive)
  (let* ((orig (selected-frame))
         (dbg  (or (car (filtered-frame-list
                         (lambda (f)
                           (string= (frame-parameter f 'name) "Debugger Frame"))))
                   (my/new-frame-on-second-monitor))))
    (when (frame-live-p dbg)
      (set-frame-parameter dbg 'my-dap-frame t)
      (setq my/dap-debug-frame dbg))

    ;; Now we have two frames!
    ;; From where we call dap-ui matters here.
    ;; main frame: orig
    ;; second frame: dbg

    ;; Open ui locals from orig
    ;; Let it split in dbg
    (when (frame-live-p dbg)
      (select-frame-set-input-focus dbg))
    (split-window-horizontally)

    ;; Open from MAIN;
    (when (frame-live-p orig)
      (select-frame-set-input-focus orig))
    (dap-ui-locals)

    ;; Open from MAIN;
    (when (frame-live-p orig)
      (select-frame-set-input-focus orig))
    (dap-ui-breakpoints)

    ;; ... weiter erweiterbar

    ;; Clean dummy only in Debugger Frame
    ;; (when (frame-live-p dbg)
    ;;   (with-selected-frame dbg
    ;;     (delete-windows-on "*dummy*")))

    ;; Select MAIN
    (when (frame-live-p orig)
      (select-frame-set-input-focus orig))))



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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add to list: display buffer alist                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
