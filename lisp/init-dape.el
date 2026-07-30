;;; init-dape.el --- Flexible Native lldb-dap config with smart defaults -*- lexical-binding: t; no-byte-compile: t; -*-

(defun +dape-live-connection-p ()
  "Check if dape is loaded and has active connections safely."
  (and (featurep 'dape)
       (cond
        ((fboundp 'dape-active-p) (dape-active-p))
        ((fboundp 'dape--live-connections) (dape--live-connections))
        ((boundp 'dape--connections) dape--connections)
        (t nil))))

(defun +dape-update-evil-state ()
  "Enable `dape-evil-mode` when a session or Dape UI buffers are active."
  (if (or (+dape-live-connection-p) (+dape-has-open-buffers-p))
      (unless dape-evil-mode
        (when dape-key-prefix
          (global-set-key dape-key-prefix dape-global-map))
        (dape-evil-mode 1))
    (when dape-evil-mode
      (when dape-key-prefix
        (global-set-key dape-key-prefix #'dape))
      (dape-evil-mode -1))))

  ;; Run the check whenever Dape starts, stops, disconnects, or closes windows
  (add-hook 'dape-on-start-hooks #'+dape-update-evil-state)
  (add-hook 'dape-on-stopped-hooks #'+dape-update-evil-state)
  (add-hook 'dape-on-disconnect-hooks #'+dape-update-evil-state)
  ;; (add-hook 'window-configuration-change-hook #'+dape-update-evil-state)

(dolist (fn '(dape-breakpoint-toggle dape-pause dape-continue dape-next
              dape-step-in dape-step-out dape-restart dape-info dape-repl
              dape-memory dape-disassemble dape-breakpoint-log
              dape-breakpoint-expression dape-breakpoint-hits
              dape-breakpoint-remove-all dape-select-thread dape-select-stack
              dape-stack-select-down dape-stack-select-up
              dape-evaluate-expression dape-watch-dwim
              dape-disconnect-quit dape-quit))
  (autoload fn "dape" nil t))

(with-eval-after-load 'dape
  (setq dape-configs nil) ; Reset previous config cache
  
  (add-to-list 'dape-configs
                `(native-lldb
                  modes (c-mode c++-mode rust-mode)
                  command "lldb-dap"
                  :type "lldb"
                  :request "launch"
                  
                  :program ,(lambda ()
                              (let* ((proj (project-current))
                                     (proj-root (if proj (project-root proj) default-directory))
                                     (build-dir (expand-file-name "build/" proj-root))
                                     (default-bin-path nil))
                                
                                (when (file-directory-p build-dir)
                                  (let ((files (directory-files-and-attributes build-dir t nil t)))
                                    (setq files (cl-remove-if-not
                                                 (lambda (file)
                                                   (let ((modes (nth 9 file)))
                                                     (and (null (nth 1 file))
                                                          (string-match-p "x" (or modes "")))))
                                                 files))
                                    (setq files (cl-sort files (lambda (a b) (time-less-p (nth 6 b) (nth 6 a)))))
                                    (when files
                                      (setq default-bin-path (car (car files))))))
                                
                                (unless default-bin-path
                                  (setq default-bin-path build-dir))
                                
                                (expand-file-name (read-file-name "Select binary: " build-dir default-bin-path t))))
                  
                  :cwd ,(lambda ()
                          (let ((proj (project-current)))
                            (expand-file-name (if proj (project-root proj) default-directory))))
                  
                  :args [])))

(after! dape
  ;; 1. Prefix Management
  (when dape-key-prefix
    (global-set-key dape-key-prefix #'dape))

  ;; 2. Define a dedicated Evil Keymap for Dape active sessions
  (defvar dape-evil-mode-map (make-sparse-keymap)
    "Keymap used during an active `dape` debug session.")

  (evil-define-minor-mode-key 'motion 'dape-evil-mode
    "n" #'dape-next
    "b" #'dape-breakpoint-toggle)

  (evil-define-minor-mode-key 'normal 'dape-evil-mode
    "c" #'dape-continue
    "s" #'dape-step-in
    "o" #'dape-step-out
    "r" #'dape-restart
    "q" #'dape-quit
    "e" #'dape-evaluate-expression)

  ;; Minor mode toggle
  (define-minor-mode dape-evil-mode
    "Minor mode enabling Evil single-key bindings during Dape debugging."
    :global t
    :keymap dape-evil-mode-map)

  ;; 3. Control binding activation based on actual connection presence
  (defun +dape-update-evil-state ()
    "Turn on `dape-evil-mode` when a connection exists, turn off when sessions end."
    (if (dape--live-connections)
        (unless dape-evil-mode
          (when dape-key-prefix
            (global-set-key dape-key-prefix dape-global-map))
          (dape-evil-mode 1))
      (when dape-evil-mode
        (when dape-key-prefix
          (global-set-key dape-key-prefix #'dape))
        (dape-evil-mode -1))))

  ;; Attach updates to session lifecycle hooks rather than paused-state hooks
  (add-hook 'dape-on-start-hooks #'+dape-update-evil-state)
  (add-hook 'dape-on-stopped-hooks #'+dape-update-evil-state)
  (add-hook 'dape-on-disconnect-hooks #'+dape-update-evil-state)

  ;; 4. Custom faces
  (custom-set-faces!
    '(dape-source-line-face :background "#a8e6a3" :foreground "black" :extend t)))

(provide 'init-dape)
;;; init-dape.el ends here
