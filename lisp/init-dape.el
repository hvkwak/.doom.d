;;;; init-dape.el --- Flexible Native lldb-dap config with smart defaults -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;;;

;; corrects "Wrong type argument: commandp, dape-breakpoint-toggle"
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
                 
                 ;; 💡 1. [Smart Auto-fill] Finds the newest executable inside the build directory
                 :program ,(lambda ()
                             (let* ((proj (project-current))
                                    (proj-root (if proj (project-root proj) default-directory))
                                    (build-dir (expand-file-name "build/" proj-root))
                                    (default-bin-path nil))
                               
                               ;; If build directory exists, find the most recently modified executable file
                               (when (file-directory-p build-dir)
                                 (let ((files (directory-files-and-attributes build-dir t nil t)))
                                   ;; Filter files that are regular files, executable, and not directories
                                   (setq files (cl-remove-if-not
                                                (lambda (file)
                                                  (let ((modes (nth 9 file)))
                                                    (and (null (nth 1 file)) ; Not a directory
                                                         (string-match-p "x" (or modes ""))))) ; Executable
                                                files))
                                   ;; Sort by modification time (newest first)
                                   (setq files (cl-sort files (lambda (a b) (time-less-p (nth 6 b) (nth 6 a)))))
                                   (when files
                                     (setq default-bin-path (car (car files))))))
                               
                               ;; Fallback to build directory path if no executable is found yet
                               (unless default-bin-path
                                 (setq default-bin-path build-dir))
                               
                               ;; Pre-fill the minibuffer with the smartest guess
                               (expand-file-name (read-file-name "Select binary: " build-dir default-bin-path t))))
                 
                 ;; 💡 2. [Auto-fill] Set the current project root as cwd automatically
                 :cwd ,(lambda ()
                         (let ((proj (project-current)))
                           (expand-file-name (if proj (project-root proj) default-directory))))
                 
                 :args [])))

(after! dape
  ;; Evil keybindings while debugging is active
  (remove-hook 'dape-stopped-hook #'dape-active-mode)

  ;; `n'/`b' live in `evil-motion-state-map' upstream, so a local motion-state
  ;; override is enough.  `c'/`s'/`o'/`r'/`q'/`e' are upstream bound in
  ;; `evil-normal-state-map' instead, which Evil checks *before* the
  ;; motion-state maps -- a motion-only override would be shadowed by
  ;; `evil-change'/`evil-substitute'/`evil-open-below'/`evil-replace'/
  ;; `evil-record-macro', so those need a local `normal' state override to
  ;; actually win.  Named for the same re-eval-safety reason as above.
  (defun +dape-active-mode-h ()
    (if dape-active-mode
        (progn
          (evil-local-set-key 'motion "n" #'dape-next)
          (evil-local-set-key 'motion "b" #'dape-breakpoint-toggle)
          (evil-local-set-key 'normal "c" #'dape-continue)
          (evil-local-set-key 'normal "s" #'dape-step-in)
          (evil-local-set-key 'normal "o" #'dape-step-out)
          (evil-local-set-key 'normal "r" #'dape-restart)
          (evil-local-set-key 'normal "q" #'dape-quit)
          (evil-local-set-key 'normal "e" #'dape-evaluate-expression))

      ;; `dape-restart' briefly flips `dape-active-mode' off then back on
      ;; as an implementation detail (see dape.el, `dape-restart'), even
      ;; though the session isn't actually ending -- it flags the
      ;; connection with `dape--restart-in-progress-p' first, so skip
      ;; cleanup entirely on that blip instead of unbinding keys and
      ;; toggling `hl-line-mode' for no reason mid-restart.
      (unless (cl-some #'dape--restart-in-progress-p dape--connections)
        (dape--stack-frame-cleanup)
        (evil-local-set-key 'motion "n" nil)
        (evil-local-set-key 'motion "b" nil)
        (evil-local-set-key 'normal "c" nil)
        (evil-local-set-key 'normal "s" nil)
        (evil-local-set-key 'normal "o" nil)
        (evil-local-set-key 'normal "r" nil)
        (evil-local-set-key 'normal "q" nil)
        (evil-local-set-key 'normal "i" nil))
      )
  )
  (remove-hook 'dape-active-mode-hook #'+dape-active-mode-h)
  (add-hook 'dape-active-mode-hook #'+dape-active-mode-h)

  ;; Customize current execution line highlight (light green background)
  (custom-set-faces!
    '(dape-source-line-face :background "#a8e6a3" :foreground "black" :extend t)))

(provide 'init-dape)
;;; init-dape.el ends here
