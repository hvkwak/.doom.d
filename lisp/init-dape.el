;;;; init-dape.el --- Flexible Native lldb-dap config with smart defaults -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;;;

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

(provide 'init-dape)
;;; init-dape.el ends here
