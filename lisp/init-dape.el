;;; init-dape.el --- Flexible Native lldb-dap config with smart defaults -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'cl-lib)

;; =============================================================================
;; 1. Autoloads & Safety Setup
;; =============================================================================

(dolist (fn '(dape dape-breakpoint-toggle dape-pause dape-continue dape-next
              dape-step-in dape-step-out dape-restart dape-info dape-repl
              dape-memory dape-disassemble dape-breakpoint-log
              dape-breakpoint-expression dape-breakpoint-hits
              dape-breakpoint-remove-all dape-select-thread dape-select-stack
              dape-stack-select-down dape-stack-select-up
              dape-evaluate-expression dape-watch-dwim
              dape-disconnect-quit dape-quit))
  (autoload fn "dape" nil t))

;; Dape가 global-map을 오염시키지 않도록 prefix 차단
(setq dape-key-prefix nil)

;; =============================================================================
;; 2. Dape Core Configuration
;; =============================================================================

(with-eval-after-load 'dape
  ;; 기존 설정 캐시 초기화
  (setq dape-configs nil)

  ;; Native-lldb 설정 (CMake build/ 디렉터리 내 최신 실행 파일 자동 탐색)
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
                 :args []))

  ;; Custom Highlight Faces
  (custom-set-faces!
    '(dape-source-line-face :background "#a8e6a3" :foreground "black" :extend t)))

(provide 'init-dape)
;;; init-dape.el ends here
