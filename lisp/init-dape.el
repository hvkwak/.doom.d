;;;; init-dape.el --- my dape config -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;;;

(setq dap-cpptools-extension-path "~/.emacs.d/debug-adapters/cpptools")

;; Path to the binary YOU built today
(setq dap-cpptools-lldb-mi-path "~/lldb-mi/build/src/lldb-mi")

(with-eval-after-load 'dape
  (add-to-list 'dape-configs
               `(cpptools
                 modes (c-mode c++-mode rust-mode)
                 ;; This points to the VS Code extension binary we unpacked earlier
                 command ,(expand-file-name "~/.emacs.d/debug-adapters/cpptools/extension/debugAdapters/bin/OpenDebugAD7")
                 :type "cppdbg"
                 :request "launch"
                 :state "launch"
                 :MIMode "lldb"
                 ;; This points to the lldb-mi binary YOU built!
                 :miDebuggerPath ,(expand-file-name "~/lldb-mi/build/src/lldb-mi")
                 ;; AUTOMATION: This finds the executable in your build folder automatically
                 :program ,(lambda ()
                             (let ((debug-file (concat (dape-cwd) "build/" (file-name-all-completions "" (concat (dape-cwd) "build/")))))
                               ;; If 'helloworld' exists in build/, use it, otherwise ask
                               (read-file-name "Select binary: " (concat (dape-cwd) "build/"))))
                 :cwd ,(lambda () (dape-cwd))
                 :environment [])))

(provide 'init-dape)
;;; init-dape.el ends here
