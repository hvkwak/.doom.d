;;; init-dap-lldb.el --- DAP Mode-LLDB Configurations -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(after! dap-mode
  (require 'dap-lldb)

  ;;; set the debugger executable (c++)
  (setq dap-lldb-debug-program '("~/LLVM-20.1.8-Linux-X64/bin/lldb-dap")) ;; python3.10 with --shared enabled.

  ;;; ask user for executable to debug if not specified explicitly (c++)
  (setq dap-lldb-debugged-program-function (lambda () (read-file-name "Select file to debug.")))

  ;;; default debug template for (c++)
  (dap-register-debug-template
   "LLDB::Run "
   (list :type "lldb-vscode"
         :request "launch"
         :name "LLDB::Run"
         :program "${workspaceFolder}/build/"
         :cwd "${workspaceFolder}"
         :args ["--scene" "cornell.json" "--threads" "8"]))
  )


(provide 'init-dap-lldb)
;;; init-dap-lldb.el ends here
