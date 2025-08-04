;;; init-dap-lldb.el --- DAP Mode-LLDB Configurations -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
(use-package! dap-mode
  :defer t
  :custom
  (setq dap-auto-configure-features '())

  :config
  (require 'dap-lldb)

  ;;; set the debugger executable (c++)
  (setq dap-lldb-debug-program '("~/LLVM-20.1.8-Linux-X64/bin/lldb-dap")) ;; python3.10 with --shared enabled.

  ;;; ask user for executable to debug if not specified explicitly (c++)
  (setq dap-lldb-debugged-program-function (lambda () (read-file-name "Select file to debug.")))

  ;;; default debug template for (c++)
  (dap-register-debug-template
   "LLDB (VS Code) :: Run Configuration"
   (list :type "lldb-vscode"
         :cwd nil
         :request "launch"
         :program nil
         :name "LLDB::Run")
   )

  ;;; The following template with projecttile-project-root could be useful
  ;; (dap-register-debug-template
  ;;  "LLDB DAP"
  ;;  (list :type "lldb-dap"
  ;;        :cwd (projectile-project-root)
  ;;        :args nil
  ;;        :request "launch"
  ;;        :program (concat (projectile-project-root) "build/main"))
  ;;  )

  )

(defun dap-debug-create-or-edit-json-template ()
    "Manage your debug configurations manually in launch.json files."
    (interactive)
    (let ((filename (concat (lsp-workspace-root) "/launch.json"))
          (default "~/.emacs.d/default-launch.json"))
      (unless (file-exists-p filename)
        (copy-file default filename))
      (find-file-existing filename)))


(provide 'init-dap-lldb)
;;; init-dap-lldb.el ends here
