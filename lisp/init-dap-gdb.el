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
         :cwd nil)))


(provide 'init-dap-gdb)
;;; init-dap-gdb.el ends here
