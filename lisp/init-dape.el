;;;; init-dape.el --- my dape config -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;;;

(after! gdb-mi
  (setq gdb-many-windows t)
  (setq gdb-show-main t))

(after! dape
  ;; RESET dape-configs to ensure no old broken configs survive
  (setq dape-configs `(
   ;; (helloworld modes (c-mode c++-mode)
   ;;             command-cwd "/home/hyobin/Documents/projects/helloworld"
   ;;             :program "/home/hyobin/Documents/projects/helloworld/build/helloworld"
   ;;             :args ""
   ;;             :stopAtBeginningOfMainSubprogram nil
   ;;      )
   (helloworld modes (c-mode c++-mode)
               ;; how to start the adapter (gdb)
               command-cwd dape-command-cwd
               command "gdb"
               command-args ("--interpreter=dap")
               :request "launch"
               :type "gdb"
               :program "/home/hyobin/Documents/projects/helloworld/build/helloworld"
               :cwd "/home/hyobin/Documents/projects/helloworld"
               :args []
               ;;:stopAtBeginningOfMainSubprogram t
               :defer-launch-attach nil
               )
   (raytracer modes (c-mode c++-mode)
              command-cwd "~/"
              command "ssh"
              command-args ("root@141.195.21.87" "-p" "40418" "gdb" "--interpreter=dap")
              :cwd "/workspace/gdv-raytracer/build/"
              :program "/workspace/gdv-raytracer/build/raytracer"
              :prefix-local "/ssh:root@141.195.21.87#40418:/"
              :prefix-remote "/"
              )
   ))
  )

(provide 'init-dape)
;;; init-dape.el ends here
