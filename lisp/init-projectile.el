;;; init-projectile.el --- Change Projectile settings -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(after! projectile ;; Set the compile command for CMake projects
  (setq projectile-indexing-method 'native)
  (setq projectile-enable-caching nil)
  (setq projectile-project-compilation-cmd "cmake -S . -B build && cmake --build build")
  (setq projectile-project-run-cmd "./build/main")
  (setq compile-command "rm -r build && mkdir build && cmake -S . -B build && cmake --build build")
  )


(provide 'init-projectile)
;;; init-projectile.el ends here
