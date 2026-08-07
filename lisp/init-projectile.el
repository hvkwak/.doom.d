;;; init-projectile.el --- Change Projectile settings -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(after! projectile ;; Set the compile command for CMake projects
  ;;(setq projectile-indexing-method 'native)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-generic-command "fd -t f -0 -H")
  )


(provide 'init-projectile)
;;; init-projectile.el ends here
