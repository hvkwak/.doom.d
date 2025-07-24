;;; init-projectile.el --- Change Projectile settings -*- lexical-binding: t; -*-

(after! projectile ;; Set the compile command for CMake projects
  (setq projectile-project-compilation-cmd "cmake -S . -B build && cmake --build build")
  (setq projectile-project-run-cmd "./build/main")
  (setq compile-command "rm -r build && mkdir build && cmake -S . -B build && cmake --build build")
  )

(defun my/projectile-canonical-path (path)
  "Return the fully expanded and true path for PATH."
  (directory-file-name (file-truename (expand-file-name path))))

(defun my/projectile-remove-duplicate-projects ()
  "Remove duplicate projects from `projectile-known-projects`."
  (setq projectile-known-projects
        (delete-dups
         (mapcar #'my/projectile-canonical-path projectile-known-projects))))

;; Hook into projectile's save/load
(advice-add #'projectile-add-known-project :filter-args
            (lambda (args)
              (let ((path (car args)))
                (list (my/projectile-canonical-path path)))))
(add-hook 'projectile-after-switch-project-hook #'my/projectile-remove-duplicate-projects)

(provide 'init-projectile)
