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

(after! projectile
  (defun my/projectile-root-git-super (dir)
    "If DIR is inside a Git submodule, return the superproject's working tree.
Return nil otherwise so other root functions can try."
    (when (and (executable-find "git")
               (locate-dominating-file dir ".git"))
      (let* ((default-directory dir)
             (out (with-temp-buffer
                    (when (zerop (call-process "git" nil t nil
                                               "rev-parse" "--show-superproject-working-tree"))
                      (string-trim (buffer-string))))))
        (when (and out (not (string-empty-p out)))
          (expand-file-name out)))))

  ;; Prepend our detector so it runs before the default ones.
  (setq projectile-project-root-functions
        (cons #'my/projectile-root-git-super projectile-project-root-functions)))

;; (defun my/projectile-canonical-path (path)
;;   "Return the fully expanded and true path for PATH."
;;   (directory-file-name (file-truename (expand-file-name path))))

;; (defun my/projectile-remove-duplicate-projects ()
;;   "Remove duplicate projects from `projectile-known-projects`."
;;   (setq projectile-known-projects
;;         (delete-dups
;;          (mapcar #'my/projectile-canonical-path projectile-known-projects))))

;; ;; Hook into projectile's save/load
;; (advice-add #'projectile-add-known-project :filter-args
;;             (lambda (args)
;;               (let ((path (car args)))
;;                 (list (my/projectile-canonical-path path)))))
;; (add-hook 'projectile-after-switch-project-hook #'my/projectile-remove-duplicate-projects)

(provide 'init-projectile)
;;; init-projectile.el ends here
