;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

;;; editor
(package! drag-stuff)

;;; Completion enhancements
(package! consult)
(package! marginalia)
(package! orderless)

;; theme
(package! professional-theme)

;;; Org-mode
(package! toc-org)
(package! org-super-agenda)   ; grouped agenda sections (see lisp/init-org-agenda.el)

;;; Search
(package! rg)

;;; Diff tools
(package! vdiff)
(package! vdiff-magit)

;;; Disabled packages
(package! better-jumper :disable t)         ; Using evil's built-in jump list instead
(package! treemacs-nerd-icons :disable t)   ; Using default treemacs icons
(package! treemacs-all-the-icons :disable t)

;; MELPA renamed its default branch from master to main (Nov 2026); Doom's
;; built-in recipe repo still assumes master. Remove once Doom fixes upstream.
(straight-use-recipes '(melpa :type git :host github :repo "melpa/melpa"
                              :branch "main" :build nil))

;; Doom pins dirvish to 300b6b2, which doesn't exist in latiagertrutis/dirvish,
;; so every sync re-clones it and warns. Pin to that fork's current main instead.
;; Remove once Doom fixes the pin upstream.
(package! dirvish :pin "9b9ed2d707c8f672eac51b067add047118aba247")
