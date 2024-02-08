;; Add `/usr/local/share/emacs/site-lisp/' to load-path, and then add all of its
;; child directories recursively to load-path

;;; Load straight.el
(defvar bootstrap-version)
;; Always use `use-package' when installing packages, making the `:straight t'
;; part of the `use-package' macro unnecessary.
(setq straight-use-package-by-default t)
;; The straight.el branch to clone.
(setq straight-repository-branch "develop")
(let ((bootstrap-file
        (file-truename (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el")))
       (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        (concat
          "https://raw.githubusercontent.com/radian-software/straight.el/"
          straight-repository-branch
          "/install.el")
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (with-no-warnings
    (load bootstrap-file nil 'nomessage))

  ;; Load all of straight's macros at compile-time.
  (add-to-list 'load-path (file-truename (concat user-emacs-directory "straight/build/straight")))

  ;; Register more Git project hosting sites with Straight.el.
  ;; TODO: Remove these host additions once straight.el includes them by
  ;;       default.
  (add-to-list 'straight-hosts '(codeberg "codeberg.org" ".git"))

  ;; Refresh package repositories
  (when (not (fboundp 'straight-pull-recipe-repositories))
    ;; Sometimes straight.el does not include the convenience function
    ;; `straight-pull-recipe-repositories', in which case we should alias
    ;; that function to our own custom version.
    (defalias 'straight-pull-recipe-repositories
      'my-straight-helpers-pull-recipe-repositories))
  (straight-pull-recipe-repositories)

  ;; Default mode for loading packages: either defer or demand.
  ;; (setq use-package-always-demand t)
  (setq use-package-always-defer t)

  ;; Install use-package via straight. After this function runs, use-package
  ;; will automatically use straight to install packages if you specify
  ;; ':stright t' instead of ':ensure t'.  If you have set
  ;; straight-use-package-by-default to t, this is unnecessary.
  (straight-use-package
    ;; Override the MELPA recipe in order to get all Elisp files for
    ;; use-package. For some reason, the MELPA recipe excludes several
    ;; important source files.
    '(use-package :type git :host github :repo "jwiegley/use-package"
       :files (:defaults))))

(let* ((usr-local-lisp "/usr/local/share/emacs/site-lisp")
        (straight-dir (file-truename (concat user-emacs-directory "straight")))
        (straight-build-dir (file-truename (concat straight-dir "/build")))
        (super-config-dir (file-truename (concat user-emacs-directory "super_config"))))
  (when (file-directory-p usr-local-lisp)
    (let ((default-directory usr-local-lisp))
      (add-to-list 'load-path usr-local-lisp)
      (normal-top-level-add-subdirs-to-load-path)))
  ;; This sets additional paths where Emacs looks for Elisp files when a load
  ;; command is issued.
  (dolist (dir (list (file-truename (concat user-emacs-directory "plugin"))
                 (file-truename (concat user-emacs-directory "private"))
                 (file-truename (concat user-emacs-directory "require"))
                 (file-truename (concat user-emacs-directory "require/fix"))))
    (add-to-list 'load-path dir))
  ;; Add all subdirectories of `~/.emacs.d/straight/build' to `load-path'.
  (dolist (dir (directory-files straight-build-dir 'full-name))
    (add-to-list 'load-path dir))

  ;; Make macros available at compile-time.
  (require 'bytecomp)
  (require 'use-package)
  (require 'straight)
  (require 'general)

  (byte-recompile-directory super-config-dir 0))
