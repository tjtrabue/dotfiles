(let* ((usr-local-lisp "/usr/local/share/emacs/site-lisp")
        (straight-dir (file-truename (concat user-emacs-directory "straight")))
        (straight-build-dir (file-truename (concat straight-dir "/build")))
        (super-config-dir (file-truename (concat user-emacs-directory "super_config"))))
  ;; Add `/usr/local/share/emacs/site-lisp/' to load-path, and then add all of its
  ;; child directories recursively to load-path
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
  ;; Bootstrap and load `straight.el'
  (load "my-straight-bootstrap")
  ;; Add all subdirectories of `~/.emacs.d/straight/build' to `load-path'.
  (dolist (dir (directory-files straight-build-dir 'full-name))
    (add-to-list 'load-path dir))

  ;; Make macros available at compile-time.
  (require 'bytecomp)
  (require 'comp-run)
  (require 'use-package)
  (require 'straight)
  (require 'general)

  ;; (native-compile-async (list super-config-dir) 'recursively)
  (byte-recompile-directory super-config-dir 0))
