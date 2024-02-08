;;; .emacs --- summary -*- lexical-binding: t -*-

;;; Commentary:
;;  This is the standard Emacs configuration file.  It uses the Emacs Lisp
;;  dialect of the Lisp programming language for its specification.  Note that
;;  the majority of the configuration is in ~/.emacs.d/my-init.org, since ORG
;;  markup is far better for layout and organization than standard Elisp.  This
;;  file loads that ORG file at the very end, once it completes all setup
;;  operations.

;;; Code:

;; Wrapping the entire file in this setting speeds up initial load time by
;; instructing Emacs not to go through the trouble of running each filename
;; loaded against special regexps.
(let ((file-name-handler-alist nil))
  ;; Show diagnostic output in the event of an error if non-nil.  Blowing up the
  ;; screen with debugging output seems to mess with evil-mode, so I find it
  ;; prudent to turn this variable off until I have a reason to turn it on.
  (setq debug-on-error nil)

  ;; Tip from Doom Emacs:
  ;; Set these variables here to speed up our initial load.
  ;; They will be reset later on to further optimize our experience.
  (defvar last-file-name-handler-alist file-name-handler-alist
    "Name of the `file-name-handler-alist' that was set to nil during startup.")
  ;; Set the garbage collection threshold super high for startup. We'll need to
  ;; reset these values later so as not to impede our computer's performance, but
  ;; it does speed up Emacs' start time.
  (setq gc-cons-threshold most-positive-fixnum)
  (setq gc-cons-percentage 0.6)
  (setq file-name-handler-alist nil)

  (defvar my/force-refresh-super-config nil
    "Whether to force reconstruction of the super config Elisp file.")

  ;; Use latest org-mode installed via `straight.el' from the beginning to avoid
  ;; Org version mismatches.
  (add-to-list
    'load-path (file-truename (concat user-emacs-directory "straight/build/org")))

  ;; JIT settings to speed up startup.
  ;; https://tychoish.com/post/towards-faster-emacs-start-times/
  ;; (setq jit-lock-stealth-time nil
  ;;   jit-lock-defer-time nil
  ;;   jit-lock-defer-time 0.05
  ;;   jit-lock-stealth-load 200)

;;; NOTE REGARDING BYTE COMPILING CONFIG FILES:
  ;; In general, I have observed that attempting to load compiled Elisp files is
  ;; much, much slower than simply interpreting their corresponding .el files at
  ;; run time. I don't know why this is, but I have a guess. I am making prolific
  ;; use of the 'use-package' and 'straight' macros, and historically macros have
  ;; not played well with compiled Lisp code. Whatever the case may be, just bear
  ;; in mind that seeking a performance improvement from compiled files is likely
  ;; a fool's errand.
  ;;
  ;; In addition, the process of compiling all of these macro-heavy Elisp files is
  ;; extremely difficult to get right. Many things are left up to chance, and
  ;; things we often take for granted in the Elisp world cease to function.

  ;; Add required libraries
  (eval-when-compile
    (require 'bytecomp)
    (require 'cl-lib))

  ;; Settings for Emacs Lisp embedded in Org source blocks.
  (with-eval-after-load "ob-emacs-lisp"
    ;; Globally set lexical bindings for all Emacs Lisp code blocks in Org files.
    (setq org-babel-default-header-args:emacs-lisp
      '((:lexical . t)
         (:tangle . "yes"))))

  (defsubst my/apply-to-dir-files (dir pattern fn &rest args)
    "Apply FN to all files in DIR matching regex PATTERN.

Any additional args ARGS are passed to FN."
    (cl-flet
      ((apply-it (f) (funcall fn (concat (file-name-as-directory dir) f) args)))
      (if (file-directory-p dir)
        (mapc #'apply-it (directory-files dir nil pattern)))))

  (defun my/recreate-super-config ()
    "Re-create the Emacs super configuration file.

Once created, the file should be placed at
`~/.emacs.d/tjtrabue-emacs.el'."
    (interactive)
    (let* ((dotfiles-home-dir (file-truename (concat (getenv "HOME") "/.dotfiles")))
            (dotfiles-bin-dir (file-truename (concat dotfiles-home-dir "/bin")))
            (super-config-script (file-truename (concat dotfiles-bin-dir "/make_emacs_super_config"))))
      (compile super-config-script)))

;;; Configure the `load-path'.
  ;; Add `/usr/local/share/emacs/site-lisp/' to load-path, and then add all of its
  ;; child directories recursively to load-path
  (let ((usr-local-lisp "/usr/local/share/emacs/site-lisp"))
    (when (file-directory-p usr-local-lisp)
      (let ((default-directory usr-local-lisp))
        (add-to-list 'load-path usr-local-lisp)
        (normal-top-level-add-subdirs-to-load-path))))
  ;; This sets additional paths where Emacs looks for Elisp files when a load
  ;; command is issued.
  (dolist (dir (list (file-truename (concat user-emacs-directory "plugin"))
                 (file-truename (concat user-emacs-directory "private"))
                 (file-truename (concat user-emacs-directory "require"))
                 (file-truename (concat user-emacs-directory "require/fix"))))
    (add-to-list 'load-path dir))

  (let* ((super-config-dir (file-truename (concat user-emacs-directory "super_config")))
          (default-directory user-emacs-directory))
    (if (or my/force-refresh-super-config
          (not (file-directory-p super-config-dir)))
      (progn
        ;; Create the super config if it is not yet present.
        (my/recreate-super-config)
        ;; Force compilation of the super config dir after it's first created.
        (byte-recompile-directory super-config-dir 0 t))
      ;; OTHERWISE only recompile files that need recompiling.
      (byte-recompile-directory super-config-dir))
    ;; TODO: Load the byte-compiled version of the Elisp files once we fix them.
    (my/apply-to-dir-files super-config-dir "\\.el$" #'load)))
;;; .emacs ends here
