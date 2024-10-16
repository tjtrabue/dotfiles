;;; .emacs --- summary -*- lexical-binding: t -*-

;;; Commentary:
;;  This is the standard Emacs configuration file.  It uses the Emacs Lisp
;;  dialect of the Lisp programming language for its specification.  Note that
;;  the majority of the configuration is in ~/.emacs.d/my-init.org, since ORG
;;  markup is far better for layout and organization than standard Elisp.  This
;;  file loads that ORG file at the very end, once it completes all setup
;;  operations.

;;; Code:

;; Show diagnostic output in the event of an error if non-nil.  Blowing up the
;; screen with debugging output seems to mess with evil-mode, so I find it
;; prudent to turn this variable off until I have a reason to turn it on.
(setq debug-on-error nil)

;; Instruct Emacs not to go through the trouble of running each filename loaded
;; against special regexps.
(setq file-name-handler-alist nil)

;; Determines how to load .el/.elc files using `load'.  If non-nil, always load
;; the newer file, regardless of extension. If nil, load based on the predefined
;; load order.
(setq load-prefer-newer t)

;; Define the level of native compilation optimization.
;; Allowed values: -1, 0, 1, 2, 3.
(setq native-comp-speed 3)

;; The number of parallel async jobs to perform.
;; Defaults to 0, meaning use half the available CPU cores.
(setq native-comp-async-jobs-number 0)

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

(defvar my/use-compiled-config nil
  "Whether to use the compiled version of tangled config files.

  Use at your own risk! I have found it not at all worthwhile to compile
  my configuration.  Most of the performance increase comes from
  native-compiling Emacs itself, along with any downloaded libraries.")

(defvar my/gc-cons-threshold (* 100 1024 1024) ;; 100 MB
  "My chosen threshold for garbage collection.")

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
  (require 'cl-lib)
  (require 'vc-git))

;; Autoload certain functions required by early config.
(autoload 'vc-git-root "vc-git")

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

(defun my/create-super-config (arg)
  "Generate the super config file(s).

Once created, the file should be placed in
`~/.emacs.d/super_config/'.

If called with a prefix ARG, run in verbose mode."
  (interactive "P")
  (let* ((dotfiles-home-dir (file-truename (concat (getenv "HOME") "/.dotfiles")))
          (dotfiles-bin-dir (file-truename (concat dotfiles-home-dir "/bin")))
          (super-config-script (file-truename (concat dotfiles-bin-dir "/make_emacs_super_config"))))
    (compile (if arg
               super-config-script
               (concat super-config-script " -q")))))

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

;; I refactored my `straight.el' bootstrap code to a separate file.
(load "my-straight-bootstrap")

;; Make downloaded straight packages available on `load-path'.
(dolist (dir (directory-files (file-truename (concat user-emacs-directory "straight/build"))
               'full-name))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))

  ;;; Load the super config:
(let* ((super-config-dir (file-truename (concat user-emacs-directory "super_config")))
        (default-directory user-emacs-directory))
  (if (or my/force-refresh-super-config
        (not (file-directory-p super-config-dir)))
    (progn
      ;; Create the super config if it is not yet present.
      (my/create-super-config)
      ;; Force compilation of the super config dir after it's first created.
      (byte-recompile-directory super-config-dir 0 t))
    ;; OTHERWISE only recompile files that need recompiling.
    (byte-recompile-directory super-config-dir))
  (if (and my/use-compiled-config
        (directory-files super-config-dir nil "\\.elc$"))
    ;; Load compiled Elisp files if availble.
    (my/apply-to-dir-files super-config-dir "\\.el$" (lambda (file &rest args)
                                                      (load (file-name-sans-extension file) args)))
    ;; (my/apply-to-dir-files super-config-dir "\\.elc$" #'load)
    ;; Otherwise, load the Elisp source files.
    (my/apply-to-dir-files super-config-dir "\\.el$" #'load))
  ;; After startup, it is important you reset the garbage collector
  ;; settings to some reasonable defaults. A large gc-cons-threshold
  ;; will cause freezing and stuttering during long-term interactive
  ;; use. I find these are nice defaults:
  (setq gc-cons-threshold my/gc-cons-threshold)
  (setq gc-cons-percentage 0.1)
  (setq file-name-handler-alist last-file-name-handler-alist))
;;; .emacs ends here
