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

;; Tip from Doom Emacs:
;; Set these variables here to speed up our initial load.
;; They will be reset later on to further optimize our experience.
(defvar last-file-name-handler-alist file-name-handler-alist
  "Name of the `file-name-handler-alist' that was set to nil during startup.")
;; Set the garbage collection threshold super high for startup. We'll need to
;; reset these values later so as not to impede our computer's performance, but
;; it does speed up Emacs' start time.
(setq gc-cons-threshold 419430400) ;; 400 MB.
(setq gc-cons-percentage 0.6)
(setq file-name-handler-alist nil)

;; Use latest org-mode installed via `straight.el' from the beginning to avoid
;; Org version mismatches.
(add-to-list
  'load-path (file-truename (concat user-emacs-directory "straight/build/org")))

;; JIT settings to speed up startup.
;; https://tychoish.com/post/towards-faster-emacs-start-times/
;; (setq jit-lock-stealth-time nil)
;; (setq jit-lock-defer-time nil)
;; (setq jit-lock-defer-time 0.05)
;; (setq jit-lock-stealth-load 200)

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
;; things we often take for granted in the Elisp world cease to function

;; Add required libraries
(require 'bytecomp)

;; Settings for Emacs Lisp embedded in Org source blocks.
(with-eval-after-load "ob-emacs-lisp"
  ;; Globally set lexical bindings for all Emacs Lisp code blocks in Org files.
  (setq org-babel-default-header-args:emacs-lisp
    '((:lexical . t)
       (:tangle . "yes"))))

;; Define and set variables
(eval-when-compile
  (defconst my/home-dir (getenv "HOME")
    "User's home directory path.")
  (defconst my/emacsrc (file-truename (concat (getenv "HOME") "/.emacs"))
    "The main Emacs config file in the user's home directory.")
  (defconst my/use-straight t
    "Whether to use straight.el instead of Emacs' built-in package manager."))

(defsubst my/apply-to-dir-files (dir pattern fn &rest args)
  "Apply FN to all files in DIR matching regex PATTERN.

Any additional args ARGS are passed to FN."
  (require 'cl-lib)
  (cl-flet
      ((apply-it (f) (funcall fn (concat (file-name-as-directory dir) f) args)))
    (if (file-directory-p dir)
        (mapc #'apply-it (directory-files dir nil pattern)))))

;; Load extra packages using Emacs 24's package system.
;; NOTE: Currently disabled since we use straight.el to manage our packages.
(if (and (not my/use-straight) (>= emacs-major-version 24))
;;; IF we want to use the built-in package manager...
  (progn
    ;; Package configuration
    (require 'package)
    ;; Add extra package archives to the list of repositories.
    ;; NOTE: HTTPS may be unsupported on Emacs versions < 27. You may need
    ;;       to change the URLs to simple HTTP in order for them to function.
    ;;       If you must do this, also uncomment the two expressions below.
    ;;       That will reset the archives list and allow you to only use
    ;;       unsecured connections for package transfer.
    ;; (setq package-archives nil)
    ;; (add-to-list 'package-archives
    ;;   '("gnu" . "http://elpa.gnu.org/packages/") t)
    (add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/") t)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
    (package-initialize)
    ;; Automatically install packages using use-package
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    (require 'use-package))
;;; OTHERWISE...
  ;; Do not auto-initialize packages! This can slow down Emacs's startup time.
  (setq package-enable-at-startup nil)
  ;; this tells package.el not to add those pesky customized variable settings
  ;; at the end of your init.el
  (setq package--init-file-ensured t))

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
               (file-truename (concat user-emacs-directory "require"))))
  (add-to-list 'load-path dir))

(let* ((super-config (file-truename (concat user-emacs-directory "tjtrabue-emacs.el")))
        (dotfiles-home-dir (file-truename (concat (getenv "HOME") "/.dotfiles")))
        (dotfiles-bin-dir (file-truename (concat dotfiles-home-dir "/bin"))))
  (unless (file-exists-p super-config)
    ;; If we do not find the giant configuration Elisp file, generate it here.
    (compile (file-truename (concat dotfiles-bin-dir "/make_emacs_super_config"))))
  (let ((default-directory user-emacs-directory))
    ;; Load the giant Elisp file!
    (load-file super-config)))

;;; .emacs ends here
