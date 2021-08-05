;;; .emacs --- summary -*- lexical-binding: t -*-

;;; Commentary:
;;  This is the standard Emacs configuration file.  It uses the Emacs Lisp
;;  dialect of the Lisp programming language for its specification.  Note that
;;  the majority of the configuration is in ~/.emacs.d/my-init.org, since ORG
;;  markup is far better for layout and organization than standard Elisp.  This
;;  file loads that ORG file at the very end, once it completes all setup
;;  operations.

;;; Code:

;; Tip from Doom Emacs:
;; Set these variables here to speed up our initial load.
;; They will be reset later on to further optimize our experience.
(defvar last-file-name-handler-alist file-name-handler-alist
  "Name of the `file-name-handler-alist' that was set to nil during startup.")
(setq gc-cons-threshold most-positive-fixnum
  gc-cons-percentage 0.6
  file-name-handler-alist nil)

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
(require 'org)

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
  (defconst my/dotfiles-dir (file-truename (concat my/home-dir "/.dotfiles"))
    "tjtrabue's dotfiles repository directory which houses the primary
  Emacs config.")
  (defconst my/dotfiles-emacs-dir (file-truename (concat my/dotfiles-dir
                                                         "/link/emacs"))
    "Main Emacs directory in tjtrabue's dotfiles repository.")
  (defconst my/emacsrc (file-truename (concat (getenv "HOME") "/.emacs"))
    "The main Emacs config file in the user's home directory.")
  (defconst my/main-emacs-init-org (file-truename
                                    (concat my/dotfiles-emacs-dir
                                            "/my-init.org"))
    "My primary Emacs configuration file in `org-mode' syntax.")
  (defconst my/main-emacs-init-el (car (org-babel-tangle-file
                                        my/main-emacs-init-org))
    "My tangled Emacs configuration file created from `my/main-emacs-init-org'.")
  (defconst my/main-emacs-init-elc (byte-compile-dest-file my/main-emacs-init-el)
    "My compiled Emacs configuration file byte-compiled from
`main-emacs-init-el'."))

;; Define functions to help us get started compiling and loading our other
;; files.
(defsubst my/file-not-exists-or-newer-than-other-p (file other)
  "Return non-nil if FILE does not exist or is newer than OTHER file."
  (or (not (file-exists-p file))
      (equal (nth 4 (file-attributes file)) (list 0 0))
      (file-newer-than-file-p file other)))

(defsubst my/tangle-config-artifact (elisp-file)
  "Tangle the ancestor to an ELISP-FILE.

Return the name of the ELISP-FILE."
  (if (not (string= (file-name-extension elisp-file) "el"))
      (error "Argument is not an Elisp file."))
  (let ((ancestor (concat (file-name-sans-extension elisp-file) ".org")))
    (when (my/file-not-exists-or-newer-than-other-p ancestor elisp-file)
      (org-babel-tangle-file ancestor))
    (identity elisp-file)))

(defsubst my/byte-compile-config-artifact (elc-file)
  "Byte-compile the ancestor to a compiled ELC-FILE.

Return the name of the ELC-FILE."
  (if (not (string= (file-name-extension file) "elc"))
      (error "Argument is not a compiled Elisp file."))
  (let ((ancestor (concat (file-name-sans-extension elc-file) ".el")))
    (when (my/file-not-exists-or-newer-than-other-p ancestor elc-file)
      ;; May need to update the .el (ancestor) file before compiling it.
      (byte-compile-file (my/tangle-config-artifact ancestor)))
    (identity elc-file)))

(defsubst my/create-update-config-artifact (file)
  "Compile or tangle the given FILE if it is newer than its original file.

This function will call `byte-compile-file' on FILE's ancestor if FILE's
extension is '.elc', and will call `org-babel-tangle-file' on the ancestor if
FILE's extension is '.el'."
  (let ((file-ext (file-name-extension file))
        (file-name (file-name-sans-extension file))
        (ancestor-file nil))
    (cond ((string= "elc" file-ext)
           ;; If we're examining a compiled Elisp file, check it against its
           ;; .el ancestor.
           (setq ancestor-file (concat file-name ".el"))
           (my/byte-compile-config-artifact file))
          ((string= "el" file-ext)
           ;; If we're examining a regular Elisp file, check it against its
           ;; .org ancestor.
           (setq ancestor-file (concat file-name ".org"))
           (my/tangle-config-artifact file)))))

(defsubst my/apply-to-dir-files (dir fn pattern)
  "Apply FN to all files in DIR.

Files in the directory are matched based on PATTERN, which is a regex."
  (require 'cl-lib)
  (cl-flet ((apply-it (f)
                      (funcall fn (concat (file-name-as-directory dir) f))))
    (if (file-directory-p dir)
        (mapc #'apply-it (directory-files dir nil pattern)))))

;; Load extra packages using Emacs 24's package system.
(when (>= emacs-major-version 24)
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
  (add-to-list 'package-archives
               '("org" . "https://orgmode.org/elpa/") t)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("marmalade" . "https://marmalade-repo.org/packages/") t)
  (package-initialize))

;; Automatically install packages using use-package
;; Should NOT use this right now since we're now using straight.el instead of
;; package.el
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (eval-when-compile (require 'use-package))

;; Have to set default-directory to the dotfiles directory in my dotfiles
;; repository because symlinks and Emacs apparently do not play well together.
(let ((default-directory my/dotfiles-emacs-dir))
  ;; Load the main configuration file.
  (org-babel-load-file my/main-emacs-init-org))

;;; .emacs ends here
