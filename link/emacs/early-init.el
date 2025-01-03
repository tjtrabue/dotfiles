;;; early-init.el --- First config file loaded -*- lexical-binding: t -*-

;;; Commentary:
;; The very first configuration file, read even before `~/.emacs'.
;; Here we set values that must be defined before the first GUI frame is created.

;;; Code:

;; * Set Variables Needed Right Away
(eval-and-compile
  (defconst my/use-straight-p t
    "Whether to use straight.el instead of Emacs' built-in package manager.")
  (defconst my/color-theme 'dark
    "A symbol that is equal to one of: \\='light, \\='dark."))

;; * Emacs Lisp File Loading / Compilation
;; In non-interactive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer 'noninteractive)
;; Define the level of native compilation optimization.
;; Allowed values: -1, 0, 1, 2, 3.
(setq native-comp-speed 3)
;; The number of parallel async jobs to perform.
;; Defaults to 0, meaning use half the available CPU cores.
(setq native-comp-async-jobs-number 0)
;; If non-nil, compile loaded .elc files asynchronously.
;; After compilation, each function definition is updated to use the
;; natively-compiled one.
(setq native-comp-jit-compilation t)

;; * Initial Garbage Collection Options
;; Set the garbage collection threshold super high for startup. We'll need to
;; reset these values later so as not to impede our computer's performance, but
;; it does speed up Emacs' start time.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; * Initial Faces
;; Set the initial frame's coloration while waiting for the theme to load.  This
;; avoids a bright flash when Emacs first starts if using a dark theme.
(let ((bg (if (eq my/color-theme 'dark) "#000000" "#ffffff"))
       (fg (if (eq my/color-theme 'dark) "#ffffff" "#000000")))
  (set-face-attribute 'default nil :background bg :foreground fg))

;; * Fullscreen Options
;; To change the initial fullscreen behavior of a frame using =initial-frame-alist=
;; or =default-frame-alist=, append one of the following options to one or both of
;; those lists:
;;
;; - `(fullscreen . fullwidth)': Make the frame as wide as possible, but do not
;; adjust vertical size.
;; - `(fullscreen . fullheight)': Make the frame as tall as possible, but do not
;; adjust horizontal size.
;; - `(fullscreen . maximized)': Set height and width to the size of the screen.
;; - `(fullscreen . fullboth)': Like 'maximized, but you cannot readjust the
;; frame size later with the mouse (removed the window manager options, such as
;; the close, minimize, and maximize buttons).
;;
;; ** How to adjust the initial frame's size
;; Use the `initial-frame-alist' to change the size of the first frame Emacs
;; creates on startup.
;;
;; ** How to adjust all frames' sizes
;; To change the size of all frames Emacs creates, set `default-frame-alist', instead.
;; Maximize Emacs' initial frame.
(add-to-list 'initial-frame-alist `(fullscreen . maximized))

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(dolist (opt '((vertical-scroll-bars . nil)
                (menu-bar-lines . 0)
                (tool-bar-lines . 0)))
  (add-to-list 'default-frame-alist opt))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)

;; * Package Manager
;; We must determine which package management system we desire BEFORE we load
;; our main Emacs configuration files since Emacs initializes the package system
;; by default before loading `~/.emacs'.

(if (and (not my/use-straight-p) (>= emacs-major-version 24))
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
    (add-to-list 'package-archives '("org"       . "https://orgmode.org/elpa/") t)
    (add-to-list 'package-archives '("melpa"     . "https://melpa.org/packages/") t)
    (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
    (package-initialize))
;;; OTHERWISE...
  ;; Do not auto-initialize packages! This can slow down Emacs's startup time.
  (setq package-enable-at-startup nil)
  ;; this tells package.el not to add those pesky customized variable settings
  ;; at the end of your init.el
  (setq package--init-file-ensured t))

;; * use-package
;; This option must be set before `use-package' is loaded in the primary configuration file.
(setq use-package-enable-imenu-support t)

;; * Xorg Integration
;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)
;;; early-init.el ends here
