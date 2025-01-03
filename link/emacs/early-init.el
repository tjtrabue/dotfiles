;;; The very first configuration file, read even before `~/.emacs'.
;; Here we set values that must be defined before the first GUI frame is created.

;; * Emacs Lisp File Loading / Compilation
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

;; * Package Manager
;; We must determine which package management system we desire BEFORE we load
;; our main Emacs configuration files since Emacs initializes the package system
;; by default before loading `~/.emacs'.
(eval-and-compile
  (defconst my/use-straight-p t
    "Whether to use straight.el instead of Emacs' built-in package manager."))

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
