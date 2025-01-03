;;; my-straight-bootstrap.el --- summary -*- lexical-binding: t -*-

;; Author: Billy's Dad
;; Maintainer: Billy's Dad
;; Version: version
;; Package-Requires: ()
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This file is more of a script that bootstraps and loads the `straight.el'
;; package manager.  `straight.el' installs and "builds" packages from their
;; source code repositories rather than from opaque tarballs, allowing a far
;; more customizable approach to managing third-party packages.
;;
;; To update all packages installed with `straight', run `M-x straight-pull-all'.

;;; Code:

(defvar bootstrap-version)
;; Always use `use-package' when installing packages, making the `:straight t'
;; part of the `use-package' macro unnecessary.
(setq straight-use-package-by-default t)
;; When to check for a package modifications. The value of this variable is a list
;; of symbols. By default, `straight.el.' checks for modifications on startup, which
;; has major performance implications for Emacs' startup time.
;; Possible values:
;;   - 'find-at-startup -> straight checks for modifications on all packages at startup
;;   - 'find-when-checking -> straight checks for modifications when `straight-check-package'
;;                            or `straight-check-all' is run.
;;   - 'check-on-save -> straight adds a check to `before-save-hook' to check for file
;;                       modifications that you perform from within Emacs (does not catch
;;                       modifications made outside of Emacs).
;;   - 'watch-files -> Requires the external `watchexec' executable; straight starts a watcher
;;                     process to detect modifications made to files in `~/.emacs.d/straight/repos/'
(setq straight-check-for-modifications nil)
(when (executable-find "find")
  ;; Only attempt to run `find' to check for file modifications if we have `find' installed
  ;; (which is usually not the case on Windows).
  (add-to-list 'straight-check-for-modifications 'find-when-checking))
(if (and (executable-find "python3") (executable-find "watchexec"))
  ;; Use filesystem watchers to check for modifications if we have `watchexec' installed.
  (add-to-list 'straight-check-for-modifications 'watch-files)
  ;; Otherwise, only check for modifications after saving files in Emacs.
  (add-to-list 'straight-check-for-modifications 'check-on-save))
;; The straight.el branch to clone.
(setq straight-repository-branch "develop")
(let ((bootstrap-file
        (file-truename
          (concat
            user-emacs-directory "straight/repos/straight.el/bootstrap.el")))
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
  (with-no-warnings (load bootstrap-file nil 'nomessage))

  ;; Register more Git project hosting sites with Straight.el.
  ;; TODO: Remove these host additions once straight.el includes them by
  ;;       default.
  (add-to-list 'straight-hosts '(codeberg "codeberg.org" ".git"))
  (add-to-list 'straight-hosts '(savannah "git.savannah.gnu.org/git" ".git"))

  ;; Refresh package repositories
  (when (not (fboundp 'straight-pull-recipe-repositories))
    ;; Sometimes straight.el does not include the convenience function
    ;; `straight-pull-recipe-repositories', in which case we should alias
    ;; that function to our own custom version.
    (defalias
      'straight-pull-recipe-repositories
      'my-straight-helpers-pull-recipe-repositories))
  (straight-pull-recipe-repositories)

  ;; Install use-package via straight.
  ;; After this function runs, use-package will automatically use straight
  ;; to install packages if you specify ':stright t' instead of ':ensure t'.
  ;; If you have set straight-use-package-by-default to t, this is
  ;; unnecessary.
  (when (< emacs-major-version 29)
    ;; `use-package' comes built into modern versions of Emacs.
    (straight-use-package
      ;; Override the MELPA recipe in order to get all Elisp files for
      ;; use-package. For some reason, the MELPA recipe excludes several
      ;; important source files.
      '(use-package
         :type git
         :host github
         :repo "jwiegley/use-package"
         :files (:defaults)))))

(provide 'my-straight-bootstrap)

;;; my-straight-bootstrap.el ends here
