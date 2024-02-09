;;; my-straight-bootstrap.el --- summary -*- lexical-binding: t -*-

;; Author: Billy's Dad
;; Maintainer: Billy's Dad
;; Version: version
;; Package-Requires: (dependencies)
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
;; package manager.

;;; Code:

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
  (require 'straight)

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

  ;; Install use-package via straight.
  ;; After this function runs, use-package will automatically use straight
  ;; to install packages if you specify ':stright t' instead of ':ensure t'.
  ;; If you have set straight-use-package-by-default to t, this is
  ;; unnecessary.
  (straight-use-package
    ;; Override the MELPA recipe in order to get all Elisp files for
    ;; use-package. For some reason, the MELPA recipe excludes several
    ;; important source files.
    '(use-package :type git :host github :repo "jwiegley/use-package"
       :files (:defaults))))

;; NOTE: This is `eval-and-compile`, not `eval-when-compile'. They are different
;; macros. `eval-and-compile' evaluates its code both during compilation and
;; when run. `eval-when-compile' only evaluates its code during compile-time.
(eval-and-compile
  (require 'use-package))

(provide 'my-straight-bootstrap)

;;; my-straight-bootstrap.el ends here
