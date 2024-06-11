;;; my-straight-helpers.el --- summary -*- lexical-binding: t -*-

;; Author: Thomas Jacob Trabue
;; Maintainer: Thomas Jacob Trabue
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

;; Convenience functions that leverage the straight.el package manager.

;;; Code:

(defun my-straight-helpers-update-and-freeze ()
  "Custom function that updates all installed packages and regenerates the
    lock file."
  (interactive)
  (straight-pull-all)
  (straight-rebuild-all)
  (straight-freeze-versions t))

(defun my-straight-helpers-pull-recipe-repositories ()
  "Update all straight.el recipe repositories. This is a custom function that
        I defined in order to make my life easier.
        --tjtrabue"
  (interactive)
  (dolist (repo straight-recipe-repositories)
    (straight-pull-package repo)))

(provide 'my-straight-helpers)

;;; my-straight-helpers.el ends here
