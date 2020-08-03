;;; my-colors.el --- summary -*- lexical-binding: t -*-

;; Author:
;; Maintainer:
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Color theme and functions used by me.

;;; Code:

;; Variables
(defvar my/tomorrow-color-theme 'eighties
  "The name of the sanityinc-tomorrow color theme.")

(defvar my/color-theme-alist nil
  "The alist of all color name-value pairs for the current theme.")

;; Functions
(defun my/get-sanityinc-tomorrow-theme-colors-alist (theme)
  "Return the color alist for a particular sanityinc tomorrow THEME.

THEME is one of 'night, 'day, 'bright, 'blue, or 'eighties."
  (cdr (assq theme color-theme-sanityinc-tomorrow-colors)))

(defun my/get-theme-colors-alist ()
  "Return the alist of all colors used by the current color theme."
  (my/get-sanityinc-tomorrow-theme-colors-alist my/tomorrow-color-theme))

(defun my/get-color (color)
  "Return COLOR from the color theme alist. "
  (let ((theme-alist (my/get-theme-colors-alist)))
    (cdr (assq color theme-alist))))

;; Convenience functions for retrieving specific colors
(defun my/get-color-background ()
  "Return the BACKGROUND color within the currently set theme."
  (interactive)
  (my/get-color 'background))

(defun my/get-color-alt-background ()
  "Return the ALT-BACKGROUND color within the currently set theme."
  (interactive)
  (my/get-color 'alt-background))

(defun my/get-color-current-line ()
  "Return the CURRENT-LINE color within the currently set theme."
  (interactive)
  (my/get-color 'current-line))

(defun my/get-color-selection ()
  "Return the SELECTION color within the currently set theme."
  (interactive)
  (my/get-color 'selection))

(defun my/get-color-foreground ()
  "Return the FOREGROUND color within the currently set theme."
  (interactive)
  (my/get-color 'foreground))

(defun my/get-color-comment ()
  "Return the COMMENT color within the currently set theme."
  (interactive)
  (my/get-color 'comment))

(defun my/get-color-red ()
  "Return the RED color within the currently set theme."
  (interactive)
  (my/get-color 'red))

(defun my/get-color-orange ()
  "Return the ORANGE color within the currently set theme."
  (interactive)
  (my/get-color 'orange))

(defun my/get-color-yellow ()
  "Return the YELLOW color within the currently set theme."
  (interactive)
  (my/get-color 'yellow))

(defun my/get-color-green ()
  "Return the GREEN color within the currently set theme."
  (interactive)
  (my/get-color 'green))

(defun my/get-color-aqua ()
  "Return the AQUA color within the currently set theme."
  (interactive)
  (my/get-color 'aqua))

(defun my/get-color-blue ()
  "Return the BLUE color within the currently set theme."
  (interactive)
  (my/get-color 'blue))

(defun my/get-color-purple ()
  "Return the PURPLE color within the currently set theme."
  (interactive)
  (my/get-color 'purple))

;; Export this module
(provide 'my-colors)

;;; my-colors.el ends here
