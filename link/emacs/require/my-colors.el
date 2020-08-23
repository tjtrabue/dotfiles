;;; my-colors.el --- summary -*- lexical-binding: t -*-

;; Author:
;; Maintainer:
;; Version: 1.0.0
;; Package-Requires: (version-num)
;; Homepage: homepage
;; Keywords:


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

;; (require 'color-theme-sanityinc-tomorrow)

;; Variables
(defvar my-colors-sinct-theme 'eighties
  "The name of the sanityinc-tomorrow color theme.

Can be one of 'night, 'day, 'bright, 'blue, or 'eighties")

(defvar my-colors-color-theme-alist nil
  "The alist of all color name-value pairs for the current theme.")

;; Functions
(defun my-colors-get-theme-colors-alist ()
  "Return the alist of all colors used by the current color theme."
  (cdr (assq my-colors-sinct-theme color-theme-sanityinc-tomorrow-colors)))

(defun my-colors-get-color (color)
  "Return COLOR from the color theme alist."
  (let ((theme-alist (my-colors-get-theme-colors-alist)))
    (cdr (assq color theme-alist))))

;;; Convenience functions for retrieving specific colors

;;;###autoload
(defun my-colors-get-color-background ()
  "Return the BACKGROUND color within the currently set theme."
  (interactive)
  (my-colors-get-color 'background))

;;;###autoload
(defun my-colors-get-color-alt-background ()
  "Return the ALT-BACKGROUND color within the currently set theme."
  (interactive)
  (my-colors-get-color 'alt-background))

;;;###autoload
(defun my-colors-get-color-current-line ()
  "Return the CURRENT-LINE color within the currently set theme."
  (interactive)
  (my-colors-get-color 'current-line))

;;;###autoload
(defun my-colors-get-color-selection ()
  "Return the SELECTION color within the currently set theme."
  (interactive)
  (my-colors-get-color 'selection))

;;;###autoload
(defun my-colors-get-color-foreground ()
  "Return the FOREGROUND color within the currently set theme."
  (interactive)
  (my-colors-get-color 'foreground))

;;;###autoload
(defun my-colors-get-color-comment ()
  "Return the COMMENT color within the currently set theme."
  (interactive)
  (my-colors-get-color 'comment))

;;;###autoload
(defun my-colors-get-color-red ()
  "Return the RED color within the currently set theme."
  (interactive)
  (my-colors-get-color 'red))

;;;###autoload
(defun my-colors-get-color-orange ()
  "Return the ORANGE color within the currently set theme."
  (interactive)
  (my-colors-get-color 'orange))

;;;###autoload
(defun my-colors-get-color-yellow ()
  "Return the YELLOW color within the currently set theme."
  (interactive)
  (my-colors-get-color 'yellow))

;;;###autoload
(defun my-colors-get-color-green ()
  "Return the GREEN color within the currently set theme."
  (interactive)
  (my-colors-get-color 'green))

;;;###autoload
(defun my-colors-get-color-aqua ()
  "Return the AQUA color within the currently set theme."
  (interactive)
  (my-colors-get-color 'aqua))

;;;###autoload
(defun my-colors-get-color-blue ()
  "Return the BLUE color within the currently set theme."
  (interactive)
  (my-colors-get-color 'blue))

;;;###autoload
(defun my-colors-get-color-purple ()
  "Return the PURPLE color within the currently set theme."
  (interactive)
  (my-colors-get-color 'purple))

;; Export this module
(provide 'my-colors)

;;; my-colors.el ends here
