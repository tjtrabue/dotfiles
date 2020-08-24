;;; my-colors.el --- summary -*- lexical-binding: t -*-

;; Author: Tom Trabue
;; Maintainer: Tom Trabue
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

;; This package provides an API for interacting with my chosen color theme.

;;; Code:

;; Variables
(defvar my-colors-sinct-theme 'eighties
  "The name of the sanityinc-tomorrow color theme.

Can be one of 'night, 'day, 'bright, 'blue, or 'eighties")

(defvar my-colors-theme-alist-name "color-theme-sanityinc-tomorrow-colors"
  "The name of the main colors theme alist.")

(defvar my-colors-color-theme-alist nil
  "The alist of all color name-value pairs for the current theme.")

;; Functions
(defun my-colors-get-sinct-colors-alist ()
  "Return the alist of all colors used by the current color theme."
  (let (theme-alist)
    ;; Evaluate the theme alist name:
    (cond ((stringp my-colors-theme-alist-name)
           (setq theme-alist (eval (intern my-colors-theme-alist-name))))
          ((symbolp my-colors-theme-alist-name)
           (setq theme-alist (eval my-colors-theme-alist-name)))
          (t
           (setq theme-alist my-colors-theme-alist-name)))
    ;; Return the alist
    (cdr (assq my-colors-sinct-theme theme-alist))))

;;;###autoload
(defun my-colors-get-colors-alist ()
  "Return the colors alist for my chosen theme."
  (my-colors-get-sinct-colors-alist))

;;;###autoload
(defun my-colors-get-color (color)
  "Return COLOR from the color theme alist.

COLOR is one of the following symbols:

  'background
  'alt-background
  'foreground
  'current-line
  'selection
  'comment
  'red
  'orange
  'yellow
  'green
  'aqua
  'blue
  'purple"

  (let ((theme-alist (my-colors-get-colors-alist)))
    (cdr (assq color theme-alist))))

;; Export this module
(provide 'my-colors)

;;; my-colors.el ends here
