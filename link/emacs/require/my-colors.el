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

(defvar my-colors-color-theme-alist '((background . "#242730")
                                      (alt-background . "#2a2e38")
                                      (foreground . "#bbc2cf")
                                      (alt-foreground . "#5D656B")
                                      (current-line . "#1f5582")
                                      (selection . "#1f5582")
                                      (comment . "#6A8FBF")
                                      (red . "#ff665c")
                                      (orange . "#e69055")
                                      (yellow . "#FCCE7B")
                                      (green . "#7bc275")
                                      (cyan . "#5cEfFF")
                                      (dark-cyan . "#6A8FBF")
                                      (blue . "#51afef")
                                      (dark-blue . "#1f5582")
                                      (magenta . "#C57BDB")
                                      (violet . "#a991f1"))
  "The alist of all color name-value pairs for the current theme.")

;; Functions
;;;###autoload
(defun my-colors-get-colors-alist ()
  "Return the colors alist for my chosen theme."
  my-colors-color-theme-alist)

;;;###autoload
(defun my-colors-get-color (color)
  "Return COLOR from the color theme alist.

COLOR is one of the following symbols:

  'background
  'alt-background
  'foreground
  'alt-foreground
  'current-line
  'selection
  'comment
  'red
  'orange
  'yellow
  'green
  'cyan
  'dark-cyan
  'blue
  'dark-blue
  'magenta
  'violet"

  (let ((theme-alist (my-colors-get-colors-alist)))
    (cdr (assq color theme-alist))))

;;;###autoload
(defun my-colors-set-color (color-name color-value)
  "Set symbol COLOR-NAME in `my-colors-color-theme-alist' to COLOR-VALUE."
  (add-to-list 'my-colors-color-theme-alist `((,color-name . ,color-value))))

;;;###autoload
(defun my-colors-set-color-theme-alist (theme-alist &optional sublist)
  "Set `my-colors-color-theme-alist' to the specified THEME-ALIST.

If the user specifies SUBLIST as the name of a nested alist within
THEME-ALIST, then use that nested SUBLIST as the main theme alist."
  (require 'my-colors)
  (let ((the-right-alist theme-alist))
    (if sublist
        (setq the-right-alist (cdr (assq sublist theme-alist))))
    (setq my-colors-color-theme-alist the-right-alist)))

;;;###autoload
(defun my-colors-set-semantic-colors-for-theme ()
  "Make Semantic's faces match our theme."
  (set-face-attribute 'semantic-tag-boundary-face nil :overline
                      (alist-get 'purple my-colors-color-theme-alist)))

;;;###autoload
(defun my-colors-set-ecb-colors-for-theme ()
  "Make some of the faces in ECB cohesive with our color theme."
  (set-face-attribute 'ecb-default-highlight-face nil
                      :foreground (alist-get 'alt-background
                                             my-colors-color-theme-alist)
                      :background (alist-get 'orange my-colors-color-theme-alist)
                      :weight 'extra-bold))


;; Export this module
(provide 'my-colors)

;;; my-colors.el ends here
