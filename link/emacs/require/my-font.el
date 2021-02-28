;;; my-font.el --- summary -*- lexical-binding: t -*-

;; Author: Thomas Jacob Trabue
;; Maintainer: Thomas Jacob Trabue
;; Version: version
;; Package-Requires: ((emacs "24.4"))
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

;; commentary

;;; Code:

(require 'cl-lib)

(defvar my-font-font-family "FiraCode Nerd Font"
  "The default font family used throughout Emacs.")

;; Set the default font.
(set-face-attribute 'default nil
  :family my-font-font-family ;; The font's name
  :height 105 ;; Unit is 1/10 pt size (i.e., height 110 = 11 pt font)
  :weight 'semi-bold ;; Style
  :width 'normal)

;;;###autoload
(defun my-font-adjust-font-size (frame)
  "Inspired by https://emacs.stackexchange.com/a/44930/17066.

FRAME is not used directly, but its presence is necessary for this function to
be attached to the 'window-size-change-functions' hook.

Adjust the font size of an Emacs frame whenever the frame's size changes."
  (let* ((attrs (frame-monitor-attributes))
          (width-px (cl-fourth (cl-third attrs))) ;; Width of current screen
          (size 13)) ;; Default for standard screen
    (when (= width-px 3840) ;; Very Large display
      (setq size 20))
    (when (= width-px 2560) ;; Large display
      (setq size 16))
    ;; Adjust default font size.
    (set-face-attribute 'default nil
      :height (* 10 size))
    ;; Also adjust line number font size
    (if (facep 'linum)
      (set-face-attribute 'linum nil
        :height (* 10 size)))
    ;; Also adjust relative line number font size
    (if (facep 'linum-relative-current-face)
      (set-face-attribute 'linum-relative-current-face nil
        :height (* 10 size)))))

(provide 'my-font)

;;; my-font.el ends here
