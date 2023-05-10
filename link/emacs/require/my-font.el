;;; my-font.el --- summary -*- lexical-binding: t -*-

;; Author: Thomas Jacob Trabue
;; Maintainer: Thomas Jacob Trabue
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
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

;; This package contains my personal font configuration.  It's supposed to be a
;; single point of configuration for my Emacs font defaults.

;;; Code:

(defvar my-font-mono-family "Cascadia Code PL"
  "The default font used throughout Emacs.")

(defvar my-font-mono-family-fallback "DejaVu Sans Mono"
  "The fallback default font used throughout Emacs.")

(defvar my-font-fixed-pitch-family my-font-mono-family
  "The default font used for fixed-pitch font faces.")

(defvar my-font-fixed-pitch-family-fallback my-font-mono-family-fallback
  "The fallback font used for fixed-pitch font faces.")

(defvar my-font-variable-pitch-family "Arkibal Serif"
  "The default font used for variable-pitch font faces.")

(defvar my-font-variable-pitch-family-fallback "DejaVu Serif"
  "The fallback font to use for the variable pitch font.")

;;;###autoload
(defun my-font-set-default-font ()
  "Set the default font for all of Emacs."
  (set-face-attribute
    'default nil
    ;; The name of the font.
    :family (cond
              ((member my-font-mono-family (font-family-list))
                my-font-mono-family)
              (t my-font-mono-family-fallback))
    ;; Unit is 1/10 pt size (i.e., height 110 = 11 pt font).
    :height 110
    ;; Style.
    :weight 'semi-bold
    :width 'normal)
  (set-face-attribute
    'fixed-pitch nil
    :family (cond
              ((member my-font-fixed-pitch-family (font-family-list))
                my-font-fixed-pitch-family)
              (t my-font-fixed-pitch-family-fallback))
    :height 110
    :weight 'semi-bold
    :width 'normal)
  (set-face-attribute
    'variable-pitch nil
    :family (cond
              ((member my-font-variable-pitch-family (font-family-list))
                my-font-variable-pitch-family)
              (t my-font-variable-pitch-family-fallback))
    :height 1.0
    :weight 'semi-bold))

;; Set fallback font for glyphs not found in default font.
(when (member "Symbols Nerd Font" (font-family-list))
  (set-fontset-font t nil "Symbols Nerd Font" nil 'append))

;;;###autoload
(defun my-font-adjust-font-size (frame)
  "Inspired by https://emacs.stackexchange.com/a/44930/17066.

FRAME is not used directly, but its presence is necessary for this function to
be attached to the `window-size-change-functions' hook.

Adjust the font size of an Emacs frame based on the monitor's size."
  (let
    (
      (width-px (display-pixel-width)) ; Monitor width
      (font-point 11) ;; Font point size for standard screen
      (font-height))
    ;; Select the font point based on the monitor's resolution.
    (when (= width-px 3840) ;; Very Large display
      (setq font-point 20))
    (when (= width-px 2560) ;; Large display
      (setq font-point 16))
    (when (= width-px 1920) ;; Standard display
      (setq font-point 11))
    ;; The face ':height' attribute is 10 * the font point.
    (setq font-height (* 10 font-point))
    ;; Adjust default font size.
    (set-face-attribute 'default nil :height font-height)
    ;; Also adjust line number font size
    (when (facep 'linum)
      (set-face-attribute 'linum nil :height font-height))
    ;; Also adjust relative line number font size
    (when (facep 'linum-relative-current-face)
      (set-face-attribute
        'linum-relative-current-face nil
        :height font-height))))

(provide 'my-font)

;;; my-font.el ends here
