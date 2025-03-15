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

(defvar my-font-preset-alist
  '((arkibal (:family "Arkibal Serif" :height 1.0 :weight semi-bold :width normal))
     ;;; Variable pitch fonts
     ;; https://freedafonts.com/arkibal-font/
     ;; https://www.dafont.com/neogrey.font
     (neogrey (:family "Neogrey Medium" :height 1.0 :weight semi-bold :width normal))
     ;; https://freedesignresources.net/triakis-font-family-free-weight/
     (triakis (:family "Triakis  Font" :height 1.3 :weight semi-bold :width normal))
     ;; https://www.fontshare.com/fonts/clash-display
     (clash-display (:family "Clash Display Variable" :height 110 :weight semi-bold :width normal))
     ;; https://fontshare.com/fonts/satoshi
     (satoshi (:family "Satoshi Variable" :height 110 :weight medium :width normal))
     ;; https://www.fontshare.com/fonts/switzer
     (switzer (:family "Switzer Variable" :height 110 :weight regular :width normal))
     ;; Basic Google sans-serif font usually installed through package manager.
     (noto-sans (:family "Noto Sans" :height 110 :weight semi-bold :width normal))
     ;;; Fixed pitch fonts
     (cascadia-code (:family "Cascadia Code PL" :height 110 :weight semi-bold :width normal))
     (jetbrains-mono (:family "JetBrainsMono Nerd Font" :height 110 :weight semi-bold :width normal))
     (fira-code (:family "FiraCode Nerd Font" :height 110 :weight semi-bold :width normal))
     (noto-sans-mono (:family "Noto Sans Mono" :height 110 :weight semi-bold :width normal))
     (dejavu-sans-mono (:family "DejaVu Sans Mono" :height 110 :weight semi-bold :width normal)))
  "Alist containing all named font preset configurations.")

(defun my-font-get-preset-plist (preset fallback)
  "Return the plist for PRESET if available, or FALLBACK otherwise."
  (let ((font-families (font-family-list))
         (preset-plist (car (alist-get preset my-font-preset-alist)))
         (fallback-plist (car (alist-get fallback my-font-preset-alist))))
    (if (member (plist-get preset-plist :family) font-families)
      preset-plist
      fallback-plist)))

(defun my-font-set-font-face-for-preset (face preset fallback)
  "Set FACE to the settings in PRESET if available, or FALLBACK preset.

FACE is one of \\='default, \\='fixed-pitch, or \\='variable-pitch.

PRESET is a symbol corresponding to the name of one of the preset plists
in `my-font-preset-alist', such as \\='noto-sans-mono.

FALLBACK is another preset symbol to use if the font specified in PRESET
is not available."
  (let* ((actual-plist (my-font-get-preset-plist preset fallback)))
    (set-face-attribute
      face nil
      ;; The name of the font.
      :family (plist-get actual-plist :family)
      ;; Unit is 1/10 pt size (i.e., height 110 = 11 pt font).
      :height (plist-get actual-plist :height)
      ;; Style.
      :weight (plist-get actual-plist :weight)
      ;; A symbol
      :width  (plist-get actual-plist :width))))

;;;###autoload
(defcustom my-font-default-preset 'cascadia-code
  "The name of the preset used by default settings, or as a fallback."
  :type '(symbol)
  :group 'my-font
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (my-font-set-font-face-for-preset 'default value 'noto-sans-mono)))

;;;###autoload
(defcustom my-font-fixed-pitch-preset 'cascadia-code
  "The name of the preset used for mono-spaced or fixed pitch fonts."
  :type '(symbol)
  :group 'my-font
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (my-font-set-font-face-for-preset 'fixed-pitch value 'dejavu-sans-mono)))

;;;###autoload
(defcustom my-font-variable-pitch-preset 'switzer
  "The name of the preset used for variable pitch fonts."
  :type '(symbol)
  :group 'my-font
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (my-font-set-font-face-for-preset 'variable-pitch value 'noto-sans)))

;;;###autoload
(defgroup my-font '((my-font-default-preset custom-variable)
                     (my-font-fixed-pitch-preset custom-variable)
                     (my-font-variable-pitch-preset custom-variable))
  "All customizable variables for `my-font'.")

;;;###autoload
(defun my-font-set-default-font ()
  "Set default fonts (fixed pitch and variable pitch) for all Emacs frames."
  (my-font-set-font-face-for-preset 'default my-font-default-preset 'noto-sans-mono)
  (my-font-set-font-face-for-preset 'fixed-pitch my-font-fixed-pitch-preset 'dejavu-sans-mono)
  (my-font-set-font-face-for-preset 'variable-pitch my-font-variable-pitch-preset 'noto-sans))

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

;; Set fallback font for glyphs and emojis not found in default font.
(let ((font-families (font-family-list)))
  (when (member "Noto Color Emoji" font-families)
    (set-fontset-font t nil "Noto Color Emoji" nil 'append))
  (when (member "Symbols Nerd Font" font-families)
    (set-fontset-font t nil "Symbols Nerd Font" nil 'append))
  (when (member "Symbola" font-families)
    (set-fontset-font t nil "Symbola" nil 'append))
  (when (member "Quivira" font-families)
    (set-fontset-font t nil "Quivira" nil 'append)))

(provide 'my-font)

;;; my-font.el ends here
