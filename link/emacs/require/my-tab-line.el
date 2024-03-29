;;; my-tab-line.el --- summary -*- lexical-binding: t -*-

;; Author: Thomas Jacob Trabue
;; Maintainer: Thomas Jacob Trabue
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

;; `tab-line-mode' and `global-tab-line-mode' create visual tabs grouped per
;; window, each one representing an open buffer.

;;; Code:

(require 'tab-line)

(defcustom my-tab-line-allowed-regexps '(;; Allow *scratch* buffer(s).
                                          "^.*\\*.*scratch.*\\*.*$"
                                          ;; Allow Customize windows.
                                          "^.*\\*.*Customize Group:.*\\*.*$")
  "List of regular expressions matching special buffers allowed in the tab line."
  :type '(regexp)
  :group 'my-tab-line)

(defcustom my-tab-line-allowed-major-modes '(dired-mode)
  "List of major modes whose buffers are allowed in the tab line."
  :type '(symbol)
  :group 'my-tab-line)

(defgroup my-tab-line '((my-tab-line-allowed-regexps custom-variable)
                         (my-tab-line-allowed-major-modes custom-variable))
  "My special `tab-line' variables.")
(defun my-tab-line-tab-name-function (buffer &optional _buffers)
  "Print tab name for BUFFER with some leading space for readability."
  (concat " " (buffer-name buffer) " "))

(defun my-tab-line-filter-display-buffers (bufs)
  "Determine which buffers in BUFS will show up in `tab-line'.

See the documentation for `perspective.el' for further details."
  (seq-filter (lambda (buf)
                (let ((buf-name (buffer-name buf))
                       (buf-file-name (buffer-file-name buf))
                       (buf-major-mode (buffer-local-value 'major-mode buf)))
                  (and
                    ;; First, make sure the buffer has a name.
                    buf-name
                    ;; Include buffers visiting files.
                    (or buf-file-name
                      (seq-some (lambda (mm)
                                  ;; Check the major mode of the each open
                                  ;; buffer to see if it is in our allow-list.
                                  (eq mm buf-major-mode))
                        my-tab-line-allowed-major-modes)
                      ;; Include specifically allowed non-file buffers.
                      (seq-some (lambda (regexp)
                                  (string-match-p regexp buf-name))
                        my-tab-line-allowed-regexps)))))
    ;; Reverse the buffer list because otherwise newly added buffers end up on
    ;; the left of the tab-line.
    (seq-reverse bufs)))

;; Specify a function to determine the list of buffers to display in the
;; `tab-line'.
(setq tab-line-tabs-function (lambda ()
                               ;; Default to apply filtering to the entire
                               ;; `buffer-list'.
                               (my-tab-line-filter-display-buffers (buffer-list))))

;; If non-nil, calling `tab-line-switch-to-next-tab' on the last tab selects
;; the first tab, and vice versa.
(setq tab-line-switch-cycling t)

;; Specify the function used to print the tab name in the tab line.
(setq tab-line-tab-name-function #'my-tab-line-tab-name-function)

;; Whether to show the "x" close button next to each tab.
(setq tab-line-close-button-show nil)

;; Whether to show the "+" button used to add a new tab.
(setq tab-line-new-button-show nil)

;; This is a custom callback function that the user can define which decides
;; into which tab group an input buffer gets added.
;;
;; This function takes a buffer as an argument, and should return a string
;; representing the name of the group into which the input buffer should be
;; placed. If the function returns nil, the buffer will be filtered out
;; entirely.
(setq tab-line-tabs-buffer-group-function nil)

;; Adjust tab-line faces.
(dolist (face '(tab-line
                 tab-line-tab-current
                 tab-line-tab-inactive
                 tab-line-tab-inactive-alternate
                 tab-line-highlight))
  (set-face-attribute face nil
    :height 1.0
    :width 'expanded
    ;; Increase tab-line height by adding a border box.
    :box `(:line-width 4 :color ,(face-background face nil t))))

;; Enable `tab-line-mode' globally.
(global-tab-line-mode 1)

(provide 'my-tab-line)

;;; my-tab-line.el ends here
