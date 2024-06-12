;;; my-tab-line.el --- summary -*- lexical-binding: t -*-

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

;; `tab-line-mode' and `global-tab-line-mode' create visual tabs grouped per
;; window, each one representing an open buffer.

;;; Code:

;;;###autoload
(require 'easy-mmode)

;;;###autoload
(defcustom my-tab-line-allowed-regexps '(;; Allow *scratch* buffer(s).
                                          "^.*\\*.*scratch.*\\*.*$"
                                          ;; Allow Customize windows.
                                          "^.*\\*.*Customize Group:.*\\*.*$")
  "List of regular expressions matching special buffers allowed in the tab line."
  :type '(regexp)
  :group 'my-tab-line)

;;;###autoload
(defcustom my-tab-line-allowed-major-modes '(dired-mode)
  "List of major modes whose buffers are allowed in the tab line."
  :type '(symbol)
  :group 'my-tab-line)

;;;###autoload
(defcustom my-tab-line-tabs-function #'my-tab-line-default-tabs-function
  "Specifies the function used to filter tabs for display."
  :type '(function)
  :group 'my-tab-line)

;;;###autoload
(defcustom my-tab-line-tab-name-format-function #'tab-line-tab-name-format-default
  "Specifies the function used to format each tab in the `tab-line'."
  :type '(function)
  :group 'my-tab-line)

;;;###autoload
(defgroup my-tab-line '((my-tab-line-allowed-regexps custom-variable)
                         (my-tab-line-allowed-major-modes custom-variable))
  "My special `tab-line' variables.")

(defun my-tab-line-tab-name-function (buffer &optional _buffers)
  "Print tab name for BUFFER with some leading space for readability."
  (concat " " (buffer-name buffer) " "))

(defun my-tab-line-default-tabs-function ()
  "Displays buffers in the `tab-line'."
  (my-tab-line-filter-display-buffers (buffer-list)))

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

;;;###autoload
(define-minor-mode my-tab-line-mode
  "Global minor mode that toggles a fancy tab-line.

By default, it widens each buffer tab, and uses font colors to mark
which file buffers have been edited since their last write.  This
tab-line can be nicely enhanced with `all-the-icons'.  See my
configuration in `my-icons.org' for more details."
  :global t
  :init-value nil
  :after-hook
  (progn
    (require 'tab-line)

    ;; Whenever the user customizes `my-tab-line-tabs-function', pass the
    ;; customization along to `tab-line-tabs-function'.
    (add-variable-watcher 'my-tab-line-tabs-function
      (lambda (_symbol newval _operation _where)
        (setq tab-line-tabs-function newval)))

    ;; Whenever the user customizes `my-tab-line-tab-name-format-function', pass the
    ;; customization along to `tab-line-tab-name-format-function'.
    (add-variable-watcher 'my-tab-line-tab-name-format-function
      (lambda (_symbol newval _operation _where)
        (setq tab-line-tab-name-format-function newval)))

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

    (if my-tab-line-mode
      (progn
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
        ;; Specify a function to determine the list of buffers to display in the
        ;; `tab-line'.
        (setq tab-line-tabs-function my-tab-line-tabs-function)
        ;; Turn on the tab-line globally.
        (global-tab-line-mode 1))
      ;; Deactivate the tab-line when the user toggles this minor-mode.
      (global-tab-line-mode -1))))

(provide 'my-tab-line)

;;; my-tab-line.el ends here
