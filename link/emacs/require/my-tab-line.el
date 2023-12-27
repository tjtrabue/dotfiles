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

(defun my-tab-line-tab-name-function (buffer &optional _buffers)
  "Print tab name for BUFFER with some leading space for readability."
  (concat " " (buffer-name buffer)))

;; If non-nil, calling `tab-line-switch-to-next-tab' on the last tab selects
;; the first tab, and vice versa.
(setq tab-line-switch-cycling t)

;; Specify the function used to print the tab name in the tab line.
(setq tab-line-tab-name-function #'my-tab-line-tab-name-function)

;; Enable `tab-line-mode' globally.
(global-tab-line-mode 1)

(provide 'my-tab-line)

;;; my-tab-line.el ends here
