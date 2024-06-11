;;; my-process-filter.el --- summary -*- lexical-binding: t -*-

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

;; Filter functions for `make-process'.

;;; Code:

(require 'ansi-color)

;;;###autoload
(defun my-process-filter-colorize-output (proc string)
  "Filter function for `make-process' that colorizes output.

PROC is the process identifier, STRING is the string to be inserted into
the process buffer.

Mainly taken from the default insertion filter in the official Emacs
documentaion:
https://www.gnu.org/software/emacs/manual/html_node/elisp/Filter-Functions.html"
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        ;; Insert the text, advancing the process marker.
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (ansi-color-apply-on-region (process-mark proc) (point))
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(provide 'my-process-filter)

;;; my-process-filter.el ends here
