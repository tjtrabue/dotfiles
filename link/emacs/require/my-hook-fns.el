;;; my-hook-fns.el --- summary -*- lexical-binding: t -*-

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

;; commentary

;;; Code:

;;;###autoload
(defun my-hook-fns-add-hook-for-major-modes (hook-fn major-modes)
  "Add HOOK-FN to mode hook for each mode in MAJOR-MODES."
  (mapc (lambda (mm)
          (let ((mm-hook (intern (concat (symbol-name mm) "-hook"))))
            (add-hook mm-hook hook-fn)))
    major-modes))

(provide 'my-hook-fns)

;;; my-hook-fns.el ends here
