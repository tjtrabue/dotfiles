;;; my-os.el --- summary -*- lexical-binding: t -*-

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

;; My own custom operating system functions.

;;; Code:

;;;###autoload
(defun my-os-get-linux-distro ()
  "Return the name of the current Linux distribution.
If the OS is not a Linux-based system, return nil."
  (interactive)
  (if (eq system-type 'gnu/linux)
      (shell-command-to-string "lsb_release -sd")
    nil))

(provide 'my-os)

;;; my-os.el ends here
