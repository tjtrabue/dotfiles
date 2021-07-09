;;; my-inferior-lisp.el --- summary -*- lexical-binding: t -*-

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

;; Package containing code for managing and interacting with external LISP
;; programs, such as Common Lisp implementations.

;;; Code:

;;;###autoload
(defun my-inferior-lisp-get-program ()
  "Return the shell command used to launch the INFERIOR-LISP-PROGRAM.
This is command becomes the selected Common Lisp interpreter."
  (let (;; Use Roswell CL manager if possible.
        (path-to-ros (executable-find "ros"))
        ;; Revert back to SBCL installation if nothing else available.
        (path-to-sbcl (executable-find "sbcl")))
     (if path-to-ros
         (concat path-to-ros " run")
       (concat path-to-sbcl))))

(provide 'my-inferior-lisp)

;;; my-inferior-lisp.el ends here
