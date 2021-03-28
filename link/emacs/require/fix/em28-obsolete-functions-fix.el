;;; em28-obsolete-functions-fix.el --- summary -*- lexical-binding: t -*-

;; Author:
;; Maintainer:
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Homepage: homepage
;; Keywords: (hotfix emacs obsolete functions)


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

;; PROBLEM: Define-obsolete-function-alias causes backwards compatibility issues
;; The =define-obsolete-function-alias= function was updated in Emacs 28 to
;; use a larger number of arguments, causing a huge amount of backwards
;; compatibility problems for various plugins. This piece of function advice
;; intercepts calls to =define-obsolete-function-alias= and pads the argument
;; list before making the function call.

;;; Commentary:

;; commentary

;;; Code:

(define-advice em28-obsolete-functions-fix-function-alias
    (:filter-args (ll) define-obsolete-function-alias)
  "Advice to add `when' field to `define-obsolete-function-alias'."
  (let ((obsolete-name (pop ll))
        (current-name (pop ll))
        (when (if ll (pop ll) "1"))
        (docstring (if ll (pop ll) nil)))
    (list obsolete-name current-name when docstring)))

(define-advice em28-obsolete-functions-fix-variable-alias
    (:filter-args (ll) define-obsolete-variable-alias)
  "Advice to add `when' field to `define-obsolete-variable-alias'."
  (let ((obsolete-name (pop ll))
        (current-name (pop ll))
        (when (if ll (pop ll) "1"))
        (docstring (if ll (pop ll) nil)))
    (list obsolete-name current-name when docstring)))

(define-advice em28-obsolete-functions-fix-make-obsolete
    (:filter-args (ll) make-obsolete)
  "Advice to add `when' field to `make-obsolete'."
  (let ((obsolete-name (pop ll))
        (current-name (pop ll))
        (when (if ll (pop ll) "1")))
    (list obsolete-name current-name when)))

(define-advice em28-obsolete-functions-fix-make-obsolete-variable
    (:filter-args (ll) make-obsolete-variable)
  "Advice to add `when' field to `make-obsolete-variable'."
  (let ((obsolete-name (pop ll))
        (current-name (pop ll))
        (when (if ll (pop ll) "1"))
        (access-type (if ll (pop ll) nil)))
    (list obsolete-name current-name when access-type)))

(provide 'em28-obsolete-functions-fix)

;;; em28-obsolete-functions-fix.el ends here
