;;; org-functions-fix.el --- summary -*- lexical-binding: t -*-

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

;; Advice to fix obsolete versions of org-mode functions used by some
;; third-party libraries.

;;; Code:

(define-advice org-macro-initialize-templates
  (:filter-args (ll) fix-org-macro-initialize-templates)
  "New `org-macro-initialize-templates' doesn't take any arguments."
  nil)

(provide 'org-functions-fix)


;;; org-functions-fix.el ends here
