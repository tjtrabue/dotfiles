;;; symbol-positions-fix.el --- summary -*- lexical-binding: t -*-

;; Author: Tom Trabue
;; Maintainer: Tom Trabue
;; Version: 1.0.0
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

;; Emacs version 29.0.50.153980 and onward no longer support
;; `read-with-symbol-position' and `read-symbol-positions-list'.  These
;; variables are needed by any plugin depending on the `elisp-refs' library,
;; such as `helpful' and `color-identifiers-mode'.  Here we provide dummy
;; definitions for these deprecated variables to allow those plugins to function
;; until they merge fixes of their own.

;;; Code:

;; Define read-symbol-positions-list
(defvar read-symbol-positions-list nil
  "This variable is no longer used in Emacs 29.0.50.153980 and onward.
Get rid of this variable as soon as no more plugins depend upon its presence.")

(provide 'symbol-positions-fix)

;;; symbol-positions-fix.el ends here
