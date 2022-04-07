;;; my-completing-read.el --- summary -*- lexical-binding: t -*-

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

;; Custom completing-read functions.

;;; Code:

;;;###autoload
(defun my-completing-read-comint-input-ring (init-str)
  "Read `comint-mode' command history from the minibuffer with completions.

INIT-STR is the initial completion string placed into the minibuffer.
If INIT-STR is nil, no initial string will be placed into the minibuffer."
  (interactive "P")
  (completing-read "Shell History: "
    (ring-elements comint-input-ring) nil nil init-str))

(provide 'my-completing-read)

;;; my-completing-read.el ends here
