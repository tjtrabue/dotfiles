;;; my-random.el --- summary -*- lexical-binding: t -*-

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

;; commentary

;;; Code:

(defvar my-random-default-length 5
  "The default string length used by functions in `my-random.el'.")

;;;###autoload
(defun my-random-number (&optional num)
  "Insert a string of random decimal digits.
If an integer NUM is provided, use this as the length of the string.
Otherwise, the default length for the string is 5.
URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'
Version 2017-05-24"
  (let* ((charset "1234567890")
          (baseCount (length charset))
          (random-string "")
          (random-index))
    (dotimes (_ (if (numberp num) (abs num) my-random-default-length))
      (setq random-index (random baseCount))
      (setq random-string (concat (substring charset random-index (1+ random-index))
                            random-string)))
    random-string))

;;;###autoload
(defun my-random-hex (&optional num)
  "Return a string of random hexadecimal digits.
If an integer NUM is provided, use this as the length of the string.
Otherwise, the default length for the string is 5.
URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'
Version 2017-08-03"
  (let ((n (if (numberp num) (abs num) my-random-default-length)))
    (format  (concat "%0" (number-to-string n) "x" ) (random (1- (expt 16 n))))))

;;;###autoload
(defun my-random-string (&optional num)
  "Return a random alphanumeric string.
If an integer NUM is provided, use this as the length of the string.
Otherwise, the default length for the string is 5.
The possible chars are: A to Z, a to z, 0 to 9.
URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'
Version 2018-08-03"
  (let* ((charset "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
          (baseCount (length charset))
          (random-string "")
          (random-index))
    (dotimes (_ (if (numberp num) (abs num) my-random-default-length))
      (setq random-index (random baseCount))
      (setq random-string (concat (substring charset random-index (1+ random-index))
                            random-string)))
    random-string))

(my-random-string)

(provide 'my-random)

;;; my-random.el ends here
