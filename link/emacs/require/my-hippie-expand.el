;;; my-hippie-expand.el --- summary -*- lexical-binding: t -*-

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

;; My custom `hippie-expand' code.  It makes more sense to keep it here rather
;; than in a `use-package' specification because hippie-expand is written in C,
;; not Emacs Lisp, so there is no feature named "hippie-expand" into which
;; `use-package' can hook.

;;; Code:

;; Adjust the list of functions that hippie-expand will try when invoked.
(setq hippie-expand-try-functions-list
  '(
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     ;; try-expand-dabbrev-from-kill
     ;; try-complete-lisp-symbol-partially
     ;; try-complete-lisp-symbol
     try-complete-file-name-partially
     try-complete-file-name
     ;; try-expand-all-abbrevs
     ;; try-expand-list
     ;; try-expand-line
     ))

(defun my-hippie-expand-set-lisp-hooks ()
  "Create hooks to add `hippie-expand' functions specific to Lisp major modes."
  (mapc (lambda (mm)
          (let ((mode-hook (intern (concat (symbol-name mm) "-hook"))))
            (add-hook mode-hook
              (lambda ()
                (setq-local hippie-expand-try-functions-list
                  (append hippie-expand-try-functions-list
                    '(try-complete-lisp-symbol-partially
                       try-complete-lisp-symbol)))))))
    my/lisp-major-modes))

;; Set up Lisp mode hooks for hippie-expand.
(my-hippie-expand-set-lisp-hooks)

;;;###autoload
(defun my-hippie-expand-or-insert-tab ()
  "Expand text before point or insert a tab if preceding text is blank."
  (interactive)
  (if (string-match-p "[[:space:]]" (byte-to-string (preceding-char)))
    (tab-to-tab-stop)
    (call-interactively 'hippie-expand)))
(provide 'my-hippie-expand)

;;; my-hippie-expand.el ends here
