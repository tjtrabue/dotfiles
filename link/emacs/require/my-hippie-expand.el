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

(require 'my-hook-fns)

;; Adjust the list of functions that hippie-expand will try when invoked.
(setq hippie-expand-try-functions-list
  '(try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     ;; try-expand-dabbrev-from-kill
     ;; try-complete-lisp-symbol-partially
     ;; try-complete-lisp-symbol
     try-complete-file-name-partially try-complete-file-name
     ;; try-expand-all-abbrevs
     ;; try-expand-list
     ;; try-expand-line
     ))

(defun my-hippie-expand--add-expand-funcs (new-hippie-expand-funcs)
  "Prepend list NEW-HIPPIE-EXPAND-FUNCS to `hippie-expand-try-functions-list'.

If using Yasnippets, place `yas-hippie-try-expand' at the front
of `hippie-expand-try-functions-list' to ensure that snippet
expansion gets priority over other expansions."
  (setq-local hippie-expand-try-functions-list
    (delete-dups
      (if (or (bound-and-true-p yas-minor-mode)
            (bound-and-true-p yas-global-mode))
        ;; Place Yasnippet hippie-expand function at the beginning of expand
        ;; functions list to make sure snippet expansion gets priority.
        (append
          '(yas-hippie-try-expand) new-hippie-expand-funcs
          (remove
            'yas-hippie-try-expand hippie-expand-try-functions-list))
        (append
          new-hippie-expand-funcs hippie-expand-try-functions-list)))))

(defun my-hippie-expand-set-lisp-hooks ()
  "Create hooks to add `hippie-expand' functions specific to Lisp major modes."
  (my-hook-fns-add-hook-for-major-modes
    (lambda ()
      (my-hippie-expand--add-expand-funcs
        '(try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
    my/lisp-major-modes))

;; Set up Lisp mode hooks for hippie-expand.
(my-hippie-expand-set-lisp-hooks)

;;;###autoload
(defun my-hippie-expand-or-tab (arg)
  "Invoke `tab-to-tab-stop' or `hippie-expand' with the relevant prefix ARG."
  (interactive "*P")
  (if (or (bolp)
        (string-match-p "[[:space:]]" (byte-to-string (preceding-char))))
    ;; If point is at beginning of line or previous character is blank, insert a
    ;; tab or number of spaces; otherwise, try to expand text with
    ;; `hippie-expand'.
    (tab-to-tab-stop)
    (hippie-expand arg)))

(provide 'my-hippie-expand)

;;; my-hippie-expand.el ends here
