;;; Initialization file for Common Lisp REPLs started by Roswell
;; The Lisp code in this file runs every time Roswell starts a new REPL.
;; It acts as a single point of configuration for any Common Lisp implementation
;; managed by Roswell, and acts as a drop-in replacement for many "rc" files
;; specific to particular Common Lisp implementations, like .sbclrc.

;; Needed for regex searching in Emacs SLY
(ql:quickload :cl-ppcre)
;; Needed for some SLY plugins
(ql:quickload "agnostic-lizard")
;; Portable MOP (Metaobject Protocol) implementation for Common Lisp.
(ql:quickload "closer-mop")

(defun print-condition-hook (condition hook)
  "Print error message CONDITION and abort the current operation."
  (declare (ignore hook))
  (format t "~A" condition)
  (clear-input)
  (abort))

;; Set debugger hook to print the error condition, but do not enter the
;; debugger.
;; (setf *debugger-hook* #'print-condition-hook)
