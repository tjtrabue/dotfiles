;;; my-process.el --- summary -*- lexical-binding: t -*-

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

;; Custom filter functions for `make-process'.

;;; Code:

(require 'ansi-color)

(defun my-process--get-project-proc-name (proc-name &optional proc-dir)
  "Return the process name for a project directory.

PROC-NAME is the user-supplied process name, and PROC-DIR is the
directory where the process is running (defaults to `default-dir')."
  (let* ((default-directory (if proc-dir proc-dir default-directory))
          (project-dir-name (file-name-base (directory-file-name default-directory))))
    (concat proc-name "-" project-dir-name)))

;;;###autoload
(defun my-process-filter-colorize-output (proc string)
  "Filter function for `make-process' that colorizes output.

PROC is the process identifier, STRING is the string to be inserted into
the process buffer.

Mainly taken from the default insertion filter in the official Emacs
documentaion:
https://www.gnu.org/software/emacs/manual/html_node/elisp/Filter-Functions.html"
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        ;; Insert the text, advancing the process marker.
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (ansi-color-apply-on-region (process-mark proc) (point))
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

;;;###autoload
(defun my-process-make-color-comp-process (proc-name cmd &optional proc-dir)
  "Create a long-lived process named PROC-NAME using CMD.

CMD is a the command list to be passed to the `:command' keyword of
`make-process'.  See the documentation for `make-process' for more
information.

The process writes output to a new buffer named with the following
convention: *<proc-name>-<proc-dir>*.

May optionally specify PROC-DIR to change the directory where the
process will run."
  (let* ((default-directory (if proc-dir proc-dir default-directory))
          (project-proc-name (my-process--get-project-proc-name proc-name default-directory))
          (proc-buf-name (concat "*" project-proc-name "*"))
          (proc-buf (get-buffer-create proc-buf-name)))
    (with-current-buffer proc-buf
      (compilation-mode)
      ;; `compilation-mode' buffers are read-only by default, so we must specify our output buffer
      ;; is writable.
      (setq-local buffer-read-only nil))
    (when (get-process project-proc-name)
      (error (concat "Process " project-proc-name " already exists")))
    (message (concat "Making process " project-proc-name))
    (make-process
      :name project-proc-name
      :buffer (get-buffer proc-buf)
      :command cmd
      :sentinel (lambda (_proc event)
                  ;; (message (concat "process " (process-name proc) " event: " event))
                  (cond
                    ;; Kill the associated buffer when the process is killed.
                    ((string-match-p "killed\n$" event) (when (bufferp proc-buf)
                                                          (kill-buffer proc-buf)))))
      :filter #'my-process-filter-colorize-output)
    (display-buffer proc-buf)))

;;;###autoload
(defun my-process-kill-color-comp-process (proc-name &optional proc-dir)
  "Kill a running process PROC-NAME.

Optionally, specify the project directory PROC-DIR where the project is
running."
  (let* ((project-proc-name (my-process--get-project-proc-name proc-name proc-dir)))
    (if (get-process project-proc-name)
      (progn
        (message (concat "Killing process " project-proc-name))
        (kill-process project-proc-name))
      (error (concat "No active process " project-proc-name)))))

(provide 'my-process)

;;; my-process.el ends here
