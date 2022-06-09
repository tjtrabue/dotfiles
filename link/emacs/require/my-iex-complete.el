;;; my-iex-complete.el --- summary -*- lexical-binding: t -*-

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

;; Completion for IEx, the interactive Elixir REPL.

;;; Code:

(defvar my-iex-complete-process nil
  "The IEx process used for completion.")

(defvar my-iex-complete-process-buffer-name "*iex-complete*"
  "The name of the buffer associated with the IEx completion process.")

(defun my-iex-complete-create-process ()
  "Create the IEx process to act as a completion server."
  (setq my-iex-complete-process (make-process
                                  :command '("iex")
                                  :buffer my-iex-complete-process-buffer-name
                                  :connection-type 'pipe)))

(provide 'my-iex-complete)

;;; my-iex-complete.el ends here
