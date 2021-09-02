;;; my-recentf.el --- summary -*- lexical-binding: t -*-

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

;; Provides functions for enabling and disabling recentf settings safely.

;;; Code:

(require 'recentf)

(defvar my-recentf-timer nil
  "Reference to timer for `recentf-save-list' so we can disable it later.")

;;;###autoload
(defun my-recentf-enable ()
  "Enable `recentf' and save the timer to `my-recentf-timer'."
  (interactive)
  ;; Turn on recentf-mode for keeping track of recently opened files.
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (setq recentf-max-saved-items 25)
  (global-set-key (kbd "C-x C-r") 'recentf-open-files)
  ;; Periodically save recent file list (every 5 minutes) so that we do not lose
  ;; the list if Emacs crashes.
  (setq my-recentf-timer (run-at-time nil (* 5 60) 'recentf-save-list)))

;;;###autoload
(defun my-recentf-disable()
  "Disable `recentf' configuration, disabling the `my-recentf-timer'.

Safe to run multiple times."
  (interactive)
  (when my-recentf-timer
    (cancel-timer my-recentf-timer))
  (when (eq (key-binding "C-x C-r") 'recentf-open-files)
    (global-unset-key (kbd "C-x C-r")))
  (recentf-mode -1))

(provide 'my-recentf)

;;; my-recentf.el ends here
