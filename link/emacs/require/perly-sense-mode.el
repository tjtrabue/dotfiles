;;; perly-sense-mode.el --- summary -*- lexical-binding: t -*-

;; Author: Tom Trabue
;; Maintainer: Tom Trabue
;; Version: 0.0.1
;; Package-Requires: ()
;; Homepage: homepage
;; Keywords: keywords


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


;;; Commentary:

;; commentary

;;; Code:

(defun ps/load-perly-sense ()
  "Load the perly-sense.el file."
  ;; *** PerlySense load (don't touch) ***
  (setq ps/external-dir (shell-command-to-string "perly_sense external_dir"))
  (if (string-match "Devel.PerlySense.external" ps/external-dir)
      (progn
        (message
         "PerlySense elisp files  at (%s) according to perly_sense, loading..."
         ps/external-dir)
        (setq load-path (cons
                         (expand-file-name
                          (format "%s/%s" ps/external-dir "emacs")
                          ) load-path))
        (load "perly-sense")
        )
    (message "Could not identify PerlySense install dir.
  Is Devel::PerlySense installed properly?
  Does 'perly_sense external_dir' give you a proper directory? (%s)" ps/external-dir)
    ))

(defun ps/configure-flymake (use-flymake)
  "Set flymake configuration for PerlySense.

  USE-FLYMAKE is a boolean value stating your intention to use or not to use
  flymake. If non-nil, it assumes you want to use flymake.

  Not very useful anymore since flymake is deprecated in favor
  of flycheck."

  (when use-flymake
    ;; If you only want syntax check whenever you save, not continously
    (setq flymake-no-changes-timeout 9999)
    (setq flymake-start-syntax-check-on-newline nil)))

(defun ps/set-faces ()
  "Set faces for `perly-sense-mode'."
  ;; Emacs named colors: http://www.geocities.com/kensanata/colors.html
  ;; The following colors work fine with a white X11
  ;; background. They may not look that great on a console with the
  ;; default color scheme.
  (set-face-background 'flymake-errline "antique white")
  (set-face-background 'flymake-warnline "lavender")
  (set-face-background 'dropdown-list-face "lightgrey")
  (set-face-background 'dropdown-list-selection-face "grey"))

(defun ps/configure-code-coverage ()
  "Set code coverage variable values for `perly-sense-mode'."

  ;; ** Code Coverage Visualization **
  ;; If you have a Devel::CoverX::Covered database handy and want to
  ;; display the sub coverage in the source, set this to t
  (setq ps/enable-test-coverage-visualization nil))

(defun ps/configure-miscellaneous ()
  "Set misc variable values for `perly-sense-mode'."
  ;; Run calls to perly_sense as a prepared shell command. Experimental
  ;; optimization, please try it out.
  (setq ps/use-prepare-shell-command t))

(defun ps/activate-perly-sense-mode ()
  "Activation hook for `perly-sense-mode'."
  ;; Define local variables for this mode
  (make-local-variable 'ps/key-prefix)
  (make-local-variable 'ps/load-flymake)
  (make-local-variable 'ps/external-dir)
  (make-local-variable 'ps/enable-test-coverage-visualization)
  (make-local-variable 'ps/use-prepare-shell-command)

  ;; The PerlySense prefix key (unset only if needed, like for \C-o)
  (global-unset-key (kbd "C-c C-o"))
  (setq ps/key-prefix (kbd "C-c C-o"))

  ;; ** Flymake **
  ;; Load flymake if t
  ;; Flymake must be installed.
  ;; It is included in Emacs 22
  ;;     (or http://flymake.sourceforge.net/, put flymake.el in your load-path)
  (setq ps/load-flymake nil)
  ;; Note: more flymake config below, after loading PerlySense

  (ps/load-perly-sense)
  (ps/set-faces)
  (ps/configure-flymake ps/load-flymake)
  (ps/configure-code-coverage)
  (ps/configure-miscellaneous))

(defun ps/deactivate-perly-sense-mode ()
  "Deactivation hook for `perly-sense-mode'."
  (unload-feature 'perly-sense))

;;;###autoload
(define-minor-mode perly-sense-mode
  "Activates the PerlySense IDE backend for Perl."
  :lighter " PerlySense"
  :keymap (let ((perly-sense-map (make-sparse-keymap)))
            perly-sense-map)

  (if perly-sense-mode
      (ps/activate-perly-sense-mode)
    (ps/deactivate-perly-sense-mode)))

(provide 'perly-sense-mode)

;;; perly-sense-mode.el ends here
