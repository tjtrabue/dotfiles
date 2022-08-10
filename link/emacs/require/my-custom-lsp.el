;;; my-custom-lsp.el --- summary -*- lexical-binding: t -*-

;; Author: Thomas Jacob Trabue
;; Maintainer: Thomas Jacob Trabue
;; Version: version
;; Package-Requires: ((lsp-mode "3.0""))
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

;; My custom LSP configuration that works alongside `lsp-mode'.

;;; Code:

;; Common Lisp emulation library
(require 'cl-lib)

(defvar my-custom-lsp-format-major-mode-blacklist '(clojure-mode
                                                     cperl-mode
                                                     lua-mode
                                                     perl-mode
                                                     python-mode
                                                     sh-mode)

  "List of major modes to ignore when activating lsp-format hooks, usually
because the LSP server for these major modes does not yet support document
formatting, or because their formatters are lacking in features. Thankfully
the reformatter package provides a generic interface for creating formatters
for any programming language that supports a formatting tool.")

(defvar my-custom-lsp-enabled-modes '(c++-mode
                                       c-mode
                                       clojure-mode
                                       cmake-mode
                                       cperl-mode
                                       css-mode
                                       dart-mode
                                       dockerfile-mode
                                       elixir-mode
                                       go-mode
                                       haskell-mode
                                       haskell-literate-mode
                                       html-mode
                                       java-mode
                                       javascript-mode
                                       js-mode
                                       json-mode
                                       LaTeX-mode
                                       latex-mode
                                       less-css-mode
                                       ;; lisp-mode
                                       lua-mode
                                       markdown-mode
                                       mhtml-mode
                                       objc-mode
                                       perl-mode
                                       php-mode
                                       python-mode
                                       ruby-mode
                                       scss-mode
                                       sh-mode
                                       sql-mode
                                       swift-mode
                                       terraform-mode
                                       TeX-latex-mode
                                       tex-mode
                                       web-mode
                                       xml-mode
                                       yaml-mode)
  "List of all major modes allowing `lsp-mode' to run as a minor mode.")

(defvar my-custom-lsp-language-id-configuration '((lisp-mode . "lisp"))
  "List to be concatenated with `lsp-language-id-configuration'.")

;;;###autoload
(defun my-custom-lsp-add-format-on-save-hook (mode)
  "This function adds a buffer local hook for the major mode represented by
MODE that runs `lsp-format-buffer' before saving the buffer's file."
  ;; Ignore major-modes listed in the blacklist variable.
  (unless (member mode my-custom-lsp-format-major-mode-blacklist)
    (add-hook 'before-save-hook #'lsp-format-buffer 0 'local)))

;;;###autoload
(defun my-custom-lsp-add-lsp-mode-hooks ()
  "Add hooks to the major modes specified in `my-lsp-enabled-modes' to
automatically start lsp-mode when editing files associated with those major
modes, as well as format buffers on save."
  (mapc (lambda (mode)
          ;; This is necessary for providing closure-like behavior
          (lexical-let ((mode mode)
                         (hook-name (concat (symbol-name mode) "-hook")))
            (add-hook (intern hook-name)
              (lambda ()
                ;; Shorten company prefix and reduce completion delay since LSP
                ;; servers are very efficient backends.
                (setq-local company-minimum-prefix-length 1)
                ;; (setq-local company-idle-delay 0.0)
                ;; Automatically start lsp when you visit a relevant file
                (lsp-deferred)
                ;; Enable auto-revert to keep files up to date with filesystem.
                (auto-revert-mode 1)
                ;; Format lsp-mode buffers on save.
                (my-custom-lsp-add-format-on-save-hook mode)))))
    my-custom-lsp-enabled-modes))

;;;###autoload
(defun my-custom-lsp-check-lsp-enabled-for-mode-p (mode &optional all &rest modes)
  "Return non-nil if a specific MODE is enabled for `lsp-mode' based on the
value of `my-lsp-enabled-modes'.

MODES are optional additional major modes to check for in
`my-enabled-mode-for-lsp'. The behavior of the check depends on the value of
ALL.

The optional ALL argument is a truthy value determining whether or not all
specified major modes appear in `my-enabled-mode-for-lsp'. If ALL is non-nil,
then the final result of this function will only be non-nil if all supplied
modes are present in `my-enabled-mode-for-lsp'. If ALL is nil, then the result
of this function will be non-nil if any of the supplied major modes are in the
list."
  (if modes
      (let ((full-modes (cons mode modes)))
        (if all
            (not (cl-set-difference full-modes my-custom-lsp-enabled-modes))
          (cl-intersection full-modes my-custom-lsp-enabled-modes)))
    (member mode my-custom-lsp-enabled-modes)))

;;;###autoload
(defun my-custom-lsp-add-dap-mode-tool-hooks ()
  "Add hooks for various languages to pull in `dap-mode' tools to aid in
debugging."
  ;; Add C++ DAP tools when entering c++-mode
  (add-hook 'c++-mode-hook (lambda ()
                             (require 'dap-cpptools))))

;;;###autoload
(defun my-custom-lsp-register-lsp-servers ()
  "Register all custom LSP servers that we want."
  ;; CL-LSP still seems incompatible with Emacs.
  (my-custom-lsp--register-common-lisp-lsp-servers))

(defun my-custom-lsp--register-common-lisp-lsp-servers ()
  "Register Common Lisp languageservers for use with `lsp-mode'."
  ;; Register cl-lsp as a language server for Common Lisp.
  ;; You'll probably need to install it with roswell.
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "cl-lsp")
    :activation-fn (lsp-activate-on "lisp")
    :server-id 'cl-lsp)))

;;;###autoload
(defun my-custom-lsp-add-language-ids ()
  "Add extra language ID specifications for `lsp-mode'."
  (setq lsp-language-id-configuration
    (append my-custom-lsp-language-id-configuration
      lsp-language-id-configuration)))

(provide 'my-custom-lsp)

;;; my-custom-lsp.el ends here
