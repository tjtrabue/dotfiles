;;; Tangle all of my configuration files.

(require 'org)

(defvar my/org-init-file (file-truename
                           (concat user-emacs-directory
                                   "my-init.org")))

(defvar my/plugin-notebook-dir (file-truename
                                 (concat user-emacs-directory
                                         "plugin-notebook")))

(setq org-babel-default-header-args:emacs-lisp
  '((:lexical . t)
    (:tangle . "yes")))

(org-babel-tangle-file my/org-init-file)
(mapc (lambda (f)
        (org-babel-tangle-file f))
      (directory-files my/plugin-notebook-dir t "\\.org$"))