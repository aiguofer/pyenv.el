;;; pyenv.el --- Emacs integration for pyenv -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Yves Senn, Diego Fernandez

;; URL: https://github.com/aiguofer/pyenv.el
;; Author: Yves Senn <yves.senn@gmail.com>, Diego Fernandez <aiguo.fernandez@gmail.com>
;; Version: 1.0.0
;; Created: 20 February 2014
;; Package-Requires: ((emacs "24"))
;; Keywords: python pyenv

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; pyenv.el lets you use the pyenv version corresponding to each project
;; and shell without the need for other packages. It also lets you use
;; the pyenv-version-alias extension. See https://github.com/aiguofer/pyenv.el
;; for more.

;; M-x global-pyenv-mode enable use of pyenv and display active pyenv.

;; M-x pyenv-use-global prepares the current Emacs session to use
;; the global python configured with pyenv.

;; M-x pyenv-use-corresponding allows you to switch the current session
;; to the pyenv version for the current project.

;; M-x pyenv-use allows you to switch the current session to the python
;; implementation of your choice.

;;; Code:
(require 'eshell)

(defcustom pyenv-installation-dir (or (getenv "PYENV_ROOT")
                                      (concat (getenv "HOME") "/.pyenv/"))
  "The path to the directory where pyenv was installed."
  :group 'pyenv
  :type 'directory)

(defun pyenv--expand-path (&rest segments)
  "Expand SEGMENTS full directory path within the pyenv installation."
  (let ((path (mapconcat #'identity segments "/"))
        (installation-dir (replace-regexp-in-string "/$" "" pyenv-installation-dir)))
    (expand-file-name (concat installation-dir "/" path))))

(defcustom pyenv-interactive-completion-function (if ido-mode 'ido-completing-read 'completing-read)
  "The function which is used by pyenv.el to interactivly complete user input."
  :group 'pyenv
  :type 'function)

(defcustom pyenv-show-active-python-in-modeline t
  "Toggles wether pyenv-mode shows the active python in the modeline."
  :group 'pyenv
  :type 'boolean)

(defcustom pyenv-modeline-function 'pyenv--modeline-with-face
  "Function to specify the pyenv representation in the modeline."
  :group 'pyenv
  :type 'function)

(defvar pyenv-use-alias nil
  "Whether to use version-alias.")

(defvar pyenv-executable (pyenv--expand-path "bin" "pyenv")
  "Path to the pyenv executable.")

(defvar pyenv-python-shim (pyenv--expand-path "shims" "python")
  "Path to the python shim executable.")

(defvar pyenv-global-version-file (pyenv--expand-path "version")
  "Path to the global version configuration file of pyenv.")

(defvar pyenv-version-environment-variable "PYENV_VERSION"
  "Name of the environment variable to configure the pyenv version.")

(defvar pyenv-version-alias-environment-variable "PYENV_VERSION_ALIAS"
  "Name of the environment variable to configure the pyenv version.")

(defvar pyenv-virtualenv-environment-variable "VIRTUAL_ENV"
  "Name of the environment variable to configure the virtualenv version.")

(defvar pyenv-pyenv-virtualenv-environment-variable "PYENV_VIRTUAL_ENV"
  "Name of the environment variable to configure the pyenv virtualenv version.")

(defvar pyenv-binary-paths (list (cons 'shims-path (pyenv--expand-path "shims"))
                                 (cons 'bin-path (pyenv--expand-path "bin")))
  "These are added to PATH and `exec-path`' when pyenv is setup.")

(defface pyenv-active-python-face '((t (:weight bold :foreground "Red")))
  "The face used to highlight the current python on the modeline."
  :group 'pyenv)

(defvar pyenv--initialized nil
  "Indicates if the current Emacs session has been configured to use pyenv.")

(defvar pyenv--modestring nil
  "Text pyenv-mode will display in the modeline.")
(put 'pyenv--modestring 'risky-local-variable t)

(defvar pyenv-mode-hook nil
  "Functions to run after switching pyenvs.")

(defvar pyenv-modestring-prefix "["
  "String to prefix in the modeline.")

(defvar pyenv-modestring-postfix "]"
  "String to postfix in the modeline.")

(defvar pyenv-set-path 't
  "Whether to set up PATH when initializing, otherwise it'll use inherited PATH.")

;;;###autoload
(defun pyenv-use-global ()
  "Activate pyenv global python."
  (interactive)
  (pyenv-use (pyenv--global-python-version) "global"))

;;;###autoload
(defun pyenv-use-corresponding ()
  "Search for .python-version and activate the corresponding python."
  (interactive)
  (let* ((curr-pyenv (pyenv--version-name))
         (new-file-path (pyenv--version-file))
         (new-pyenv (pyenv--version-file-read new-file-path))
         (alias-file-path (pyenv--version-alias-file))
         (alias (if (string= new-file-path pyenv-global-version-file)
                    "global"
                    (if alias-file-path (pyenv--version-file-read alias-file-path)))))
    (if (not (string= curr-pyenv new-pyenv))
        (pyenv-use new-pyenv alias))))

;;;###autoload
(defun pyenv-use (python-version &optional alias)
  "Choose what PYTHON-VERSION you want to activate, using an optional ALIAS for display."
  (interactive
   (let ((picked-python (pyenv--completing-read "Python version: " (pyenv--list))))
     (list picked-python)))

  (pyenv--activate python-version alias)
  (run-hooks 'pyenv-mode-hook)
  (message "[pyenv] using %s" python-version))

(defun pyenv--prefix ()
  "Get peynv prefix."
  (pyenv--call-process "prefix"))

(defun pyenv--version-name ()
  "Get peynv version name."
  (pyenv--call-process "version-name"))

(defun pyenv--version-file ()
  "Get peynv version file path."
  (pyenv--call-process "version-file"))

(defun pyenv--version-file-read (path)
  "Get contents of peynv version file for PATH."
  (pyenv--call-process "version-file-read" path))

(defun pyenv--version-alias ()
  "Get peynv version name alias."
  (pyenv--call-process "version-alias"))

(defun pyenv--version-alias-file ()
  "Get peynv version alias file path."
  (pyenv--call-process "version-alias-file"))

(defun pyenv--list ()
  "Get peynv version names."
  (split-string (pyenv--call-process "versions" "--bare") "\n"))

(defun pyenv--list-virtualenvs ()
  "Get peynv virtualenv version names."
  (split-string (pyenv--call-process "virtualenvs" "--bare") "\n"))

(defun pyenv--setup ()
  "Set up pyenv by ensuring the PATH is correcty set if necessary."
  (when (and pyenv-set-path (not pyenv--initialized))
    (dolist (path-config pyenv-binary-paths)
      (let ((bin-path (cdr path-config)))
        (setenv "PATH" (concat bin-path ":" (getenv "PATH")))
        (add-to-list 'exec-path bin-path)))
    (setq eshell-path-env (getenv "PATH"))
    (setq pyenv--initialized t))
  (pyenv-use-global))

(defun pyenv--teardown ()
  "Tear down pyenv by resetting PATH if necessary."
  (when pyenv--initialized
    (dolist (path-config pyenv-binary-paths)
      (let ((bin-path (cdr path-config)))
        (setenv "PATH" (replace-regexp-in-string (regexp-quote (concat bin-path ":")) "" (getenv "PATH")))
        (setq exec-path (remove bin-path exec-path))))
    (setq eshell-path-env (getenv "PATH"))
    (setq pyenv--initialized nil)))

(defun pyenv--activate (python-version &optional alias)
  "Set the the pyenv environment variable to PYTHON-VERSION and the pyenv-version-alias environment variable to ALIAS then update modeline."
  (setenv pyenv-version-environment-variable python-version)
  (if (member (pyenv--version-name) (pyenv--list-virtualenvs))
      (progn
        (setenv pyenv-virtualenv-environment-variable (pyenv--prefix))
        (setenv pyenv-pyenv-virtualenv-environment-variable (pyenv--prefix)))
    (progn
        (setenv pyenv-virtualenv-environment-variable)
        (setenv pyenv-pyenv-virtualenv-environment-variable)))
  (if pyenv-use-alias (setenv pyenv-version-alias-environment-variable alias))
  (pyenv--update-mode-line))

(defun pyenv--completing-read (prompt options)
  "Show interactive PROMPT with completions for OPTIONS based on available pyenvs."
  (funcall pyenv-interactive-completion-function prompt options))

(defun pyenv--global-python-version ()
  "Return the global pyenv version."
  (pyenv--version-file-read pyenv-global-version-file))

(defun pyenv--call-process (&rest args)
  "Call the pyenv command passed in ARGS."
  (with-temp-buffer
    (let* ((success (apply #'call-process pyenv-executable nil t nil
                           (delete nil args)))
           (raw-output (buffer-substring-no-properties
                        (point-min) (point-max)))
           (output (pyenv--replace-trailing-whitespace raw-output)))
      (if (= 0 success)
          output
        (message output)))))

(defun pyenv--replace-trailing-whitespace (text)
  "Remove trailing spaces from the given TEXT."
  (replace-regexp-in-string "[[:space:]]\\'" "" text))


(defun pyenv--modeline-alias-or-active ()
  "Return either the alias or the version name of the currently active pyenv version."
  (let ((active (pyenv--version-name))
        (alias (pyenv--version-alias)))
    (if pyenv-use-alias (if alias alias active)
      active)))

(defun pyenv--update-mode-line ()
  "Update the modeline using `pyenv-modeline-function'."
  (setq pyenv--modestring (funcall pyenv-modeline-function
                                   (pyenv--modeline-alias-or-active))))

(defun pyenv--modeline-with-face (current-python)
  "Set the pyenv modeline to CURRENT-PYTHON using pyenv-active-python-face."
  (list pyenv-modestring-prefix
        (list (propertize current-python 'face 'pyenv-active-python-face))
        pyenv-modestring-postfix))

(defun pyenv--modeline-plain (current-python)
  "Set the pyenv modeline in plain text to CURRENT-PYTHON."
  (list pyenv-modestring-prefix current-python pyenv-modestring-postfix))

;;;###autoload
(define-minor-mode global-pyenv-mode
  "use pyenv to configure the python version used by your Emacs."
  :global t
  (if global-pyenv-mode
      (progn
        (when pyenv-show-active-python-in-modeline
          (unless (memq 'pyenv--modestring global-mode-string)
            (setq global-mode-string (append (or global-mode-string '(""))
                                             '(pyenv--modestring)))))
        (pyenv--setup))
    (setq global-mode-string (delq 'pyenv--modestring global-mode-string))
    (pyenv--teardown)))

(provide 'pyenv)

;;; pyenv.el ends here
