;;; pyenv.el --- Emacs integration for pyenv

;; Copyright (C) 2013 Yves Senn

;; URL: https://github.com/senny/pyenv.el
;; Author: Yves Senn <yves.senn@gmail.com>
;; Version: 0.0.3
;; Created: 20 February 2014
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

;; M-x global-pyenv-mode toggle the configuration done by pyenv.el

;; M-x pyenv-use-global prepares the current Emacs session to use
;; the global python configured with pyenv.

;; M-x pyenv-use allows you to switch the current session to the python
;; implementation of your choice.

;;; Compiler support:

;; helper function used in variable definitions
(defcustom pyenv-installation-dir (or (getenv "PYENV_ROOT")
                                      (concat (getenv "HOME") "/.pyenv/"))
  "The path to the directory where pyenv was installed."
  :group 'pyenv
  :type 'directory)

(defun pyenv--expand-path (&rest segments)
  (let ((path (mapconcat 'identity segments "/"))
        (installation-dir (replace-regexp-in-string "/$" "" pyenv-installation-dir)))
    (expand-file-name (concat installation-dir "/" path))))

(defcustom pyenv-interactive-completion-function
  (if ido-mode 'ido-completing-read 'completing-read)
  "The function which is used by pyenv.el to interactivly complete user input"
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
  "whether to use version-alias")

(defvar pyenv-executable (pyenv--expand-path "bin" "pyenv")
  "path to the pyenv executable")

(defvar pyenv-python-shim (pyenv--expand-path "shims" "python")
  "path to the python shim executable")

(defvar pyenv-global-version-file (pyenv--expand-path "version")
  "path to the global version configuration file of pyenv")

(defvar pyenv-version-environment-variable "PYENV_VERSION"
  "name of the environment variable to configure the pyenv version")

(defvar pyenv-version-alias-environment-variable "PYENV_VERSION_ALIAS"
  "name of the environment variable to configure the pyenv version")

(defvar pyenv-binary-paths (list (cons 'shims-path (pyenv--expand-path "shims"))
                                 (cons 'bin-path (pyenv--expand-path "bin")))
  "these are added to PATH and exec-path when pyenv is setup")

(defface pyenv-active-python-face
  '((t (:weight bold :foreground "Red")))
  "The face used to highlight the current python on the modeline.")

(defvar pyenv--initialized nil
  "indicates if the current Emacs session has been configured to use pyenv")

(defvar pyenv--modestring nil
  "text pyenv-mode will display in the modeline.")
(put 'pyenv--modestring 'risky-local-variable t)

(defvar pyenv-mode-hook nil
  "functions to run after switching pyenvs")

(defvar pyenv-modestring-prefix "["
  "string to prefix in the modeline")

(defvar pyenv-modestring-postfix "]"
  "string to postfix in the modeline")

;;;###autoload
(defun pyenv-use-global ()
  "activate pyenv global python"
  (interactive)
  (pyenv-use (pyenv--global-python-version) "global"))

;;;###autoload
(defun pyenv-use-corresponding ()
  "search for .python-version and activate the corresponding python"
  (interactive)
  (let* ((curr-pyenv (pyenv/version-name))
         (new-file-path (pyenv/version-file))
         (new-pyenv (pyenv/version-file-read new-file-path))
         (alias-file-path (pyenv/version-alias-file))
         (alias (if (string= new-file-path pyenv-global-version-file)
                    "global"
                    (if alias-file-path (pyenv/version-file-read alias-file-path)))))
    (if (not (string= curr-pyenv new-pyenv))
        (pyenv-use new-pyenv alias)
      (message "[pyenv] same pyenv, not changing"))))

;;;###autoload
(defun pyenv-use (python-version &optional alias)
  "choose what python you want to activate"
  (interactive
   (let ((picked-python (pyenv--completing-read "Python version: " (pyenv/list))))
     (list picked-python)))

  (pyenv--activate python-version alias)
  (run-hooks 'pyenv-mode-hook)
  (message (concat "[pyenv] using " python-version)))

(defun pyenv/version-name ()
  (pyenv--call-process "version-name"))

(defun pyenv/version-file ()
  (pyenv--call-process "version-file"))

(defun pyenv/version-file-read (path)
  (pyenv--call-process "version-file-read" path))

(defun pyenv/version-alias ()
  (pyenv--call-process "version-alias"))

(defun pyenv/version-alias-file ()
  (pyenv--call-process "version-alias-file"))

(defun pyenv/list ()
  (split-string (pyenv--call-process "versions" "--bare") "\n"))

(defun pyenv--setup ()
  (when (not pyenv--initialized)
    (dolist (path-config pyenv-binary-paths)
      (let ((bin-path (cdr path-config)))
        (setenv "PATH" (concat bin-path ":" (getenv "PATH")))
        (add-to-list 'exec-path bin-path)))
    (setq eshell-path-env (getenv "PATH"))
    (setq pyenv--initialized t)
    (pyenv--update-mode-line)))

(defun pyenv--teardown ()
  (when pyenv--initialized
    (dolist (path-config pyenv-binary-paths)
      (let ((bin-path (cdr path-config)))
        (setenv "PATH" (replace-regexp-in-string (regexp-quote (concat bin-path ":")) "" (getenv "PATH")))
        (setq exec-path (remove bin-path exec-path))))
    (setq eshell-path-env (getenv "PATH"))
    (setq pyenv--initialized nil)))

(defun pyenv--activate (python-version &optional alias)
  (setenv pyenv-version-environment-variable python-version)
  (if pyenv-use-alias (setenv pyenv-version-alias-environment-variable alias))
  (pyenv--update-mode-line))

(defun pyenv--completing-read (prompt options)
  (funcall pyenv-interactive-completion-function prompt options))

(defun pyenv--global-python-version ()
  (pyenv/version-file-read pyenv-global-version-file))

(defun pyenv--call-process (&rest args)
  (with-temp-buffer
    (let* ((success (apply 'call-process pyenv-executable nil t nil
                           (delete nil args)))
           (raw-output (buffer-substring-no-properties
                        (point-min) (point-max)))
           (output (pyenv--replace-trailing-whitespace raw-output)))
      (if (= 0 success)
          output
        (message output)))))

(defun pyenv--replace-trailing-whitespace (text)
  (replace-regexp-in-string "[[:space:]]\\'" "" text))


(defun pyenv--modeline-alias-or-active ()
  (let ((active (pyenv/version-name))
        (alias (pyenv/version-alias)))
    (if pyenv-use-alias (if alias alias active)
      active)))

(defun pyenv--update-mode-line ()
  (setq pyenv--modestring (funcall pyenv-modeline-function
                                   (pyenv--modeline-alias-or-active))))

(defun pyenv--modeline-with-face (current-python)
  (list pyenv-modestring-prefix
        (list (propertize current-python 'face 'pyenv-active-python-face))
        pyenv-modestring-postfix))

(defun pyenv--modeline-plain (current-python)
  (list pyenv-modestring-prefix current-python pyenv-modestring-postfix))

(defun pyenv-update-on-buffer-switch (prev curr)
  "Function that can be added to switch-buffer-functions hook to update
your pyenv whenever you switch to a Python buffer that uses a different
pyenv version"
  (if (string-equal "Python" (format-mode-line mode-name nil nil curr))
      (pyenv-use-corresponding)))

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
