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

(defvar pyenv-executable (pyenv--expand-path "bin" "pyenv")
  "path to the pyenv executable")
  
(defvar pyenv-python-shim (pyenv--expand-path "shims" "python")
  "path to the python shim executable")

(defvar pyenv-global-version-file (pyenv--expand-path "version")
  "path to the global version configuration file of pyenv")

(defvar pyenv-version-environment-variable "PYENV_VERSION"
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

;;;###autoload
(defun pyenv-use-global ()
  "activate pyenv global python"
  (interactive)
  (pyenv-use (pyenv--global-python-version)))

;;;###autoload
(defun pyenv-use-corresponding ()
  "search for .python-version and activate the corresponding python"
  (interactive)
  (let ((version-file-path (or (pyenv--locate-file ".python-version")
                               (pyenv--locate-file ".pyenv-version"))))
    (if version-file-path (pyenv-use (pyenv--read-version-from-file version-file-path))
      (message "[pyenv] could not locate .python-version or .pyenv-version"))))

;;;###autoload
(defun pyenv-use (python-version)
  "choose what python you want to activate"
  (interactive
   (let ((picked-python (pyenv--completing-read "Python version: " (pyenv/list))))
     (list picked-python)))
  (pyenv--activate python-version)
  (message (concat "[pyenv] using " python-version)))

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

(defun pyenv--activate (python-version)
  (setenv pyenv-version-environment-variable python-version)
  (pyenv--update-mode-line))

(defun pyenv--completing-read (prompt options)
  (funcall pyenv-interactive-completion-function prompt options))

(defun pyenv--global-python-version ()
  (pyenv--read-version-from-file pyenv-global-version-file))

(defun pyenv--read-version-from-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (pyenv--replace-trailing-whitespace (buffer-substring-no-properties (point-min) (point-max)))))

(defun pyenv--locate-file (file-name)
  "searches the directory tree for an given file. Returns nil if the file was not found."
  (let ((directory (locate-dominating-file default-directory file-name)))
    (when directory (concat directory file-name))))

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

(defun pyenv--update-mode-line ()
  (setq pyenv--modestring (funcall pyenv-modeline-function
                                   (pyenv--active-python-version))))

(defun pyenv--modeline-with-face (current-python)
  (append '(" [")
          (list (propertize current-python 'face 'pyenv-active-python-face))
          '("]")))

(defun pyenv--modeline-plain (current-python)
  (list " [" current-python "]"))

(defun pyenv--active-python-version ()
  (or (getenv pyenv-version-environment-variable) (pyenv--global-python-version)))

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
