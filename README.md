# pyenv.el

Use pyenv to manage your Python versions within Emacs using [pyenv](https://github.com/pyenv/pyenv). This is useful to make sure your code completion backend has the correct `PYTHONPATH` set up so auto-completion, jump to def, etc work as expected in each project. It can also display the current `pyenv` version running in the modeline as a reminder of the environment you're working in.

This repo was originally forked from [pyenv.el](https://github.com/cyberved/pyenv.el), which was forked from [rbenv.el](https://github.com/senny/rbenv.el).

This version has been changed quite a bit to support a variety of features:

- Allow multiple pyenv versions enabled at once (just like pyenv)
- Use a global pyenv mode
- Only activate pyenv mode by setting `PYENV_VERSION` environment variable
- Allow easy customization of modeline text
- Support use of [pyenv-version-alias](https://github.com/aiguofer/pyenv-version-alias)
- Make it easy to auto-switch pyenv when switching between buffers
- Allow hooks to run when pyenv changes

Why not [pyenv-mode](https://github.com/proofit404/pyenv-mode)?

- Aside from setting the environment variable, `pyenv-mode` also sets the `python-shell-virtualenv-root` which doesn't make much sense in the context of using pyenv, since pyenv enables using multiple versions at once as well as using non-virtualenv versions. It would be though to change that design.
- `pyenv-mode` depends on `pythonic`, and to get something resembling `global-mode` you also need [pyenv-mode-auto](https://github.com/ssbb/pyenv-mode-auto).
- I forked this a long time ago to enable using multiple versions at once like pyenv. It was easier to continue modifying this than try to re-architect and hope my PRs get accepted with `pyenv-mode`

## Installation

Clone this repo into a directory and:

```lisp
(add-to-list 'load-path (expand-file-name "/path/to/pyenv.el/"))
(require 'pyenv)
(global-pyenv-mode)
```

Alternatively, use [straight.el](https://github.com/raxod502/straight.el):

```listp
(use-package pyenv
  :straight (:host github :repo "aiguofer/pyenv.el")
  :config
  (global-pyenv-mode))
```

## Usage

* `global-pyenv-mode` activate / deactivate pyenv.el (The current Python version is shown in the modeline)
* `pyenv-use-global` will activate your global python
* `pyenv-use` allows you to choose what python version you want to use
* `pyenv-use-corresponding` searches for .python-version and activates
  the corresponding python

## Configuration

### pyenv installation directory
By default pyenv.el assumes that you installed pyenv into
`~/.pyenv`. If you use a different installation location you can
customize pyenv.el to search in the right place:

```lisp
(setq pyenv-installation-dir "/usr/local/pyenv")
```

*IMPORTANT:*: Currently you need to set this variable before you load pyenv.el

### the modeline
pyenv.el will show you the active python in the modeline. If you don't
like this feature you can disable it:

```lisp
(setq pyenv-show-active-python-in-modeline nil)
```

The default modeline representation is the python version (colored red) in square
brackets. You can change the format by customizing the variable:

```lisp
;; this will remove the colors
(setq pyenv-modeline-function 'pyenv--modeline-plain)
```

You can also define your own function to format the python version as you like.

You can also configure the pre/post-fix if you don't like the square brackets, for example, to set the prefix
to the python glyph using Nerd Fonts:

```lisp
(setq pyenv-modestring-prefix " ")
(setq pyenv-modestring-postfix nil)
```

### pyenv-version-alias

If using [pyenv-version-alias](https://github.com/aiguofer/pyenv-version-alias), you can enable it with:

```lisp
(setq pyenv-use-alias 't)
```

### auto-update pyenv when switching buffers

In order to automatically switch to the corresponding pyenv when switching between Python buffers, you can use [switch-buffer-functions](https://github.com/10sr/switch-buffer-functions-el) and set it up like this (note I only update when changing to a Python buffer):

```lisp
(use-package switch-buffer-functions
  :straight t
  :config
  (defun pyenv-update-on-buffer-switch (prev curr)
    (if (string-equal "Python" (format-mode-line mode-name nil nil curr))
        (pyenv-use-corresponding)))

  (add-hook 'switch-buffer-functions 'pyenv-update-on-buffer-switch))
```

### always using the right Jupyter console

If you have a jupyter kernel per pyenv version installed onto a "global" jupyter install (for example, using [pyenv-jupyter-kernel](https://github.com/aiguofer/pyenv-jupyter-kernel)), you could make sure all your shell related commands (`python-shell-send-buffer/region`) work on the right (currently active) kernel using:

```lisp
(setq python-shell-interpreter "jupyter-console"
      python-shell-interpreter-args "--simple-prompt"
      python-shell-prompt-detect-failure-warning nil)

(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter-console")

(defun my-setup-python (orig-fun &rest args)
  "Use corresponding kernel"
  (let* ((curr-python (car (split-string (pyenv/version-name) ":")))
         (python-shell-buffer-name (concat "Python-" curr-python))
         (python-shell-interpreter-args (concat "--simple-prompt --kernel=" curr-python)))
    (apply orig-fun args)))

(advice-add 'python-shell-get-process-name :around #'my-setup-python)
```

### ensure `PATH` is set correctly

By default, `pyenv.el` will set up the `PATH` to make sure the necessary `pyenv` directories (`$PYENV_ROOT/{bin,shims}`) are in your `PATH`. If Emacs has already inherited the correct `PATH`, you can disable this behavior by setting:

```lisp
(setq pyenv-set-path nil)
```

Note: this needs to be before you call `(global-pyenv-mode)`

## Running hooks on pyenv switch

You can use `pyenv-mode-hook` to do things when you change your pyenv. This can be useful for updating code completion backends. For example, you could run `elpy-rpc-restart` when you switch pyenv versions like so:

```lisp
(add-hook 'pyenv-mode-hook 'elpy-rpc-restart)
```
