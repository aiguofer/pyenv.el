pyenv.el
========

use pyenv to manage your Python versions within Emacs

pyenv.el was forked from [rbenv.el](https://github.com/senny/rbenv.el)

Installation
------------

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

Usage
-----

* `global-pyenv-mode` activate / deactivate pyenv.el (The current Python version is shown in the modeline)
* `pyenv-use-global` will activate your global python
* `pyenv-use` allows you to choose what python version you want to use
* `pyenv-use-corresponding` searches for .python-version and activates
  the corresponding python

Configuration
-------------

**pyenv installation directory**
By default pyenv.el assumes that you installed pyenv into
`~/.pyenv`. If you use a different installation location you can
customize pyenv.el to search in the right place:

```lisp
(setq pyenv-installation-dir "/usr/local/pyenv")
```

*IMPORTANT:*: Currently you need to set this variable before you load pyenv.el

**the modeline**
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

### Auto-update Pyenv

In order to automatically switch to the corresponding pyenv when switching between Python buffers, you can use [switch-buffer-functions](https://github.com/10sr/switch-buffer-functions-el) and set it up like this:

```lisp
(use-package switch-buffer-functions
  :straight t
  :config
  (add-hook 'switch-buffer-functions 'pyenv-update-on-buffer-switch))
```

## Running hooks on pyenv switch

You can use `pyenv-mode-hook` to do things when you change your pyenv. This can be useful for updating code completion backends. For example, you could run `elpy-rpc-restart` when you switch pyenv versions like so:

```lisp
(add-hook 'pyenv-mode-hook 'elpy-rpc-restart)
```
