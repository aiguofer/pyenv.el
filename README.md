pyenv.el
========

use pyenv to manage your Python versions within Emacs

Installation
------------

```lisp
(add-to-list 'load-path (expand-file-name "/path/to/pyenv.el/"))
(require 'pyenv)
(global-pyenv-mode)
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

Press
-----

If you want to read more about pyenv.el check out the following links:

* [Use the right Python with emacs and pyenv](http://blog.senny.ch/blog/2013/02/11/use-the-right-python-with-emacs-and-pyenv/) by Yves Senn

[![githalytics.com alpha](https://cruel-carlota.pagodabox.com/f4c783738c250ce724df3c5b9753a786 "githalytics.com")](http://githalytics.com/senny/pyenv.el)
