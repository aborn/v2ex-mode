# v2ex-mode
[![Build Status](https://travis-ci.org/aborn/v2ex-mode.svg?branch=master)](https://travis-ci.org/aborn/v2ex-mode)
[![MELPA](https://melpa.org/packages/v2ex-mode-badge.svg)](https://melpa.org/#/v2ex-mode)
[![MELPA Stable](http://stable.melpa.org/packages/v2ex-mode-badge.svg)](http://stable.melpa.org/#/v2ex-mode)  
v2ex-mode, visiting [http://v2ex.com/](http://v2ex.com/) freely in emacs.

## Install
Install it from elpa package source (i.e. [melpa](https://melpa.org/) or [popkit elpa](https://elpa.popkit.org/)).  
```elisp
M-x package-install RET v2ex-mode RET
```

Or, you can install it manually, download **v2ex-mode.el** file to **\<your-local-directory>**, and
and add-to-list load-path and load it.  
```elisp
(add-to-list 'load-path "<your-local-directory>")
(load "v2ex-mode")
```

## Usage
```elisp
M-x v2ex
```

## Hotkey
In \*v2ex* buffer, following hot-key are supported:  
**H** -- fetch the hot topics;  
**L** -- fetch the latest topics;  
**r** -- reload the current content;  
**q** -- exit/quit kill & quit \*v2ex* buffer.

## Vim-like move
In v2ex-mode, vim-like *hjkl* hot-key are supported, which used for move cursor
swiftly.

## Show
The content list are shown in \*v2ex* buffer.  
![](doc/v2ex.png "v2ex conent list")
