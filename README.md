# v2ex-mode
v2ex-mode, visiting http://v2ex.com/ using emacs.

## Install
Install it from elpa package source (i.e. [melpa](https://melpa.org/) or [popkit elpa](https://elpa.popkit.org/)).  
```elisp
M-x package-install v2ex-mode
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

## Show
The content list are shown in \*v2ex* buffer.  
![](doc/v2ex.png "v2ex conent list")
