Minimal Vi minor mode
=====================

[![Build Status](https://travis-ci.org/iquiw/mivi.svg?branch=dawn)](https://travis-ci.org/iquiw/mivi)
[![Coverage Status](https://coveralls.io/repos/github/iquiw/mivi/badge.svg?branch=dawn)](https://coveralls.io/github/iquiw/mivi?branch=dawn)

About
-----

MiVi is Minimal Vi minor mode, which provides basic Vi-like editing
layer on Emacs. Its behavior is based on *nvi*, while there are some
intentional differences for convenience of editing or simplicity of
implementation.

Setup
-----

### Depends ###

* undo-tree

### Configuration ###

``` emacs-lisp
(use-package mivi
  :config
  (mivi-setup))
```

Feature
-------

### Design Choice ###

* <kbd>h</kbd>, <kbd>l</kbd> across lines.
* <kbd>$</kbd> acts like <kbd>C-e</kbd> in Emacs.

### Not Implemented ###

* All ex commands <kbd>:</kbd>.
* Shift commands <kbd>&lt;</kbd>, <kbd>&gt;</kbd>.
* Buffer (in Vi-context) related functions.
* More...

