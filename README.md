# Minimal Vi minor mode

[![Build Status](https://travis-ci.org/iquiw/mivi.svg?branch=master)](https://travis-ci.org/iquiw/mivi)
[![Coverage Status](https://coveralls.io/repos/github/iquiw/mivi/badge.svg?branch=master)](https://coveralls.io/github/iquiw/mivi?branch=master)

## About

MiVi is Minimal Vi minor mode, which provides basic Vi-like editing layer on
Emacs. Its behavior is based on *nvi*, while there are some intentional
differences for convenience of editing or simplicity of implementation.

## Setup

### Depends

* Emacs 26
* undo-tree

### Configuration

With [use-package](https://github.com/jwiegley/use-package),

``` emacs-lisp
(use-package mivi
  :config
  (mivi-setup))
```

In default, `mivi-mode` is enabled on `fundamental-mode` or any derived mode
of `conf-mode`, `prog-mode` and `text-mode`.
To enable `mivi-mode` in a major-mode:

``` emacs-lisp
(add-to-list 'mivi-enabled-major-modes 'some-mode)
```

To enable `mivi-mode` in any mode derived a major-mode:

``` emacs-lisp
(add-to-list 'mivi-enabled-derived-modes 'some-mode)
```

## Feature

### Design Choice

* <kbd>h</kbd>, <kbd>l</kbd> across lines.
* <kbd>u</kbd> acts like *nvi*, that is, it undo and redo alternately.
  To repeat undo, press <kbd>.</kbd> after <kbd>u</kbd>.
* <kbd>$</kbd> acts like <kbd>C-e</kbd> in Emacs.
* Change <kbd>c</kbd> acts as delete and insert, not as overwrite like *nvi*.
* <kbd>:s</kbd> is implemented by `replace-match`. As a result, matched text
  replacement is `\&` instead of `&`.
* `viper-mode` inserts `\n` at the end of buffer always even if `require-final-newline` is nil.
  `mivi-mode` does not do it. Instead, it appends `\n` to copied text
  when the last line is copied and it does not contain `\n`.

### Enhancement

* In command state, searched text by <kbd>/</kbd>, etc. is highlight'ed with overlay.
  <kbd>C-w</kbd>/<kbd>M-w</kbd> kills/copies the searched text if the overlay is active.
* <kbd>C-a</kbd> searches the current word as *nvi*. Additionally,
  * It searches string in region if region is active.
  * It searches `isearch-string` if isearch is running.

### Not Implemented

* All ex commands <kbd>:</kbd> except for the following commands.
  * <kbd>:&</kbd>, only global flag is supported.
  * <kbd>:d</kbd>
  * <kbd>:g</kbd>, combination with `d` and `s` commands.
  * <kbd>:s</kbd>, only global flag is supported.
  * <kbd>:y</kbd>
  * <kbd>:v</kbd>, combination with `d` and `s` commands.
* Shift commands <kbd>&lt;</kbd>, <kbd>&gt;</kbd>.
* Buffer (in Vi-context) related functions.
* More...

## Note

### Key binding

The following keys are overridden against Emacs Key Binding Convention.

* <kbd>C-h</kbd>: In insert state, it is bound to `backward-char`.
* <kbd>\e</kbd>: On tty (`emacs -nw`), it is used to catch escape character
  in `input-decode-map`.

## License

Licensed under the GPL 3+ license.
