eprime-mode
===========

A mode that informs users of words not conforming to E' in Emacs Lisp.


* Installation

Simply do `M-x package-refresh-contents` followed by `M-x package-install eprime-mode`, and everything should work out of the box.

You can find eprime-mode on both Marmalade and MELPA.

#### Setting up packages in Emacs 24

Simply add

`(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))`

To your .emacs.

#### Setting up packages in Emacs 23 or lower

Use something like this -

`(require 'package)
;; Any add to list for package-archives (to add marmalade or melpa) goes here
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)`

For more information, please see [the Emacs wiki](http://www.emacswiki.org/emacs/ELPA).

