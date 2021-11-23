# eprime-mode - An E′ checking mode for Emacs

_Version:_ 1.2.0<br>

A minor mode for Emacs informing users of words not conforming to E′, as you type or on demand.

## Installation

Install manually, or using a library like `straight.el`:

       (use-package eprime-mode
        :straight (:host gitlab :repo "thornjad/eprime-mode" :branch "main")
        :hook ((text-mode) . eprime-mode))

## E prime? What?

E′ (or E-prime) refers to a subset of the English language excluding all forms of the verb "to
be". Such a practice may strengthen writing skills and may provide a path for greater mindfulness
in writing. Only shaky evidence really exists, but it can provide some fun and challenge.

Check out the [the Wikipedia page](https://en.wikipedia.org/wiki/E-Prime) to learn more.

## License

Copyright (C) 2020-2021 Jade Michael Thornton\
Copyright (C) 2014 Andrew Hynes

This program is free software; you may redistribute it and/or modify it under the terms of the
GNU General Public License version 3 only, as published by the Free Software Foundation. This
program carries no warranty whatsoever, without even the implied warranty of merchantability or
fitness for a particular purpose. See <https://www.gnu.org/licenses/> for more details.


---
Converted from `eprime-mode.el` by [_el2md_](https://gitlab.com/thornjad/el2md).
