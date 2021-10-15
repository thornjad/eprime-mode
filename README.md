# eprime-mode

A mode that informs users of words not conforming to E' for Emacs.

## Installation

This package has not been added to MELPA. Manually install, or use `use-package` with straight.el:

       (use-package eprime-mode
         :straight (:host gitlab :repo "thornjad/eprime-mode" :branch "main")
         :hook ((text-mode) . eprime-mode))

## E-prime? What?...

D. David Bourland, Jr., proposed an improvement to English called E-prime (or E'), which doesn't allow forms of "to be", aiming to remedy some of its problems (for example, E' avoids subjectivity and prefers objectivity). Since then, many scholars have picked up the idea and debated hotly over its usefulness (or lack thereof).

Some argue it can clarify thoughts and strengthen writing, but I won't decide for you! Regardless of whether you think it could wind up resulting in something beneficial if everyone flipped to E' from English, I find it a fun game, if nothing else.

To find out more, please see [the Wikipedia page](https://en.wikipedia.org/wiki/E-Prime).

## Copying

Copyright (C) 2020-2021 Jade Michael Thornton\
Copyright (C) 2014 Andrew Hynes

This program is free software; you may redistribute it and/or modify it under the
terms of the GNU General Public License version 3, as published by the Free Software
Foundation. This program carries no warranty whatsoever, without even the implied
warranty of merchantability or fitness for a particular purpose. See
<https://www.gnu.org/licenses/> for more details.



