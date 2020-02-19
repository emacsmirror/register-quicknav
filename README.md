# register-quicknav - Quickly jump to next/previous register

*Author:* tastytea <tastytea@tastytea.de><br>
*Version:* 0.1.0<br>

This package is built on top of register.el and allows you to quickly jump to
the next/previous position register.  If you reach the end, the search wraps
around and continues with the first (or last) register.

## Features

* Cycle through all position registers in both directions.
* Clear current register.

## Known limitations

Works only for as long as the buffer containing the registers is open.  If
you close and reopen it, it won't work anymore.

## Installation

To use `register-quicknav.el`, put it in your load-path and add the following
to your .emacs:

    (require 'register-quicknav)
    (global-set-key (kbd "<C-f5>") 'register-quicknav/prev-register)
    (global-set-key (kbd "<C-f6>") 'register-quicknav/next-register)
    (global-set-key (kbd "M-r")    'register-quicknav/clear-current-register)

Or, with use-package:

    (use-package register-quicknav
      :commands (register-quicknav/prev-register
                 register-quicknav/next-register)
      :bind (("C-<f5>" . register-quicknav/prev-register)
             ("C-<f6>" . register-quicknav/next-register)
             ("M-r"    . register-quicknav/clear-current-register)))


---
Converted from `register-quicknav.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
