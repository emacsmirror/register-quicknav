# register-quicknav - Quickly jump to next/previous register

*Author:* tastytea <tastytea@tastytea.de><br>
*Version:* 0.1.0<br>
*URL:* [https://schlomp.space/tastytea/register-quicknav](https://schlomp.space/tastytea/register-quicknav)<br>

This package is built on top of `register.el` and allows you to quickly jump
to the next/previous position register.  If you reach the end, the search
wraps around and continues with the first (or last) register.

## Features

* Cycle through all position registers in both directions.
* Clear current register.

## Known limitations

Works only for as long as the buffer containing the registers is open.  If
you close and reopen it, it won't work anymore.

## Installation

To use `register-quicknav.el`, put it in your load-path and add the following
to your init.el:

    (require 'register-quicknav)
    (global-set-key (kbd "<C-f5>") #'register-quicknav/prev-register)
    (global-set-key (kbd "<C-f6>") #'register-quicknav/next-register)
    (global-set-key (kbd "M-r")    #'register-quicknav/clear-current-register)

Or, with use-package:

    (use-package register-quicknav
      :bind (("C-<f5>" . register-quicknav/prev-register)
             ("C-<f6>" . register-quicknav/next-register)
             ("M-r"    . register-quicknav/clear-current-register)))

Instead of manually copying `register-quicknav.el` into your load-path, you
can use [quelpa](https://github.com/quelpa/quelpa):

    (quelpa '(register-quicknav
              :fetcher git
              :url "https://schlomp.space/tastytea/register-quicknav.git"))

## Variables

* `register-quicknav/buffer-only`: Cycle only through position registers in
  current buffer.


---
Converted from `register-quicknav.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
