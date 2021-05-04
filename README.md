Side Hustle
===========

Hustle through a buffer's Imenu in a side window in GNU Emacs.


Installation
------------

Add somethign like this to your init file:

    (define-key (current-global-map) (kbd "M-s l") #'side-hustle-toggle)


Bugs and Feature Requests
-------------------------

Send me an email (address in the package header). For bugs, please
ensure you can reproduce with:

    $ emacs -Q -l side-hustle.el

Known issues are tracked with `FIXME` comments in the source.


Alternatives
------------

[Imenu-List](https://github.com/bmag/imenu-list)
