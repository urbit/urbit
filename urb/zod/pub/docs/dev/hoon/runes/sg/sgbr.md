`~|`
====

`p` in stack trace if `q` crashes

Presents the product of `p` in the stack trace if `q` crashes. `p` is only
evaluated if `q` crashes.

Produces
--------

Twig: `[%sgbr p=twig q=twig]`

Accepts
-------

`p` is a [`++twig`](). `q` is a twig.

Tall form
---------

    ~|  p
        q

Wide form
---------

    ~|(p q)
