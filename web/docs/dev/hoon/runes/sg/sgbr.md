sigbar, `~|`, %sgbr
============================

Add to stacktrace

`~|` is a synthetic rune that presents the product of `p` in the stack
trace if `q` crashes. The computation is only performed if needed.

Produces
--------

Twig: `[%sgbr p=twig q=twig]`

Sample
------

`p` is a [twig](). `q` is a [twig]().

Tall form
---------

    ~|  p
        q

Wide form
---------

    ~|(p q)

Irregular form
--------------

None

Examples
--------
