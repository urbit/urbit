buccol `$:` %bccl
==========================

Mold autocons. Creates a type of an array of types.

`$:` is a mold rune that constructs the mold of a tuple from a
tuple of molds.

Produces
--------

[mold](): `[p=mold q=mold]`

Children
-------

`p` is a [mold](). `q` is a [mold]().

Tall form
---------

    $:  p
        q
    ==

Wide form
---------

    $:(p q)

Irregular form
--------------

    [p q]

Examples
--------

    ~zod/try=> *[1 2]
    [%1 %2]
    ~zod/try=> (,$:(1 2) "ham")
    [%1 %2]
    ~zod/try=> (,[1 2] "ham")
    [%1 %2]
    ~zod/try=> (,[@ 2] "ham")
    [104 %2]
