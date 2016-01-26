buccol `$:` %bccl
==========================

Tile autocons

`$:` is a tile rune that constructs the tile of a tuple from a tuple of
tiles.

Produces
--------

[Tile](): `[p=tile q=tile]`

Sample
------

`p` is a [tile](). `q` is a [tile]().

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
    ~zod/try=> (,[1 2] "ham")
    [%1 %2]
    ~zod/try=> (,[@ 2] "ham")
    [104 %2]
