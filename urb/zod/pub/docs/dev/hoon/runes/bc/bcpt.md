`$@` 
====

Wing to tile

`$@` is a natural rune that whips (normalizes) wing `p` into tile `q`.

Produces
--------

Tile: `[%bcpt p=wing q=tile]`

Accepts
------

`p` is a [`++wing`](). `q` is a [tile]().

    p@q

Examples
--------

    ~zod/try=> =+(a=97 a@cord)
    'a'
