`$&` 
====

Produces a type (mold) that's a union between two different kinds of noun `p` and `q`, cells whose head is a cell and cells whose head is an atom. Its default value (bunt) is the value of `q`. One important use of `$&` is to implement autocons in [`++twig`]() and [`++tile`]().


`$&` is a tile rune that produces a `%bush`. A `%bush` is a tile whose
[icon]() is a [fork]() between two different kinds of nouns: cells whose
head is a cell and cells whose head is an atom `(tile q)`. Its default
value is the value of `q`. One important use of `$&` is to implement
autocons in [`++twig`]() and [`++tile`]().

Produces
--------

[Tile](): `[%bush p=tile q=tile]`

Accepts
-------

`p` is a [tile]().

Tall form
---------

    $&  p
        q

Wide form
---------

    $&(p q)

Examples
--------
