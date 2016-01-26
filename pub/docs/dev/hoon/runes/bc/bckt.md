bucket `$^` %bckt
==========================

Normalizing gate, `%herb`

`$^`, is a tile rune that declares an [`%herb`](). An `%herb` is a gate,
accepting a sample of \* and normalizing it as you choose. If you use a
twig as a tile, it's treated as an herb.

Produces
--------

Tile: `[%herb p=twig]`

Sample
------

`p` is a [twig]().

Tall form
---------

    $^  p

Wide form
---------

    $^(p)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> *$^(cord)
    ''
    ~zod/try=> *$^(|=(a=* ?^(a a [~ a])))
    [~ 0]
    ~zod/try=> `$^(|=(a=* ?^(a a [~ a])))`1
    ! type-fail
    ! exit
    ~zod/try=> `$^(|=(a=* ?^(a a [~ a])))`[1 2]
    [1 2]
    ~zod/try=> :type; *$^(|=(a=* ?^(a a [~ a])))
    [~ 0]
    {[%~ @] [* *]}
