buccab `$_` %bccb
==========================

Default value

`$_` produces a tile-by-example, called a `%weed`. A `%weed` contains
the twig used to make the example.

Produces
--------

Tile: `[%weed p=twig]`

Sample
------

`p` is a [tile]().

Tall form
---------

    $_  p

Wide form
---------

    $_(p)

Irregular form
--------------

    _p

Examples
--------

    ~zod/try=> *_12
    12
    ~zod/try=> :type; *_12
    12
    @ud
    ~zod/try=> ^-(_(add 2 2) 'a')
    97
