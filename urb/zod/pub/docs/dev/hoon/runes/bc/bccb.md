buccab `$_` %bccb
==========================

Uses a twig `p` to produce a type (mold) that is the same as the
default type (span) of `p`, except for that the default value
(bunt) of the type produced is the value of `p`, rather than the
standard default value.

`$_` produces a mold-by-example, called a `%weed`. A `%weed` contains
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
    ~zod/try=> *@
    12
    ~zod/try=> :type; *_12
    12
    @ud
    ~zod/try=> ^-(_(add 2 2) 'a')
    97

Here, see how the default value of `_12` is 12, whereas the default value of `@`--the type (span) of `12`--is `0`.
