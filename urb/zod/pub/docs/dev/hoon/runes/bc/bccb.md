`$_`
====

Unbunt (any noun to specific value)

Uses a [++twig]() `p` to produce a [mold]() that takes an arbitrary noun and produces the value of `p`. Used when you want the default value of a mold to be anything other than the standard default value ([bunt]()).

Produces
--------

A validator function that takes an arbitrary noun and produces the value of `p`.

Accepts
-------

`p` is a twig.

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
    0
    ~zod/try=> :type; *_12
    12
    @ud
    ~zod/try=> ^-(_(add 2 2) 'a')
    97

Here, see how the default value of `_12` is 12, whereas the default value of `@`--the type ([span]()) of `12`--is `0`.
