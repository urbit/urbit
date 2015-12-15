`$_`
====

Unbunt (create mold from example)

Uses a [++twig]() `p` to produce a [mold]() for the type (span)
of `p`.  The validator function has a default value of `p`. Used
when either (1) you want the default value of a mold to be
anything other than the standard default value ([bunt]()), or (2)
you are referring to a type (span) that is difficult or
cumbersome to describe in any way other than by example.

Produces
--------

A mold whose span is the span of `p`.

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

Here, see how the default value of `_12` is 12, whereas the
default value of `@`--the type ([span]()) of `12`--is `0`.
