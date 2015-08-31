tisgal, `=<`, %tsgl
============================

Product as subject, inverse

`=<` is a synthetic rune that uses the product of `q` as the subject of
`p`. `=<` is the inverse of `=>`. Use `=<` when your computation, `q` is
lengthier and `=<` makes for more readable code.

See also
--------

tisgar, `=>`, %tsgr
============================

Produces
--------

Twig: `[%tsgl p=twig q=twig]`

Sample
------

`p` and `q` are [twig]()s.

Tall form
---------

    =<  p
        q

Wide form
---------

    =<(p q)

Irregular form
--------------

    p:q

Examples
--------

    ~zod/try=> b:[a=1 b=2 c=3]
    2
    ~zod/try=> [. .]:(add 2 4)
    [6 6]

In this simple example we first produce `b` from the tuple
`[a=1 b=2 c=3]` using the irregular form of `=<`. Then we use `.` to
produce our context from the computation `(add 2 4)` as a cell, `[6 6]`.

    ~zod/try=> =<  lom
               |%
               ++  lom  (add 2 tak)
               ++  tak  4
               --
    6

This example is a more common case, where we want to pull some specific
value out of a longer computation. Here we use the tall form of `=<` to
pull the arm `lom` from the core created with [`|%`]().
