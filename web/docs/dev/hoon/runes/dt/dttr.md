dottar, `.*`, %dttr
============================

Nock

`.*` is a natural rune that calculates the nock of subject `p` and
formula `q`. `.*` makes it possible to compute nock formulas in user
space.

See also
--------

[Nock tutorial]()

Produces
--------

Twig: `[%dttr p=twig q=twig]`

Sample
------

`p` is a [twig](). `q` is a [twig]().

Tall form
---------

    .*  p
    q

Wide form
---------

    .*(p q)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> .*([20 30] [0 2])
    20
    ~zod/try=> .*(33 [4 0 1])
    34
    ~zod/try=> .*(|.(50) [9 2 0 1])
    50
    ~zod/try=> .*(12 [7 [`1 [4 `1]] [`2 `3 `2]])
    [12 13 12]
    ~zod/try=> .*(~ [5 1^4 [4 1^3]])
    0
    ~zod/try=> .*(~ [5 1^5 [4 1^3]])
    1

See the [nock tutorial]() for further discussion of Nock.
