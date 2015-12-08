`?=`
====

Tests whether `q` is of type (mold) `p`.


`?=` is a natural rune that produces true if the leg at wing `q` is in
tile `p`.

Produces
--------

Twig: `[%wtts p=tile q=wing]`

Sample
------

`p` is a [tile](). `q` is a [`++wing`]().

Tall form
---------

    ?=  p
        q

Wide form
---------

    ?=(p q)

Examples
--------

    ~zod/try=> ?=(@ 'a')
    %.y
    ~zod/try=> ?=(^ 'a')
    %.n
    ~zod/try=> ?=(%b 'a')
    %.n
    ~zod/try=> ?=(%a 'a')
    %.y
