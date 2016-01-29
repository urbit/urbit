wuttis, `?=`, %wtts
============================

Is in tile

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

Irregular form
--------------

None

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
