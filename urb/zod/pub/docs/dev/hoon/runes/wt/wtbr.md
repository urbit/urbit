`?|`
====

Computes the logical 'or' operation on a list of boolean expressions `p`.

Produces
--------

Twig: `[%wtbr p=tusk]`

Accepts
-------

`p` is a [`++tusk`](), a list of [twig]()s.

Tall form
---------

    ?|  i.p
        i.t.p
        i.t.t.p
    ==

Wide form
---------

    ?|(i.p i.t.p i.t.t.p)

Irregular form
--------------

    |(i.p i.t.p i.t.t.p)

Examples
--------

    ~zod/try=> ?|(& |)
    %.y
    ~zod/try=> |(& |)
    %.y
    ~zod/try=> |(| |)
    %.n
    ~zod/try=> (gth 2 1)
    %.y
    ~zod/try=> |((gth 2 1) |)
    %.y
    ~zod/try=> |((gth 1 2) |)
    %.n
    ~zod/try=> |((gth 2 1) &)
    %.y
    ~zod/try=> |((gth 1 2) &)
    %.y
