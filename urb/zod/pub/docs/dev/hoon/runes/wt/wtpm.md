wutpam, `?&`, %wtpm
============================

AND

`?&` is a synthetic rune that computes the "and" of the loobeans in `p`.
`?&` is commonly used as a control-flow mechanism.

Produces
--------

Twig: `[%wtpm p=tusk]`

Sample
------

`p` is a [`++tusk`](), a list of [twig]()s.

Tall form
---------

    ?&  i.p
        i.t.p
        i.t.t.p
    ==

Wide form
---------

    ?&(i.p i.t.p i.t.t.p)

Irregular form
--------------

    &(i.p i.t.p i.t.t.p)

Examples
--------

    ~zod/try=> ?&(& &)
    %.y
    ~zod/try=> &(& &)
    %.y
    ~zod/try=> &(& |)
    %.n
    ~zod/try=> &((gth 2 1) |)
    %.n
    ~zod/try=> &((gth 2 1) &)
    %.y
