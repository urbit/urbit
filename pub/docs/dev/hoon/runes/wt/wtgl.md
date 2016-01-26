wutgal, `?<`, %wtgl
============================

Assert no

`?<` is a synthetic rune that produces `q`, asserting that `p` is no
(`|`, 1).

See also
--------

wutgar, `?>`, %wtgr
============================

Produces
--------

Twig: `[%wtgl p=twig q=twig]`

Sample
------

`p` and `q` are [twig]()s.

Tall form
---------

    ?<  p
        q

Wide form
---------

    ?<(p q)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> ?<(=(0x1 0) %foo)
    %foo
    ~zod/try=> ?<(=(0x1 1) %foo)
    ! exit

Equivalent to
-------------

    ?:(p !! q)
