wutgar, `?>`, %wtgr
============================

Assert yes

`?>` is a synthetic rune that produces `q`, asserting that `p` is yes
(`&`, 0).

See also
--------

wutgal, `?<`, %wtgl
============================

Produces
--------

Twig: `[%wtgr p=twig q=twig]`

Sample
------

`p` and `q` are [twig]()s.

Tall form
---------

    ?>  p
        q

Wide form
---------

    ?>(p q)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> ?>(=(0x1 1) %foo)
    %foo
    ~zod/try=> ?>(=(0x1 0) %foo)
    ! exit

Equivalent to
-------------

    ?.(p !! q)
