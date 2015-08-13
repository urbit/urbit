wutzap, `?!`, %wtzp
============================

NOT

`?!` is a synthetic rune that produces the logical "not" of `p`.

Produces
--------

Twig: `[%wtzp p=twig]`

Sample
------

`p` is a [twig]().

Tall form
---------

    ?!  p

Wide form
---------

    ?!(p)

Irregular form
--------------

    !p

Examples
--------

    ~zod/try=> !&
    %.n
    ~zod/try=> !|
    %.y
    ~zod/try=> (gth 5 6)
    %.n
    ~zod/try=> !(gth 5 6)
    %.y
    ~zod/try=> !1
    ! type-fail
    ! exit
