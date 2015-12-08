`?>`
====

Asserts that `p` is true before evaluating `q`. Crashes if `p` evaluates to true.


`?>` is a synthetic rune that produces `q`, asserting that `p` is yes
(`&`, 0).

Produces
--------

Twig: `[%wtgr p=twig q=twig]`

Accepts
-------

`p` and `q` are [twig]()s.

Tall form
---------

    ?>  p
        q

Wide form
---------

    ?>(p q)

Examples
--------

    ~zod/try=> ?>(=(0x1 1) %foo)
    %foo
    ~zod/try=> ?>(=(0x1 0) %foo)
    ! exit

Equivalent to
-------------

    ?.(p !! q)
