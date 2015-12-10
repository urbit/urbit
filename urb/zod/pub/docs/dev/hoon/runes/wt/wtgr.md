`?>`
====

Assert `p` is true

Asserts that `p` is true before evaluating `q`. Crashes if `p` evaluates to true.

Produces
--------

Twig: `[%wtgr p=twig q=twig]`

Accepts
-------

`p` and `q` are [`++twig`]()s.

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
