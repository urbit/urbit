`?<`
====

Assert `p` is false

Asserts that `p` is false before evaluating `q`. Crashes if `p`
evaluates to true.

Produces
--------

Twig: `[%wtgl p=twig q=twig]`

Accepts
-------

`p` and `q` are [`++twig`]()s.

Tall form
---------

    ?<  p
    q

Wide form
---------

    ?<(p q)

Examples
--------

    ~zod/try=> ?<(=(0x1 0) %foo)
    %foo
    ~zod/try=> ?<(=(0x1 1) %foo)
    ! exit

Equivalent to
-------------

    ?:(p !! q)
