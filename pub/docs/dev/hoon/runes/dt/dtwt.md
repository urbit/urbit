dotwut,`.?`, %dtwt
===========================

Noun or cell

`.?`, `dotwut`, `[%dtwt p=twig]` is a natural rune that applies nock `3`
to a noun: if the noun is a cell, it returns the loobean `&` (true); if
the noun is an atom, it returns the loobean `|` (false).

Produces
--------

Twig: `[%dtwt p=twig]`

Sample
------

`p` is a [twig]().

Tall form
---------

    .?  p

Wide form
---------

    .?(p)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> .?(~)
    %.n
    ~zod/try=> .?(5)
    %.n
    ~zod/try=> .?(~porlep)
    %.n

In all of these cases our sample is implicitly down-cast to an atom,
which produces `|`.

    ~zod/try=> .?([1 2 3])
    %.y
    ~zod/try=> .?("ha")
    %.y
    ~zod/try=> ._a_b__
    [%a %b]
    ~zod/try=> .?(._a_b__)
    %.y

`[1 2 3]` is clearly a cell, `"ha"` is equivalent to the null-terminated
tuple of its ASCII codes, and `._a_b__` is also clearly a cell. Each
produce `&`.
