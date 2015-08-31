barlus, `|+`, %brls
============================

`%iron` gate

`|+` is a synthetic rune that produces a [dry]() [`%iron`]() [gate]()
with sample [`$*(p)`]() and [arm]()s `q`. `|+` is similar to `|=`, but
differs in that its sample cannot be read. It can be thought of as
similar to a private function.

See also
--------

bartis, `|=`, %brts

Produces
--------

Twig: `[%brls p=tile q=twig]`

Sample
------

`p` is a [tile](). `q` is a [twig]().

Tall form
---------

    |+  p
        q

Wide form
---------

    |+(p q)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> +<:|+(a=@ a)
    ! -axis.6
    ! peek-park
    ! exit
    ~zod/try=> +<:|=(a=@ a)
    a=0

Here we're trying to read the sample, using `+<` of two different kinds
of gates. With `|+` you can see we cause an error, whereas with `|=` our
default sample is `a=0`.

    ~zod/try=> %.(20 |+(a=@ a))
    20
    ~zod/try=> %.(20 |=(a=@ a))
    20
    ~zod/try=> %.(20 |+(a=@ (add a 12)))
    32

Kicking a `|+` gate, however, is the same as `|=`.
