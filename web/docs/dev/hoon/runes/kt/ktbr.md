ketbar, `^|`, %ktbr
============================

Convert `%gold` to `%iron`

`^|` is a natural rune that converts a [`%gold`]() core into an
[`%iron`]() core.

See also
--------

barlus, `|+`, %brls
============================

ketpam, `^&`, %ktpm
============================

Produces
--------

Twig: `[%ktbr p=twig]`

Sample
------

`p` is a [twig]().

Tall form
---------

    ^|  p

Wide form
---------

    ^|(p)

Irregular form
--------------

None

Examples
--------

    /~zod/try=> =cor  |=  a=@
          +(a)
    new var %cor
    /~zod/try=> +<.cor
    a=0
    /~zod/try=> =iro  ^|(cor)
    new var %iro
    /~zod/try=> +<.iro
    ! -axis.6
    ! peek-park
    ! exit

Here we crete a simple gate and assign it to the shell variable `cor`.
We can examine the sample of `cor` with `+<` to produce `a=0`. Assigning
a new shell variable, `iro` as the `^|` of `cor` we can no longer peek
in to its subject.
