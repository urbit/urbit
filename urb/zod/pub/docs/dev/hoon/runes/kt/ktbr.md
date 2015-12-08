`^|`
====

Converts a core to a core whose context is unreadable. Useful in
preventing type fails when replacing one core with one context
with another core with a different context.


`^|` is a natural rune that converts a [`%gold`]() core into an
[`%iron`]() core.

Produces
--------

Twig: `[%ktbr p=twig]`

Accepts
-------

`p` is a [twig]().

Tall form
---------

    ^|  p

Wide form
---------

    ^|(p)

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
