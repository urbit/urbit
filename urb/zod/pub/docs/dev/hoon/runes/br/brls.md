`|+`
====

Function with unreadable sample

`|+` is a synthetic rune that produces a [dry]() [`%iron`]()
function with argument (aka [sample]()) [`$*(p)`]() and [arm]()s
`q`. `|+` is similar to `|=`, but differs in that its input
(sample) and context cannot be read.

Because of this, the type of a `|+` function is fairly general.
A gate may be defined in a different context or have a more
general input type while still being within the type.  One of the
most common usages of iron functions is as input to another
function, where `$+` is used in the type definition.

Produces
--------

Twig: `[%brls p=mold q=twig]`

Accepts
-------

`p` is a [mold](). `q` is a [`++twig`]().

Tall form
---------

    |+  p
    q

Wide form
---------

    |+(p q)

Examples
--------

    ~zod/try=> +<:|+(a=@ a)
    ! -axis.6
    ! peek-park
    ! exit
    ~zod/try=> +<:|=(a=@ a)
    a=0

Here we're trying to read the sample, using the [head]() of the
[tail]() of two different kinds of functions. With `|+` you can
see we cause an error, whereas with `|=` our default sample is
`a=0`.

    ~zod/try=> %.(20 |+(a=@ a))
    20
    ~zod/try=> %.(20 |=(a=@ a))
    20
    ~zod/try=> %.(20 |+(a=@ (add a 12)))
    32

Kicking a `|+` gate, however, is the same as `|=`.
