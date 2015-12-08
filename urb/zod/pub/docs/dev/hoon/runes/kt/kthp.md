`^-`
====

Casts `q` to the type (aka mold) of `p`. The same as
`^+`, except for that `q` is actually cast to the bunt of `q`
(`$,(q)`). The easiest way to make a basic cast. Used when you
already have already created a validator function (clam) from a
type (mold). 


`^-` is a synthetic rune that casts `q` to `~(bunt al p)`, i.e. the icon
of `p`.

Produces
--------

Twig: `[%kthp p=tile q=twig]`

Accepts
-------

`p` is a [tile](). `q` is a [twig]().

Tall form
---------

    ^-  p
        q

Wide form
---------

    ^-(p q)

Irregular form
--------------

    `p`q

Examples
--------

    ~zod/try=> (add 90 7)
    97
    ~zod/try=> `@t`(add 90 7)
    'a'
    ~zod/try=> ^-(@t (add 90 7))
    'a'

Here we see a straightforward use of `^-` in both irregular and wide
form, simply casting an atom to a char. `97` is the ASCII code for
`'a'`.

    /~zod/try=> =cor  |=  a=@
          ^-  (unit ,@ta)
          [~ u=a]
    new var %cor
    /~zod/try=> (cor 97)
    [~ ~.a]

In this case we see a very common use of `^-`, at the top of a [gate]().
This pattern is considered good hoon style for two reasons: it gives the
reader a clear pattern for understanding what your code produces, and it
helps ensure type-safety.
