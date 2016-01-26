ketdot, `^.`, %ktdt
============================

Cast to product type

`^.` is a synthetic rune that casts `q` to the type of `(p q)`. `^.` is
the same as casting `q` to the product type of `p` and comes in handy
when you don't want to run the contents of `p`.

Produces
--------

Twig: `[%ktdt p=twig q=twig]`

Sample
------

`p` is a [twig](). `q` is a [twig]().

Tall form
---------

    ^.  p
        q

Wide form
---------

None

Irregular form
--------------

None

Examples
--------

    /~zod/try=> =cor  |=  [~ a=@]
          [~ p=a]
    changed %cor
    /~zod/try=> ^.(cor [~ 97])
    [~ p=97]

In this example we create a gate `cor` that takes a cell of `~` and an
atom and produces `[~ p=a]`. Using `^.` we can cast without calling
`cor` and produce the same result.
