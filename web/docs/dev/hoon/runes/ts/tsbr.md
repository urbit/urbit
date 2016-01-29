tisbar, `=|`, %tsbr
============================

Bunt to subject

`=|` is a synthetic rune that pushes the [bunt](), or default value, of
`p` on the subject and sends it to `q`.

Produces
--------

Twig: `[%tsbr p=tile q=twig]`

Sample
------

`p` is a [tile](). `q` is a [twig]().

Tall form
---------

    =|  p
        q

Wide form
---------

    =|(p q)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> =|(a=@p a)
    ~zod

Here we use `=|` to generate the default value of a `@p`, which is
`~zod`.

    /~zod/try=> 
        =|  a=[b=@ c=@]
        [+(b.a) +(+(c.a))]
    [1 2]

Here we add the bunt of `a=[b=@ c=@]` and perform a very basic operation
on it.

Equivalent to
-------------

    =+(*p q)
