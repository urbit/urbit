`=|`
====

Produces the default value of input type (aka mold) `p` and makes it
available/pushes it onto `q`.


`=|` is a synthetic rune that pushes the [bunt](), or default value, of
`p` on the subject and sends it to `q`.

Produces
--------

Twig: `[%tsbr p=mold q=twig]`

Accepts
-------

`p` is a [mold](). `q` is a [twig]().

Tall form
---------

    =|  p
        q

Wide form
---------

    =|(p q)

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
