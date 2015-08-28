dottis, `.=`, %dtts
============================

Equality

`.=` is a natural rune that applies Nock 5 (equality) to determine if
the products of p and q are equivalent, and produces a loobean.

See also
--------

[Nock tutorial]()

Produces
--------

Twig: `[%dtts p=twig q=twig]`

Sample
------

`p` is a [twig](). `q` is a [twig]().

Tall form
---------

    .=  p
        q

Wide form
---------

    .=(p q)

Irregular form
--------------

    =(p q)

Examples
--------

    ~zod/try=> =(0 0)
    %.y
    ~zod/try=> =(1 2)
    %.n

Comparing two atoms is the most straightforward case of `.=`.

    ~zod/try=> =("a" [97 ~])
    %.y
    ~zod/try=> =(~nec 1)
    %.y
    ~zod/try=> =([%a 2] a/(dec 3))
    %.y
    ~zod/try=> =([%b 2] a/(dec 3))
    %.n

It's important to keep in mind that `.=` compares the atomic equivalent
of each `p` and `q`. In the first case of this example the tape `"a"` is
actually the list `[97 0]` since the ASCII code for `'a'` is 97. The
following cases serve to show similar implicit down-casts.

    /~zod/try=> =isa  |=  a=@t
                      ?:  =(a 'a')
                        'yes a'
                      'not a'
    new var %isa
    /~zod/try=> (isa 'b')
    'not a'
    /~zod/try=> (isa 'a')
    'yes a'

In common practice `.=` is often used inside of [`?`]() runes, where
switching on equality is needed. Here we construct a simple gate to test
if our sample is equal to `'a'` and produce either `'yes a'` or
`'not a'` accordingly.
