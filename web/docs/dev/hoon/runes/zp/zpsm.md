zapsem, `!;`, %zpsm
============================

`[type noun]` pair

`!;` is a natural rune that produces the product of twig `q` as a
`[type noun]` pair, with twig `p` serving as an example of the type.

Produces
--------

Twig: `[%zpsm p=twig q=twig]`

Sample
------

`p` and `q` are [twig]()s.

Tall form
---------

    !;  p
        q

Wide form
---------

    !;(p q)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> !;(*type 1)
    [[%atom p=%ud] 1]
    ~zod/try=> !;(*type [1 2])
    [[%cell p=[%atom p=%ud] q=[%atom p=%ud]] 1 2]
    ~zod/try=> !;([%atom ''] ~doznec)
    [[%atom 'p'] ~doznec]
