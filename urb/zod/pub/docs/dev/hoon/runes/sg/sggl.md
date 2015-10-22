siggal, `~<`, %sggl
============================

Hint to product

`~<` is a synthetic rune that applies arbitrary [hint]() `p` to the
product of `q`. `~<` is similar to `~>`, but computes `q` before
applying the hint `p`.

Produces
--------

Twig: `[%sggl p=$|(term [p=term q=twig]) q=twig]`

Sample
------

`p` is either a [`++term`]() or a `++term` twig pair. `q` is a [twig]()

Tall form
---------

`p=%foo`:

        ~<  %foo
        q

`p=[p=%foo q=bar]`:

        ~<  %foo.bar
        q

Wide form
---------

    ~<(%foo q)
    ~<(%foo.bar q)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> (make '~<(%a 42)')
    [%7 p=[%1 p=42] q=[%10 p=97 q=[%0 p=1]]]
    ~zod/try=> (make '~<(%a.+(.) 42)')
    [%7 p=[%1 p=42] q=[%10 p=[p=97 q=[%4 p=[%0 p=1]]] q=[%0 p=1]]]
