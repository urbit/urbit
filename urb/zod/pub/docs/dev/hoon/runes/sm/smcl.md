`;:`
====

Fold over tuple

Apply a binary function `p` to a tuple `q` with `n` elements.
Statically folds over the tuple `q`.

Produces
--------

Twig: `[%smcl p=twig q=tusk]`

Accepts
-------

`p` is a [`++twig`](). `q` is a [`++tusk`](), a list of twigs.

Tall form
---------

    ;:  p
      i.q
      i.t.q
      i.t.t.q
    ==

Wide form
---------

    ;:(p i.q i.t.q i.t.t.q)

Irregular form
--------------

    :(p i.q i.t.q i.t.t.q)

Examples
--------

    ~zod/try=> (add 3 (add 4 5))
    12
    ~zod/try=> ;:(add 3 4 5)
    12
    ~zod/try=> :(add 3 4 5)
    12

Here we see how `;:` is equivalent to nesting our calls to the binary
gate `++add`.

    ~zod/try=> :(weld "foo" "bar" "baz")
    ~[~~f ~~o ~~o ~~b ~~a ~~r ~~b ~~a ~~z]
    ~zod/try=> `tape`:(weld "foo" "bar" "baz")
    "foobarbaz"
    ~zod/try=> `tape`(weld "foo" (weld "bar" "baz"))
    "foobarbaz"

Following on from our previous example, using `;:` with [`++weld`]() is
convenient for concatenating multiple strings.
