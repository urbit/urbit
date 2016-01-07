`$:`
====

Tuple of molds

Note that this is one of the few molds that does "partial
validation".  If a value matches the first few elements of the
tuple, but not the later ones, then the validator keeps the
matches and defaults the later ones.

Produces
--------

A validator function that validates a tuple of nouns against a tuple of [mold]()s.

Accepts
--------

`p` is a list of molds.

Tall form
---------

    $:  i.p
        i.t.p
        i.t.t.p
    ==

Wide form
---------

    $:(i.p i.t.p i.t.t.p)

Irregular form
--------------

    {i.p i.t.p i.t.t.p}

Examples
--------

    ~zod/try=> *[1 2]
    [%1 %2]
    ~zod/try=> (,$:(1 2) "ham")
    [%1 %2]
    ~zod/try=> (,[1 2] "ham")
    [%1 %2]
    ~zod/try=> (,[@ 2] "ham")
    [104 %2]
