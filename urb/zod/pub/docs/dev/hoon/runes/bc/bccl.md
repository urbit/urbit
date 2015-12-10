`$:`
====

Tuple of molds

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
