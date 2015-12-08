`~_`
====

Inserts pre-formatted stackframe `p` into the stacktrace of `q`. 


`~_` is a synthetic rune that inserts `p`, a trap producing `tank`, in
the trace of `q`.

Produces
--------

Twig: `[%sgcb p=twig q=twig]`

Accepts
-------

`p` is a [twig]() `q` is a [twig]()

Tall form
---------

    ~_  p
        q

Wide form
---------

    ~_(p q)

Examples
--------

    ~zod/try=> (make '~_(+216 ~)')
    [%10 p=[p=1.851.876.717 q=[p=[%1 p=[0 216]] q=[%0 p=1]]] q=[%1 p=0]]
    ~zod/try=> `@tas`1.851.876.717
    %mean
