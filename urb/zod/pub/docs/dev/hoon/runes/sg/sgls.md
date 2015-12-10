`~+`
====

Memoize computation

Hints to the interpreter to memoize (cache) the computation of `p`.

Produces
--------

Twig: `[%sgls p=twig]`

Accepts
-------

`p` is a [`++twig`]()

Tall form
---------

    ~+  p

Wide form
---------

    ~+(p)

Examples
--------

    ~zod/try=> 20
    20
    ~zod/try=> ~+(20)
    20
    ~zod/try=> 20
    ~zod/try=> (make '20')
    [%1 p=20]
    ~zod/try=> (make '~+(20)')
    [%10 p=[p=1.869.440.365 q=[%1 p=0]] q=[%1 p=20]]
    ~zod/try=> `@tas`1.869.440.365
    %memo

By using [`++make`]() to display the compiled nock, we can see that `~+`
inserts a `%memo` hint.
