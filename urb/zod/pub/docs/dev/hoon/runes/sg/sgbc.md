`~$`
====

Label for profiling

Labels computation `q` as `p` for profiling.

Produces
--------

Twig: `[%sgbc p=term q=twig]`

Accepts
-------

`p` is a [`++term`](). `q` is a [`++twig`]().

Tall form
---------

    ~$  p
    q

Wide form
---------

    ~$(p q)

Examples
--------

    ~zod/try=> (make '~$(foo |-($))')
    [%10 p=[p=1.702.259.052 q=[%1 p=7.303.014]] q=[%8 p=[%1 p=[9 2 0 1]] q=[%9 p=2 q=[%0 p=1]]]]
    ~zod/try=> `@tas`1.702.259.052
    %live
    ~zod/try=> `@tas`7.303.014
    %foo
