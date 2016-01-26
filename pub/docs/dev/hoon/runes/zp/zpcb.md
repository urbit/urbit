zapcap, %zpcb
======================

Path / location trace

`!_` is a virtual natural rune that traces the stack if debugging is
enabled, putting the current path and location range of each rune.

Produces
--------

Twig: `[%zpcb p=spot q=twig]`

Sample
------

`p` is a [`++spot`](). `q` is a [twig]().

Tall form
---------

    ?!  p

Wide form
---------

    ?!(p)

Irregular form
--------------

    !p

Examples
--------

    ~zod/try=> (ream '20')
    [%dtzy p=%ud q=20]
    ~zod/try=> (ream '!:(20)')
    [%zpcb p=[p=/ q=[p=[p=1 q=4] q=[p=1 q=6]]] q=[%dtzy p=%ud q=20]]
    ~zod/try=> (ream '=+(20 ~)')
    [%tsls p=[%dtzy p=%ud q=20] q=[%bczp p=%null]]
    ~zod/try=> (ream '!:(=+(20 ~))')
    [ %zpcb
      p=[p=/ q=[p=[p=1 q=4] q=[p=1 q=12]]]
        q
      [ %tsls
        p=[%zpcb p=[p=/ q=[p=[p=1 q=7] q=[p=1 q=9]]] q=[%dtzy p=%ud q=20]]
        q=[%zpcb p=[p=/ q=[p=[p=1 q=10] q=[p=1 q=11]]] q=[%bczp p=%null]]
      ]
    ]
    ~zod/try=> =+(20 !!)
    ! exit
    ~zod/try=> !:(=+(20 !!))
    ! /~zod/try/~2014.10.31..21.11.11..be6c/:<[1 4].[1 13]>
    ! /~zod/try/~2014.10.31..21.11.11..be6c/:<[1 10].[1 12]>
    ! exit
