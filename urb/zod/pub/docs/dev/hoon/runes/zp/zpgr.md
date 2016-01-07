`!>`
====

Produce [type value]

Produces a cell (known as a [`++vase`]() here) of both the type
and value of `p`.

Uses biblical arms [`++onan`](), [`++abel`](). 

Produces
--------

Twig: `[%zpgr p=twig]`

Accepts
-------

`p` is a [`++twig`]().

Tall form
---------

    !>  p

Wide form
---------

    !>(p)

Examples
--------

    ~zod/try=> !>(1)
    [p=[%atom p=%ud] q=1]
    ~zod/try=> !>(~zod)
    [p=[%atom p=%p] q=0]

In these simple examples we see constant type information printed. `1`
and `~zod` are shown to be odored atoms of `%ud` and `%p` respectively.

    ~zod/try=> !>([1 2])
    [p=[%cell p=[%atom p=%ud] q=[%atom p=%ud]] q=[1 2]]
    ~zod/try=> !>([|.(20)]:~)
    [   p
      [ %core
        p=[%cube p=0 q=[%atom p=%n]]
          q
        [ p=%gold
          q=[%cube p=0 q=[%atom p=%n]]
          r=[p=[1 20] q={[p=%$ q=[%ash p=[%dtzy p=%ud q=20]]]}]
        ]
      ]
      q=[[1 20] 0]
    ]

In these slightly more complex cases we see a cell and a core expanded
to show their type information.
