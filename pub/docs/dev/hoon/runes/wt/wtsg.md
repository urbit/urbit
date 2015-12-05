wutsig, `?~`, %wtsg
============================

If `~`

`?~` is a synthetic rune that produces `q` if `p` is `~`, `r` otherwise.

Produces
--------

Twig: `[%wtsg p=wing q=twig r=twig]`

Sample
------

`p` is a [`++wing`](). `q` and `r` are [twig]()s.

Tall form
---------

    ?~  p
      q
    r

Wide form
---------

    ?~(p q r)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> ?~('a' 1 2)
    2
    ~zod/try=> ?~('' 1 2)
    1
    ~zod/try=> ?~(~zod 1 2)
    1
    ~zod/try=> ?~((sub 20 20) 1 2)
    1

Equivalent to
-------------

    ?:(?=(~ p) q r)
