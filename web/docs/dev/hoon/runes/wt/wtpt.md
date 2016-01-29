wutpat, `?@`, %wtpt
============================

If atom

`?@` is a synthetic rune that produces `q` if `p` is an atom, `r`
otherwise.

Produces
--------

Twig: `[%wtpt p=wing q=twig r=twig]`

Sample
------

`p` is a [`++wing`](). `q` and `r` are [twig]()s.

Tall form
---------

Kingside:

    ?@  p
      q
    r

Wide form
---------

    ?@(p q r)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> ?@(~ 1 2)
    ! mint-vain
    ! exit
    ~zod/try=> ?@(%ha 1 2)
    1
    ~zod/try=> ?@("" 1 2)
    1
    ~zod/try=> ?@("a" 1 2)
    2
    ~zod/try=> ?@([1 1] 1 2)
    ! mint-vain
    ! exit
    ~zod/try=> ?@(`*`[1 1] 1 2)
    2 

Equivalent to
-------------

    ?:(?=(@ p) q r)
