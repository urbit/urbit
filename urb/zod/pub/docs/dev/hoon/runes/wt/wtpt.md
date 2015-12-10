`?@`
====

If atom-then-else

If-then-else statement that tests whether `p` is an atom, producing `q` if true and `r` if false.

Produces
--------

Twig: `[%wtpt p=wing q=twig r=twig]`

Accepts
-------

`p` is a [`++wing`](). `q` and `r` are [++twig]()s.

Tall form
---------

Kingside:

    ?@  p
      q
    r

Wide form
---------

    ?@(p q r)

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
