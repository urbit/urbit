`?~`
====

If-null-then-else

If-then-else statement that tests whether `p` is null, producing
`q` if true and `r` if false.

Produces
--------

Twig: `[%wtsg p=wing q=twig r=twig]`

Accepts
-------

`p` is a [`++wing`](). `q` and `r` are [`++twig`]()s.

Tall form
---------

    ?~  p
      q
    r

Wide form
---------

    ?~(p q r)

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
