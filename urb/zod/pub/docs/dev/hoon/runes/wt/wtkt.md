`?^`
====

If `p` is cell-then-else

Produces
--------

Twig: `[%wtkt p=wing q=twig r=twig]`

Accepts
-------

`p` is a [`++wing`](). `q` and `r` are [`++twig`]()s.

Tall form
---------

    ?^  p
      q
    r

Wide form
---------

    ?^(p q r)

Examples
--------

    ~zod/try=> =(*@tas "")
    %.y
    ~zod/try=> ?^  ""
                 %full
               %empty
    %empty
    ~zod/try=> ?^  "asd"
                 %full
               %empty
    %full

Here we show that `*@tas`, the [bunt]() of `@tas` is equivalent to the empty
[`++tape`]() `""`, then use it in two `?^` cases.

    ~zod/try=> *(unit)
    ~
    ~zod/try=> ?^  `(unit)`~
                 %full
               %empty
    %empty
    ~zod/try=> ?^  `(unit)`[~ u=20]
                 %full
               %empty
    %full

Similar to the above case, we show the bunt of a [`++unit`](), which is
`~`, and test against it.

Equivalent to
-------------

    ?:(?=(^ p) q r)
