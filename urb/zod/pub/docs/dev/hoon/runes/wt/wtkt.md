`?^`
====

If `p` is cell-then-else

Tests if `p` is a cell.  If so, produce `q`.  Else, produce `r`.

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

    ~zod/try=> ?^  ""
                 %full
               %empty
    %empty
    ~zod/try=> ?^  "asd"
                 %full
               %empty
    %full

Here we show that `""` is an atom while `"asd"` is a cell.

    ~zod/try=> ?^  `(unit)`~
                 %full
               %empty
    %empty
    ~zod/try=> ?^  `(unit)`[~ u=20]
                 %full
               %empty
    %full

Similarly, `~` is an atom and `[~ u=20]` is a cell.

Equivalent to
-------------

    ?:(?=(^ p) q r)
