`:^`
====

Quadruple. Tuple of four.

Produces
--------

Twig: `[%clkt p=twig q=twig r=twig s=twig]`

Accepts
-------

`p` is a [++twig](). `q` is a twig. `r` is a twig. `s` is a
twig.

Tall form
---------

    :^    p
        q
      r
    s

    :^  p  q
      r
    s

    :^  p  q  r  
    s

Wide form
---------

    :^(p q r s)

Examples
--------

    /~zod/try=> :^(1 2 3 4)
    [1 2 3 4]
    /~zod/try=> :^  5  6
                  7
                8
    [5 6 7 8]

These are the most straightforward cases of `:^`, producing a tuple of four
values in both tall and wide forms respectively.

    /~zod/try=> 
    :^  (add 2 4)  (add 2 6)
      |-  (div 4 2)
      ~
    [6 8 2 ~]

Most commonly `:^` helps to organize code, allowing you to produce a
cell from nested computation.
