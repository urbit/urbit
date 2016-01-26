colket, `:^`, %clkt
============================

Tuple of four

`:^` is a synthetic rune that produces a cell `[p q r s]`.

Produces
--------

Twig: `[%clkt p=twig q=twig r=twig s=twig]`

Sample
------

`p` is a [twig](). `q` is a [twig](). `r` is a [twig](). `s` is a
[twig]().

Tall form
---------

Kingside:

    :^    p
        q
      r
    s

Queenside:

    :^  p  q
      r
    s

    :^  p  q  r  
    s

Wide form
---------

    :^(p q r s)

Irregular form
--------------

undefined

Examples
--------

    /~zod/try=> :^(1 2 3 4)
    [1 2 3 4]
    /~zod/try=> :^  5  6
                  7
                8
    [5 6 7 8]

This is the most straightforward case of `:^`, producing a tuple of four
values in both tall and wide form.

    /~zod/try=> 
    :^  (add 2 4)  (add 2 6)
      |-  (div 4 2)
      ~
    [6 8 2 ~]

Most commonly `:^` helps to organize code, allowing you to produce a
cell from nested computation.
