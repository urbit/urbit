`:+`
====

Construct a tuple of three elements.


`:+` is a synthetic rune that produces a cell `[p q r]`.

Produces
--------

Twig: `[%clls p=twig q=twig r=twig]`

Accepts
-------

`p` is a [twig](). `q` is a [twig](). `r` is a [twig]().

Tall form
---------

Kingside:

    :+  p
      q
    r

Queenside:

    :+  p  q
    r

Wide form
---------

    :+(p q r)

Examples
--------

    /~zod/try=> :+  1
                  2
                3
    [1 2 3]
    /~zod/try=> :+(%a ~ 'b')
    [%a ~ 'b']

This is the most straightforward case of `:+`, producing a tuple of four
values in both tall and wide form.

    /~zod/try=> 
    :+  (add 2 4)  (add 2 6)
      |-  (div 4 2)
    [6 8 2]

Most commonly `:+` helps to organize code, allowing you to produce a
cell from nested computation.
