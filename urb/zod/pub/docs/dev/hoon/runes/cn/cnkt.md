`%^`
====

Call function w/three arguments

Calls the arm `$` from the [door]() `p` with its [sample]() set
to `[q r s]`.  `%^` in the most common case simply calls `p` with
`q`, `r`and `s` as its arguments.

Produces
--------

Twig: `[%cnkt p=twig q=twig r=twig s=twig]`

Accepts
-------

`p` is a [`++twig`](), most commonly a function. The arguments
`q`, `r` and `s` are twigs as well.

Tall form
---------

    %^    p
        q
      r
    s

Wide form
---------

    %^(p q r s)
