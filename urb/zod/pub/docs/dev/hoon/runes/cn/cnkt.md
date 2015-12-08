`%^`
====

Call function with three arguments.

`%^` is a synthetic rune that that [pull]()s [`$`]() from the [door]()
`p` with its sample set to `[%cntr q r s]`. `%^` in the most common case
simply [slam]()s `p` with `q`, `r` and `s`, similar to a function call
with three arguments.

Produces
--------

Twig: `[%cnkt p=twig q=twig r=twig s=twig]`

Accepts
-------

`p` is a [twig](), most commonly a function. The arguments `q`, `r` and `s` are
[twig]()s.

Tall form
---------

    %^    p
        q
      r
    s

Wide form
---------

    %^(p q r s)
