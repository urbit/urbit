cenlus, `%+`, %cnls
============================

Slam, two arguments

`%+` is a synthetic rune that that [pull]()s [`$`] from the [door]() `p`
with its sample set to `[%cntr q r]`. `%+` in the most common case
simply [slam]()s `p` with `q` and `r`, similar to a function call with
two arguments.

Produces
--------

Twig: `[%cnls p=twig q=twig r=twig]`

Sample
------

`p` is a [twig](), most commonly a [gate](). `q` and `r` are [twig]()s.

Tall form
---------

    %+  p
      q
    r

Wide form
---------

    %+(p q r)

Irregular form
--------------

None

Examples
--------

    /~zod/try=> =a  |=  [b=@ c=@]
        (add b c)
    new var %a
    /~zod/try=> %+  a
                  2
                1
    3
    /~zod/try=> %+(a 2 3)
    5

First we set a shell variable `a` to be a gate that takes two arguments
and produces their sum. Then we use `%+` to pass values to our gate.
`%+` is most useful for code organization, when you need to compute
intermediate products for your final computation.

Equivalent to
-------------

    %-(p [q r])
