`%+`
====

Call function w/two arguments

Calls the arm `$` from the [door]() `p` with its [sample]() set to `[q r]`.

Produces
--------

Twig: `[%cnls p=twig q=twig r=twig]`

Accepts
-------

`p` is a [`++twig`](), most commonly a function. The arguments `q` and `r` are
twigs as well

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

First we set a shell variable `a` to be a function that takes two arguments
and produces their sum. Then we use `%+` to pass values to our gate.
`%+` is most useful for code organization, when you need to compute
intermediate products for your final computation.
