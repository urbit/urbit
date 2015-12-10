centar, `%*`, %cntr
============================

`%~` call arm from [door]() w/context changes

The same as `%~` but can make changes anywhere in the [context](), not just within the input data ([sample]()). Evaluates the expression `p` within core `q` with changes `r` to the context.

Produces
--------

Twig: `[%cntr p=wing q=twig r=tram]`

Accepts
-------

`p` is a [++wing](). `q` is a [++twig](). `r` is a [++tram]().

Tall form
---------

    %*  p  q
      p.i.r  q.i.r
      p.i.t.r  q.i.t.r
    ==

Wide form
---------

    %*(p q p.i.r q.i.r, p.i.t.r q.i.t.r)

Examples
--------

    /~zod/try=> =a  42
    new var %a
    /~zod/try=> =b  |%
        ++  mult  (mul 2 a)
        --
    new var %b
    /~zod/try=> mult.b
    84
    /~zod/try=> %*(mult b a 2)
    4

Here we add a variable `a` to our context, and a door `b` with a wing
`++mult`that depends on `a`. Calling `++mult` produces `84`, and we can
use `%*` to replace `a` in the context of `mult.b` with `2` to simply
produce `4`.
