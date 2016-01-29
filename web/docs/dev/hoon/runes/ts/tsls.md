tislus, `=+`, %tsls
============================

Push on

`=+` is a synthetic rune that pushes `p` on the subject and sends it to
`q`. `=+` is the inverse of `=-`. Use `=+` when your `p` isn't too long
and `=+` makes for more readable code.

See also
--------

tishep, `=-`, %tshp
============================

Produces
--------

Twig: `[%tsls p=twig q=twig]`

Sample
------

`p` and `q` are [twig]()s.

Tall form
---------

    =+  p
        q

Wide form
---------

    =+(p q)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> 
        =+  a=1
        a
    1

The simplest case of a `=+`, we push `a=1` on to our subject, and
produce `a`.

    ~zod/try=> 
    =cor  |=  a=@
          =+  b=1
          =+  c=2
          :(add a b c)
    new var %cor
    ~zod/try=> 
    (cor 0)
    3

This is a common case of `=+`, when we need to add intermediate values
to our subject to divide up our computation. `=+` makes for procedural,
top to bottom code organization.
